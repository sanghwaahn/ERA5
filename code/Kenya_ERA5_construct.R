package_tmp <- c(
  "readxl", "openxlsx", "writexl", "fst", "ncdf4",
  "dplyr", "data.table", "stringr", "tidyr", "purrr", "furrr", "zoo", "lubridate",
  "RColorBrewer", "viridis",
  "sf", "sp", "raster", "exactextractr", "rnaturalearth", "rnaturalearthdata", "rnaturalearthhires",
  "here", "progressr", "future.apply", "crsuggest", "signal",
  "tidyverse"
)

packages <- unique(package_tmp)
lapply(packages, library, character.only = TRUE)
'%!in%' <- function(x,y)!('%in%'(x,y))
#dplyr::filter dplyr::select specified 



user_path <- "/Users/sanghwa/Library/CloudStorage/Box-Box/" 

main_dir <- here(paste0(user_path, "Kenya_land_rental"))
raw_data_dir <- file.path(main_dir, "data", "raw", "era5")
geo_data_dir <- file.path(main_dir, "data", "raw", "geospatial")
temp_data_dir <- file.path(main_dir, "data", "temp")
clean_data_dir <- file.path(main_dir, "data", "clean")
figure_data_dir <- file.path(main_dir, "figure", "check")
farmer_location_dir <-file.path(main_dir, "data", "raw", "farmer_subcounty")



##### Process Precipitation Data #####
start_year_precip <- 1940
end_year_precip <- 2025
years <- start_year_precip:end_year_precip
kenya_tp_list <- list()

for (year in years) {
  file_path <- file.path(raw_data_dir, paste0("era5_kenya_total_precipitation_", year, ".nc"))
  
  if (file.exists(file_path)) {
    nc_data <- nc_open(file_path)
    
    valid_time_vals <- ncvar_get(nc_data, "valid_time")  
    valid_time_units <- ncatt_get(nc_data, "valid_time", "units")$value  
    lon_vals <- ncvar_get(nc_data, "longitude")
    lon_units <- ncatt_get(nc_data, "longitude", "units")$value
    lat_vals <- ncvar_get(nc_data, "latitude")
    lat_units <- ncatt_get(nc_data, "latitude", "units")$value
    
    total_precipitation <- ncvar_get(nc_data, "tp") 
    
    df <- expand.grid(lon = lon_vals, lat = lat_vals, valid_time = valid_time_vals)
    df$total_precipitation <- as.vector(total_precipitation)
    df$year <- year
    
    df$base_date <- as.Date(ifelse(df$year == 1940, 
                                   paste0(df$year, "-01-02"), 
                                   paste0(df$year, "-01-01")))
    df$date <- df$base_date + df$valid_time    
    
    yearly_sum_tp <- df %>%
      group_by(lat, lon) %>%
      summarise(yearly_sum_tp = sum(total_precipitation, na.rm = TRUE), .groups = "drop")
    
    df$month <- as.integer(format(df$date, "%m"))
    
    monthly_tp <- df %>%
      group_by(lat, lon, month) %>%
      summarise(monthly_sum_tp = sum(total_precipitation, na.rm = TRUE), .groups = "drop") %>%
      mutate(month_label = paste0("monthly_sum_tp_m", month)) %>%
      dplyr::select(-month) %>%
      pivot_wider(names_from = month_label, values_from = monthly_sum_tp)    
    
    
    results <- monthly_tp %>% 
      left_join(yearly_sum_tp, by = c("lat", "lon"))
    
    results$year <- year
    
    
    # df <- df %>%
    #   left_join(yearly_sum_tp, by = c("lat", "lon")) %>%
    #   left_join(monthly_tp, by = c("lat", "lon"))
    
    # interim_output_file_onset <- file.path(temp_data_dir, paste0("grid_precipitation_", year, ".csv"))
    # write.csv(df, interim_output_file_onset, row.names = FALSE)
    
    kenya_tp_list[[as.character(year)]] <- results
    
    interim_output_file_onset_fst <- file.path(temp_data_dir, paste0("grid_precipitation_", year, ".fst"))
    write_fst(results, interim_output_file_onset_fst)
    
    africa_map <- ne_countries(scale = "medium", continent = "Africa", returnclass = "sf")
    kenya_map <- ne_countries(scale = "medium", country = "Kenya", returnclass = "sf")
    
    yearly_sum_tp$yearly_sum_tp_mm <- yearly_sum_tp$yearly_sum_tp * 1000
    breaks_mm <- c(0, 200, 400, 600, 800, 1200, 1600, 2000, 4000, Inf)
    labels_mm <- c("0–200", "200–400", "400–600", "600–800", "800–1200",
                   "1200–1600", "1600–2000", "2000–4000", "Over 4000")
    
    yearly_sum_tp$tp_bin <- cut(
      yearly_sum_tp$yearly_sum_tp_mm,
      breaks = breaks_mm,
      labels = labels_mm,
      include.lowest = TRUE,
      right = FALSE
    )        
    
    blue_colors <- brewer.pal(9, "Blues")
    # blue_colors <- rev(brewer.pal(8, "Blues"))
    
    p_tp <- ggplot(yearly_sum_tp, aes(x = lon, y = lat, fill = tp_bin)) +
      geom_tile() +
      geom_sf(data = africa_map, fill = NA, color = "gray70", linewidth = 0.4, inherit.aes = FALSE) +
      geom_sf(data = kenya_map, fill = NA, color = "black", linewidth = 0.5, inherit.aes = FALSE) +
      scale_fill_manual(
        values = blue_colors,
        name = "Yearly Precip. (mm)",
        drop = FALSE,
        guide = guide_legend(reverse = TRUE)  
      ) +
      coord_sf(xlim = c(31, 44), ylim = c(-7, 6), expand = FALSE) +
      theme_bw(base_size = 14) +
      theme(
        legend.position = "right",
        legend.title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size = 11),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 13, face = "bold")
      ) +
      labs(
        title = paste("Kenya - Yearly Total Precipitation (mm) -", year),
        x = "Longitude",
        y = "Latitude"
      )
    
    ggsave(file.path(figure_data_dir, paste0("precip_yearly_sum_", year, ".png")), p_tp, width = 7, height = 7)
    
    nc_close(nc_data)
    
    rm(df, monthly_tp, yearly_sum_tp, total_precipitation,
       valid_time_vals, valid_time_units, lon_vals, lon_units,
       lat_vals, lat_units, nc_data)
    gc(verbose = FALSE)
    
    
  } else {
    message("File not found: ", file_path)
  }
}

kenya_tp_all <- bind_rows(kenya_tp_list)

output_file_precip <- file.path(clean_data_dir, paste0("grid_total_precipitation_", start_year_precip, "_", end_year_precip, ".csv"))
write.csv(kenya_tp_all, output_file_precip, row.names = FALSE)
message("Saved combined grid-level precipitation stats data to: ", output_file_precip)
# output_file_precip_fst <- file.path(clean_data_dir, paste0("grid_total_precipitation_", start_year_precip, "_", end_year_precip, ".fst"))
# write_fst(kenya_tp_all, output_file_precip_fst)


df_check <- kenya_tp_all %>% dplyr::filter(year==1945)

target_counties <- c("BUNGOMA", "KAKAMEGA", "MIGORI", "SIAYA")
# target_counties <- c("NYAGONDO")

villages_filtered <- kenya_villages %>%
  dplyr::filter(str_to_upper(NAME) == NAME & NAME %in% target_counties)



##### Check with Maps #####

start_year_precip_draw <- 1940
end_year_precip_draw <- 2024
precip_data <- read_csv(output_file_precip) %>% dplyr::filter(year >= start_year_precip_draw & year <= end_year_precip_draw)
# precip_data <- read_fst(output_file_precip_fst) %>% dplyr::filter(year >= start_year_precip_draw & year <= end_year_precip_draw)


africa_map <- ne_countries(scale = "medium", continent = "Africa", returnclass = "sf")
# kenya_map <- ne_countries(scale = "medium", country = "Kenya", returnclass = "sf")
kenya_map_path <- file.path(geo_data_dir, "humdata", "ken_admbnda_adm0_iebc_20191031.shp")
kenya_map <- st_read(kenya_map_path)

kenya_villages_path <- file.path(geo_data_dir, "kenya_villages.shp")
kenya_villages <- st_read(kenya_villages_path)
st_crs(kenya_map)
st_crs(kenya_villages)


# kenya_counties_path <- file.path(geo_data_dir, "kenya_County_level_1.shp")
kenya_counties_path <- file.path(geo_data_dir, "humdata", "ken_admbnda_adm1_iebc_20191031.shp")
kenya_counties <- st_read(kenya_counties_path)
st_crs(kenya_counties)
#kenya_counties <- st_transform(kenya_counties, st_crs(kenya_map))

kenya_subcounties_path <- file.path(geo_data_dir, "humdata", "ken_admbnda_adm2_iebc_20191031.shp")
kenya_subcounties <- st_read(kenya_subcounties_path)
st_crs(kenya_subcounties)


location_file_path <- file.path(farmer_location_dir, "fin_counties_subcounties.dta")
location_df <- read_dta(location_file_path)

fin_counts <- location_df %>%
  group_by(county_ID, county, sub_county) %>%
  summarise(n_farmers = n_distinct(fin), .groups = "drop")

kenya_subcounties_clean <- kenya_subcounties %>%
  mutate(ADM1_EN = str_to_title(ADM1_EN),
         ADM2_EN = str_to_title(ADM2_EN))

kenya_subcounties_clean_filter <- kenya_subcounties_clean %>% dplyr::filter(ADM1_EN == "Bungoma")
# Bungoma South -> Kanduyi , Webuye -> Webuye East & Webuye West

fin_counts_clean <- fin_counts %>%
  mutate(
    county = str_to_title(county),
    sub_county = str_to_title(sub_county),
    # Prepare for matching
    join_key = case_when(
      sub_county == "Bungoma South" ~ "Kanduyi",
      sub_county == "Webuye" ~ "Webuye",  # handle later
      TRUE ~ sub_county
    )
  )


# Regular one-to-one join first
joined_normal <- kenya_subcounties_clean %>%
  inner_join(
    fin_counts_clean %>% dplyr::filter(join_key != "Webuye"),
    by = c("ADM1_EN" = "county", "ADM2_EN" = "join_key")
  ) %>%
  mutate(multiple_match = 0)

# Handle special case for "Webuye" joining to both "Webuye East" and "Webuye West"
webuye_matches <- kenya_subcounties_clean %>%
  dplyr::filter(ADM2_EN %in% c("Webuye East", "Webuye West")) %>%
  left_join(
    fin_counts_clean %>% dplyr::filter(sub_county == "Webuye"),
    by = c("ADM1_EN" = "county")
  ) %>%
  mutate(multiple_match = 1)

# Combine 
kenya_subcounties_merged <- bind_rows(joined_normal, webuye_matches) %>% dplyr::select(-join_key)


##### County-level summary stat #####
target_counties <- c("Bungoma", "Kakamega", "Migori", "Siaya")
highlighted_counties <- kenya_counties %>% dplyr::filter(ADM1_EN %in% target_counties)

villages_in_target <- st_join(kenya_villages, highlighted_counties, join = st_within, left = FALSE)
village_output_file <- file.path(clean_data_dir, "village_list.csv")
st_drop_geometry(villages_in_target) %>% write_csv(village_output_file)
village_counts <- villages_in_target %>%
  st_drop_geometry() %>%
  count(ADM1_EN, name = "num_villages")
print(village_counts)


precip_data_grid <- precip_data %>%
  dplyr::mutate(
    yearly_sum_tp = yearly_sum_tp * 1000,
    dplyr::across(dplyr::starts_with("monthly_sum_tp_m"), ~ .x * 1000)
  )

grid_polygons <- precip_data_grid %>%
  distinct(lat, lon) %>%
  mutate(
    geometry = purrr::pmap(list(lon, lat), function(x, y) {
      st_polygon(list(matrix(c(
        x - 0.125, y - 0.125,
        x + 0.125, y - 0.125,
        x + 0.125, y + 0.125,
        x - 0.125, y + 0.125,
        x - 0.125, y - 0.125
      ), ncol = 2, byrow = TRUE)))
    })
  ) %>%
  st_sf(crs = 4326)

grid_polygons <- grid_polygons %>%
  mutate(lat = as.numeric(lat), lon = as.numeric(lon))

kenya_crs <- 21037  # UTM Zone 37S (Kenya)

grid_polygons_area <- grid_polygons %>%
  st_transform(crs = kenya_crs) %>%
  mutate(pixel_area = st_area(geometry))

highlighted_counties_proj <- highlighted_counties %>%
  st_transform(crs = kenya_crs)

# Intersect with county polygons
grid_intersections_all <- st_intersection(grid_polygons_area, highlighted_counties_proj) %>%
  mutate(intersection_area = st_area(geometry))

# Compute area and assign county with largest intersection
grid_intersections_main <- grid_intersections_all %>%
  group_by(lon, lat) %>%
  slice_max(order_by = intersection_area, n = 1, with_ties = FALSE) %>%
  ungroup()

precip_data_grid_tagged <- precip_data_grid %>%
  inner_join(grid_intersections_main %>% st_drop_geometry() %>% dplyr::select(lon, lat, ADM1_EN), by = c("lon", "lat"))


county_colors <- c("Bungoma" = "red", "Kakamega" = "yellow", "Siaya" = "green", "Migori" = "blue")
p_county_grids <- ggplot() +
  geom_tile(data = precip_data_grid_tagged, aes(x = lon, y = lat, fill = ADM1_EN)) +
  scale_fill_manual(values = county_colors, name = "County") +
  geom_sf(data = highlighted_counties, fill = NA, color = "black", size = 0.6) +
  theme_minimal() +
  labs(title = "Grids Colored by Overlapping County",
       x = "Longitude", y = "Latitude")

ggsave(file.path(figure_data_dir, "grid_color_by_county.png"), p_county_grids, width = 8, height = 7)


# Area of full pixel (approx. in m^2, use st_area on projected CRS if needed)
# pixel_area <- st_area(grid_polygons[1, ])

county_weights <- grid_intersections_all %>%
  mutate(weight = as.numeric(intersection_area / pixel_area)) %>%
  dplyr::select(lon, lat, ADM1_EN, weight)


# check this part
precip_weighted <- precip_data_grid %>%
  inner_join(county_weights, by = c("lon", "lat"), relationship = "many-to-many") %>%
  dplyr::filter(ADM1_EN %in% target_counties)

# Weighted average by county-year
monthly_vars <- paste0("monthly_sum_tp_m", 1:12)
summary_vars <- c(monthly_vars, "yearly_sum_tp")

county_yearly_summary <- precip_weighted %>%
  group_by(ADM1_EN, year) %>%
  summarise(across(all_of(summary_vars), ~ weighted.mean(.x, w = weight, na.rm = TRUE)), .groups = "drop")

p_county_yearly <- ggplot(county_yearly_summary, aes(x = year, y = yearly_sum_tp, color = ADM1_EN)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = county_colors) +
  labs(title = "County-Level Yearly Precipitation (1940–2024)",
       x = "Year", y = "Total Precipitation (mm)", color = "County") +
  theme_minimal()

ggsave(file.path(figure_data_dir, "county_yearly_precip_1940_2024.png"), p_county_yearly, width = 10, height = 6)


county_monthly_long <- county_yearly_summary %>%
  dplyr::filter(year >= 2018) %>%
  pivot_longer(cols = all_of(monthly_vars), names_to = "month_str", values_to = "precip") %>%
  mutate(
    month = as.integer(stringr::str_remove(month_str, "monthly_sum_tp_m")),
    date = lubridate::ymd(paste(year, month, "01", sep = "-"))
  )

p_county_monthly <- ggplot(county_monthly_long, aes(x = date, y = precip, color = ADM1_EN)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = county_colors) +
  labs(title = "County-Level Monthly Precipitation (2018–2024)",
       x = "Date", y = "Monthly Precipitation (mm)", color = "County") +
  theme_minimal()

ggsave(file.path(figure_data_dir, "county_monthly_precip_2018_2024.png"), p_county_monthly, width = 10, height = 6)


county_precip_output <- file.path(clean_data_dir, "county_precipitation_summary.csv")
readr::write_csv(county_yearly_summary, county_precip_output)


##### Sub County-level summary stat and figure #####
highlighted_subcounties <- kenya_subcounties_merged %>%
  dplyr::filter(ADM1_EN %in% target_counties)

highlighted_subcounties_valid <- st_make_valid(highlighted_subcounties)

# Retry the spatial join
villages_in_subcounty <- st_join(
  kenya_villages, 
  highlighted_subcounties_valid, 
  join = st_within, 
  left = FALSE
)

# Save village-subcounty matches
village_subcounty_file <- file.path(clean_data_dir, "village_list_by_subcounty.csv")
st_drop_geometry(villages_in_subcounty) %>% write_csv(village_subcounty_file)

# Count villages by subcounty
village_counts_subcounty <- villages_in_subcounty %>%
  st_drop_geometry() %>%
  count(ADM1_EN, ADM2_EN, name = "num_villages")

print(village_counts_subcounty)


highlighted_subcounties_proj <- highlighted_subcounties_valid %>%
  st_transform(crs = kenya_crs)

# Intersect grid cells with subcounty polygons
grid_intersections_subcounty <- st_intersection(grid_polygons_area, highlighted_subcounties_proj) %>%
  mutate(intersection_area = st_area(geometry))

# For each grid cell, retain only the subcounty with the largest intersection
grid_main_subcounty <- grid_intersections_subcounty %>%
  group_by(lon, lat) %>%
  slice_max(order_by = intersection_area, n = 1, with_ties = FALSE) %>%
  ungroup()

# Tag each grid with the corresponding subcounty and county
precip_data_grid_tagged_subcounty <- precip_data_grid %>%
  inner_join(
    grid_main_subcounty %>% st_drop_geometry() %>% dplyr::select(lon, lat, ADM1_EN, ADM2_EN),
    by = c("lon", "lat")
  )

# Reuse the county color scheme
county_colors <- c("Bungoma" = "red", "Kakamega" = "yellow", "Siaya" = "green", "Migori" = "blue")
p_subcounty_grids <- ggplot() +
  geom_tile(data = precip_data_grid_tagged_subcounty, aes(x = lon, y = lat, fill = ADM1_EN)) +
  scale_fill_manual(values = county_colors, name = "County") +
  geom_sf_pattern(
    data = highlighted_subcounties,
    aes(),
    fill = NA,               # Transparent base
    pattern = 'stripe',      # Pattern type
    pattern_angle = 45,      # Diagonal lines
    pattern_density = 0.5,
    pattern_spacing = 0.02,
    pattern_fill = "gray",
    pattern_colour = "gray",
    color = NA               # No outline
  ) +
  geom_sf(data = highlighted_subcounties, fill = NA, color = "black", size = 0.4) +
  geom_sf(data = highlighted_counties, fill = NA, color = "black", linewidth = 1.2) +
  theme_minimal() +
  labs(
    title = "Grids Colored by County with Subcounty Boundaries",
    x = "Longitude", y = "Latitude"
  )

ggsave(file.path(figure_data_dir, "grid_color_by_subcounty.png"), p_subcounty_grids, width = 8, height = 7)


# Compute relative intersection weight for each grid within each sub-county
subcounty_weights <- grid_intersections_subcounty %>%
  mutate(weight = as.numeric(intersection_area / pixel_area)) %>%
  dplyr::select(lon, lat, ADM1_EN, ADM2_EN, weight)

# Merge sub-county weights with precipitation
precip_weighted_subcounty <- precip_data_grid %>%
  inner_join(subcounty_weights, by = c("lon", "lat"), relationship = "many-to-many") %>%
  dplyr::filter(ADM1_EN %in% target_counties)



subcounty_ordered <- subcounty_colors %>%
  arrange(factor(ADM1_EN, levels = c("Bungoma", "Kakamega", "Siaya", "Migori"))) %>%
  pull(ADM2_EN)

monthly_vars <- paste0("monthly_sum_tp_m", 1:12)
summary_vars <- c(monthly_vars, "yearly_sum_tp")

subcounty_yearly_summary <- precip_weighted_subcounty %>%
  group_by(ADM1_EN, ADM2_EN, year) %>%
  summarise(across(all_of(summary_vars), ~ weighted.mean(.x, w = weight, na.rm = TRUE)), .groups = "drop")

subcounty_yearly_summary <- subcounty_yearly_summary %>%
  mutate(ADM2_EN = factor(ADM2_EN, levels = subcounty_ordered))


# Define number of subcounties per target county
subcounty_names <- unique(subcounty_weights %>% dplyr::filter(ADM1_EN %in% target_counties) %>% pull(ADM2_EN))
subcounty_list <- subcounty_weights %>% 
  distinct(ADM1_EN, ADM2_EN) %>%
  dplyr::filter(ADM1_EN %in% target_counties)

# Generate color spectra
colors_bungoma <- brewer.pal(9, "Reds")[3:8]
colors_kakamega <- brewer.pal(9, "YlOrBr")[3:8]
colors_siaya <- brewer.pal(9, "Greens")[3:8]
colors_migori <- brewer.pal(9, "Blues")[3:8]

# Assign colors manually by county
set.seed(42)
subcounty_colors <- subcounty_list %>%
  group_by(ADM1_EN) %>%
  mutate(color = case_when(
    ADM1_EN == "Bungoma"  ~ colors_bungoma[seq_len(n())],
    ADM1_EN == "Kakamega" ~ colors_kakamega[seq_len(n())],
    ADM1_EN == "Siaya"    ~ colors_siaya[seq_len(n())],
    ADM1_EN == "Migori"   ~ colors_migori[seq_len(n())]
  )) %>%
  ungroup()

color_vec <- subcounty_colors %>%
  dplyr::select(ADM2_EN, color) %>%
  deframe()


p_subcounty_yearly <- ggplot(subcounty_yearly_summary, aes(x = year, y = yearly_sum_tp, color = ADM2_EN)) +
  geom_line(linewidth = 0.9) +
  scale_color_manual(values = color_vec, name = "Subcounty") +
  labs(
    title = "Subcounty-Level Yearly Precipitation (1940–2024)",
    x = "Year", y = "Total Precipitation (mm)",
    caption = "Subcounties from Bungoma use red spectrum, Kakamega yellow, Siaya green, and Migori blue."
  ) +
  theme_minimal()

ggsave(file.path(figure_data_dir, "subcounty_yearly_precip_1940_2024.png"), p_subcounty_yearly, width = 12, height = 8)


for (cty in target_counties) {
  county_data <- subcounty_yearly_summary %>% dplyr::filter(ADM1_EN == cty)
  county_colors <- color_vec[names(color_vec) %in% county_data$ADM2_EN]
  
  p_cty_yearly <- ggplot(county_data, aes(x = year, y = yearly_sum_tp, color = ADM2_EN)) +
    geom_line(linewidth = 0.9) +
    scale_color_manual(values = county_colors, name = "Subcounty") +
    labs(
      title = paste0(cty, " Subcounty-Level Yearly Precipitation (1940–2024)"),
      x = "Year", y = "Total Precipitation (mm)",
      caption = "Subcounties from Bungoma use red spectrum, Kakamega yellow, Siaya green, and Migori blue."
    ) +
    theme_minimal() +
    theme(plot.caption = element_text(hjust = 0, size = 9, face = "italic"))
  
  ggsave(file.path(figure_data_dir, paste0("subcounty_yearly_precip_", cty, ".png")), 
         p_cty_yearly, width = 10, height = 6)
}


subcounty_monthly_long <- subcounty_yearly_summary %>%
  dplyr::filter(year >= 2018) %>%
  pivot_longer(cols = all_of(monthly_vars), names_to = "month_str", values_to = "precip") %>%
  mutate(
    month = as.integer(str_remove(month_str, "monthly_sum_tp_m")),
    date = lubridate::ymd(paste(year, month, "01", sep = "-"))
  )

subcounty_monthly_long <- subcounty_monthly_long %>%
  mutate(ADM2_EN = factor(ADM2_EN, levels = subcounty_ordered))

p_subcounty_monthly <- ggplot(subcounty_monthly_long, aes(x = date, y = precip, color = ADM2_EN)) +
  geom_line(linewidth = 0.9) +
  scale_color_manual(values = color_vec, name = "Subcounty") +
  labs(
    title = "Subcounty-Level Monthly Precipitation (2018–2024)",
    x = "Date", y = "Monthly Precipitation (mm)",
    caption = "Subcounties from Bungoma use red spectrum, Kakamega yellow, Siaya green, and Migori blue."
  ) +
  theme_minimal()

ggsave(file.path(figure_data_dir, "subcounty_monthly_precip_2018_2024.png"), p_subcounty_monthly, width = 12, height = 8)


for (cty in target_counties) {
  county_monthly_data <- subcounty_monthly_long %>% dplyr::filter(ADM1_EN == cty)
  county_colors <- color_vec[names(color_vec) %in% county_monthly_data$ADM2_EN]
  
  p_cty_monthly <- ggplot(county_monthly_data, aes(x = date, y = precip, color = ADM2_EN)) +
    geom_line(linewidth = 0.9) +
    scale_color_manual(values = county_colors, name = "Subcounty") +
    labs(
      title = paste0(cty, " Subcounty-Level Monthly Precipitation (2018–2024)"),
      x = "Date", y = "Monthly Precipitation (mm)",
      caption = "Subcounties from Bungoma use red spectrum, Kakamega yellow, Siaya green, and Migori blue."
    ) +
    theme_minimal() +
    theme(plot.caption = element_text(hjust = 0, size = 9, face = "italic"))
  
  ggsave(file.path(figure_data_dir, paste0("subcounty_monthly_precip_", cty, ".png")), 
         p_cty_monthly, width = 10, height = 6)
}


subcounty_precip_output <- file.path(clean_data_dir, "subcounty_precipitation_summary.csv")
readr::write_csv(subcounty_yearly_summary, subcounty_precip_output)



##### Overall summary figure #####
mean_tp <- precip_data %>%
  group_by(lon, lat) %>%
  summarise(mean_yearly_tp = mean(yearly_sum_tp, na.rm = TRUE), .groups = "drop") %>%
  mutate(mean_yearly_tp_mm = mean_yearly_tp * 1000)

breaks_mm <- c(0, 200, 400, 600, 800, 1200, 1600, 2000, 4000, Inf)
labels_mm <- c("0–200", "200–400", "400–600", "600–800", "800–1200",
               "1200–1600", "1600–2000", "2000–4000", "Over 4000")

mean_tp$tp_bin <- cut(
  mean_tp$mean_yearly_tp_mm,
  breaks = breaks_mm,
  labels = labels_mm,
  include.lowest = TRUE,
  right = FALSE
)

mean_tp$tp_bin <- factor(mean_tp$tp_bin, levels = labels_mm, ordered = TRUE)

blue_colors <- brewer.pal(9, "Blues")  # No reversal

p_tp_avg <- ggplot(mean_tp, aes(x = lon, y = lat, fill = tp_bin)) +
  geom_tile() +
  geom_sf(data = highlighted_counties, fill = NA, color = "green", linewidth = 0.9, inherit.aes = FALSE) +
  geom_sf(data = kenya_villages, color = "yellow", size = 0.1, alpha = 0.05, inherit.aes = FALSE) +
  geom_sf(data = africa_map, fill = NA, color = "gray70", linewidth = 0.4, inherit.aes = FALSE) +
  geom_sf(data = kenya_counties, fill = NA, color = "red", linewidth = 0.4, inherit.aes = FALSE) +
  geom_sf(data = kenya_map, fill = NA, color = "black", linewidth = 0.5, inherit.aes = FALSE) +
  scale_fill_manual(
    values = blue_colors,
    name = "Avg. Yearly Precip. (mm)",
    drop = FALSE,
    guide = guide_legend(reverse = TRUE)  # dark = top = high bin
  ) +
  coord_sf(xlim = c(31, 44), ylim = c(-7, 6), expand = FALSE) +
  theme_bw(base_size = 14) +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 13, face = "bold"),
    legend.text = element_text(size = 11),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 13, face = "bold")
  ) +
  labs(
    title = paste0("Kenya - Average Yearly Total Precipitation (mm), ", start_year_precip_draw, "–", end_year_precip_draw),
    x = "Longitude",
    y = "Latitude"
  )

avg_file <- file.path(figure_data_dir, paste0("precip_yearly_avg_", start_year_precip_draw, "_", end_year_precip_draw, ".png"))
ggsave(avg_file, p_tp_avg, width = 7, height = 7)


p_yearly_1980_2024 <- precip_data_grid_tagged %>%
  dplyr::filter(year >= 1980) %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(avg_precip = mean(yearly_sum_tp, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = avg_precip)) +
  geom_line(color = "steelblue", linewidth = 1) +
  labs(title = "Avg Yearly Precipitation (1980–2024) — Target Grids",
       x = "Year", y = "Precipitation (mm)") +
  theme_minimal()

ggsave(filename = file.path(figure_data_dir, "target_yearly_precip_1980_2024.png"),
       plot = p_yearly_1980_2024, width = 8, height = 5, dpi = 300)

p_yearly_1940_2024 <- precip_data_grid_tagged %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(avg_precip = mean(yearly_sum_tp, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = avg_precip)) +
  geom_line(color = "darkgreen", linewidth = 1) +
  labs(title = "Avg Yearly Precipitation (1940–2024) — Target Grids",
       x = "Year", y = "Precipitation (mm)") +
  theme_minimal()

ggsave(filename = file.path(figure_data_dir, "target_yearly_precip_1940_2024.png"),
       plot = p_yearly_1940_2024, width = 8, height = 5, dpi = 300)



monthly_data <- precip_data_grid_tagged %>%
  dplyr::filter(year >= 2018) %>%
  dplyr::mutate(coord_label = paste0("(", lat, ", ", lon, ")"))

monthly_long <- monthly_data %>%
  tidyr::pivot_longer(cols = dplyr::starts_with("monthly_sum_tp_m"),
                      names_to = "month_str",
                      values_to = "precip") %>%
  dplyr::mutate(
    month = as.integer(stringr::str_remove(month_str, "monthly_sum_tp_m")),
    date = lubridate::ymd(paste(year, month, "01", sep = "-"))
  )

p_monthly_2018_2024 <- ggplot(monthly_long, aes(x = date, y = precip, color = coord_label)) +
  geom_line(alpha = 0.7, linewidth = 0.8) +
  labs(title = "Monthly Precipitation (2018–2024) — Target Grids",
       x = "Date", y = "Monthly Precipitation (mm)",
       color = "Grid (lat, lon)") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 7))

ggsave(filename = file.path(figure_data_dir, "target_grid_monthly_precip_2018_2024.png"),
       plot = p_monthly_2018_2024, width = 10, height = 6, dpi = 300)

yearly_grid <- precip_data_grid_tagged %>%
  dplyr::filter(year >= 1980) %>%
  dplyr::mutate(coord_label = paste0("(", lat, ", ", lon, ")"))

p_yearly_grid <- ggplot(yearly_grid, aes(x = year, y = yearly_sum_tp, color = coord_label)) +
  geom_line(linewidth = 0.8, alpha = 0.8) +
  labs(title = "Yearly Precipitation by Grid (1980–2024)",
       x = "Year", y = "Precipitation (mm)", color = "Grid (lat, lon)") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 7))

ggsave(filename = file.path(figure_data_dir, "target_grid_yearly_precip_1980_2024.png"),
       plot = p_yearly_grid, width = 10, height = 6, dpi = 300)


##### Grid-level Precipitation Stats Generation (2000-2024) #####
start_year_precip_raw <- 1940
end_year_precip_raw <- 2024
years <- start_year_precip_raw:end_year_precip_raw
kenya_tp_list_raw <- list()

for (year in years) {
  file_path <- file.path(raw_data_dir, paste0("era5_kenya_total_precipitation_", year, ".nc"))
  
  if (file.exists(file_path)) {
    nc_data <- nc_open(file_path)
    
    valid_time_vals <- ncvar_get(nc_data, "valid_time")  
    valid_time_units <- ncatt_get(nc_data, "valid_time", "units")$value  
    lon_vals <- ncvar_get(nc_data, "longitude")
    lon_units <- ncatt_get(nc_data, "longitude", "units")$value
    lat_vals <- ncvar_get(nc_data, "latitude")
    lat_units <- ncatt_get(nc_data, "latitude", "units")$value
    
    total_precipitation <- ncvar_get(nc_data, "tp") 
    
    df <- expand.grid(lon = lon_vals, lat = lat_vals, valid_time = valid_time_vals)
    df$total_precipitation <- as.vector(total_precipitation)
    df$year <- year
    
    df$base_date <- as.Date(ifelse(df$year == 1940, 
                                   paste0(df$year, "-01-02"), 
                                   paste0(df$year, "-01-01")))
    df$date <- df$base_date + df$valid_time
    
    df <- df %>% dplyr::select(-valid_time, -base_date)
    
    kenya_tp_list_raw[[as.character(year)]] <- df
    
    
  } else {
    message("File not found: ", file_path)
  }
}

kenya_tp_all_raw <- bind_rows(kenya_tp_list_raw)

# If you have enough memory (free from : vector memory limit of 16.0 Gb reached, see mem.maxVSize())
# output_file_precip_raw <- file.path(clean_data_dir, paste0("grid_precipitation_raw_", start_year_precip_raw, "_", end_year_precip_raw, ".csv"))
# write.csv(kenya_tp_all_raw, output_file_precip_raw, row.names = FALSE)
# message("Saved combined grid-level precipitation raw data data to: ", output_file_precip_raw)

# Define three year ranges
part1_years <- 1940:1973
part2_years <- 1974:2005
part3_years <- 2006:2025

# Split
kenya_tp_part1 <- kenya_tp_all_raw %>% filter(year %in% part1_years)
kenya_tp_part2 <- kenya_tp_all_raw %>% filter(year %in% part2_years)
kenya_tp_part3 <- kenya_tp_all_raw %>% filter(year %in% part3_years)

output_part1 <- file.path(temp_data_dir, "grid_precipitation_raw_part1_1940_1973.fst")
output_part2 <- file.path(temp_data_dir, "grid_precipitation_raw_part2_1974_2005.fst")
output_part3 <- file.path(temp_data_dir, "grid_precipitation_raw_part3_2006_2025.fst")
write_fst(kenya_tp_part1, output_part1)
write_fst(kenya_tp_part2, output_part2)
write_fst(kenya_tp_part3, output_part3)

message("Saved all three parts as fst files.")

# Load parts
kenya_tp_part1 <- read_fst(file.path(temp_data_dir, "grid_precipitation_raw_part1_1940_1973.fst"))
kenya_tp_part2 <- read_fst(file.path(temp_data_dir, "grid_precipitation_raw_part2_1974_2005.fst"))
kenya_tp_part3 <- read_fst(file.path(temp_data_dir, "grid_precipitation_raw_part3_2006_2025.fst"))


# Combine
kenya_tp_all_raw <- bind_rows(kenya_tp_part1, kenya_tp_part2, kenya_tp_part3)
rm(kenya_tp_part1, kenya_tp_part2, kenya_tp_part3)
#kenya_tp_all_raw <- read_csv(output_file_precip_raw)

kenya_tp_2000_2024 <- kenya_tp_all_raw %>%
  dplyr::filter(year >= 2000, year <= 2024)

# Create yearly total precipitation
yearly_sum_tp <- kenya_tp_2000_2024 %>%
  group_by(lat, lon, year) %>%
  summarise(yearly_sum_tp = sum(total_precipitation, na.rm = TRUE), .groups = "drop")

# Create monthly precipitation variables (wide format)
kenya_tp_2000_2024$month <- as.integer(format(kenya_tp_2000_2024$date, "%m"))

monthly_tp <- kenya_tp_2000_2024 %>%
  group_by(lat, lon, month) %>%
  summarise(monthly_sum_tp = sum(total_precipitation, na.rm = TRUE), .groups = "drop") %>%
  mutate(month_label = paste0("monthly_sum_tp_m", month)) %>%
  dplyr::select(-month) %>%
  pivot_wider(names_from = month_label, values_from = monthly_sum_tp)

grid_precip_stats_2000_2024 <- left_join(yearly_sum_tp, monthly_tp, by = c("lat", "lon"))
output_file_stats_2000_2024 <- file.path(clean_data_dir, "grid_precip_stats_2000_2024.csv")
write.csv(grid_precip_stats_2000_2024, output_file_stats_2000_2024, row.names = FALSE)
message("Saved 2000–2024 grid-level stats to: ", output_file_stats_2000_2024)

rm(kenya_tp_2000_2024)

# Define target years and periods
target_years <- 2018:2024
# reference_full <- 1940:2024
reference_recent <- 2000:2024
period_recent <- 2018:2021

# Assign season labels
daily_tp <- kenya_tp_all_raw %>%
  filter(year %in% c(reference_recent, target_years)) %>%
  mutate(
    date = as.Date(date),
    year = year(date),
    month = month(date),
    season = case_when(
      month %in% 3:7 ~ "long_rain",
      month %in% 10:12 ~ "short_rain",
      TRUE ~ "other"
    ),
    tp_mm = total_precipitation * 1000
  )

rm(kenya_tp_all_raw)


# Sub-annual (season-based: long_rain, short_rain, other)
sub_annual_precip <- daily_tp %>%
  filter(year %in% c(reference_recent, target_years)) %>%
  group_by(lon, lat, year, season) %>%
  summarise(total_tp_mm = sum(tp_mm, na.rm = TRUE), .groups = "drop")

annual_precip <- daily_tp %>%
  filter(year %in% c(reference_recent, target_years)) %>%
  group_by(lon, lat, year) %>%
  summarise(total_tp_mm = sum(tp_mm, na.rm = TRUE), .groups = "drop") %>%
  mutate(season = "annual")

precip_by_period <- bind_rows(sub_annual_precip, annual_precip)

mean_tp_recent <- precip_by_period %>%
  filter(year %in% reference_recent) %>%
  group_by(lon, lat, season) %>%
  summarise(mean_ref_recent = mean(total_tp_mm), .groups = "drop")

tp_with_dev <- precip_by_period %>%
  filter(year %in% target_years) %>%
  left_join(mean_tp_recent, by = c("lon", "lat", "season")) %>%
  mutate(dev_from_recent = total_tp_mm - mean_ref_recent)

tp_with_dev_wide <- tp_with_dev %>%
  pivot_wider(
    names_from = season,
    values_from = c(total_tp_mm, mean_ref_recent, dev_from_recent)
  )


# Make bins #
# Subset for bin computation using bounding box (Western Kenya)
precip_for_bin <- daily_tp %>%
  filter(
    year %in% period_recent,
    # lat >= -3, lat <= 3,
    # lon >= 32, lon <= 38
    lat >= -1.5, lat <= 1,
    lon >= 34, lon <= 35
  )

# Define fixed precipitation bin breaks
fixed_breaks <- c(-Inf, 15, 30, 60, Inf)
bin_labels <- paste0("bin_", 1:4)

# Create bin definition dataframe with same fixed breaks for all seasons
precip_bin_df <- tibble::tibble(
  bin = bin_labels,
  annual_lower      = fixed_breaks[-length(fixed_breaks)],
  annual_upper      = fixed_breaks[-1],
  long_rain_lower   = fixed_breaks[-length(fixed_breaks)],
  long_rain_upper   = fixed_breaks[-1],
  short_rain_lower  = fixed_breaks[-length(fixed_breaks)],
  short_rain_upper  = fixed_breaks[-1],
  other_lower       = fixed_breaks[-length(fixed_breaks)],
  other_upper       = fixed_breaks[-1]
)


bin_output_path <- file.path(clean_data_dir, "precip_bin_western_kenya_2018_2021_fixed.csv")
readr::write_csv(precip_bin_df, bin_output_path)
message("Saved bin breaks to: ", bin_output_path)


plot_and_save_histogram <- function(data, season_name, output_dir) {
  season_data <- if (season_name == "annual") {
    data
  } else {
    data %>% filter(season == season_name)
  }
  
  p <- ggplot(season_data, aes(x = tp_mm)) +
    geom_histogram(bins = 100, fill = "skyblue", color = "black") +
    labs(
      title = paste("Histogram of Precipitation -", season_name),
      x = "Daily Precipitation (mm)",
      y = "Frequency"
    ) +
    theme_minimal()
  
  output_file <- file.path(output_dir, paste0("histogram_tp_", season_name, ".png"))
  ggsave(output_file, plot = p, width = 8, height = 5, dpi = 300)
  message("Saved: ", output_file)
}

seasons <- c("annual", "long_rain", "short_rain", "other")

for (s in seasons) {
  plot_and_save_histogram(precip_for_bin, s, figure_data_dir)
}


daily_tp_binned <- daily_tp %>%
  mutate(
    annual_bin = as.character(cut(tp_mm, breaks = fixed_breaks, include.lowest = TRUE, labels = bin_labels)),
    season_bin = case_when(
      season == "long_rain"  ~ as.character(cut(tp_mm, breaks = fixed_breaks,  include.lowest = TRUE, labels = bin_labels)),
      season == "short_rain" ~ as.character(cut(tp_mm, breaks = fixed_breaks, include.lowest = TRUE, labels = bin_labels)),
      season == "other"      ~ as.character(cut(tp_mm, breaks = fixed_breaks,      include.lowest = TRUE, labels = bin_labels)),
      TRUE ~ NA_character_
    )
  )

check <- daily_tp_binned %>% filter(date == "2000-03-01")

annual_bin_counts <- daily_tp_binned %>%
  filter(year %in% target_years, !is.na(annual_bin)) %>%
  group_by(lon, lat, year, annual_bin) %>%
  summarise(day_count = n(), .groups = "drop") %>%
  mutate(var_name = paste0("annual_", annual_bin)) %>%
  dplyr::select(-annual_bin) %>%
  pivot_wider(names_from = var_name, values_from = day_count, values_fill = 0)

season_bin_counts <- daily_tp_binned %>%
  filter(year %in% target_years, !is.na(season_bin)) %>%
  group_by(lon, lat, year, season, season_bin) %>%
  summarise(day_count = n(), .groups = "drop") %>%
  mutate(var_name = paste0(season, "_", season_bin)) %>%
  dplyr::select(-season_bin, -season) %>%
  pivot_wider(names_from = var_name, values_from = day_count, values_fill = 0)

binned_counts_combined <- full_join(annual_bin_counts, season_bin_counts, by = c("lon", "lat", "year"))

# Add check columns for each category by summing over relevant columns
binned_counts_combined <- binned_counts_combined %>%
  rowwise() %>%
  mutate(
    annual_check     = sum(c_across(starts_with("annual_")),     na.rm = TRUE),
    long_rain_check  = sum(c_across(starts_with("long_rain_")),  na.rm = TRUE),
    short_rain_check = sum(c_across(starts_with("short_rain_")), na.rm = TRUE),
    other_check      = sum(c_across(starts_with("other_")),      na.rm = TRUE)
  ) %>%
  ungroup()

# Function to get ordered names: bin_1 to bin_5, then check
ordered_bin_names <- function(df_names, prefix) {
  bin_part <- paste0(prefix, "_bin_", 1:4)
  check_part <- paste0(prefix, "_check")
  intersect(bin_part, df_names) %>%    # keep only existing ones
    c(check_part)
}

# Apply to each group
all_names <- names(binned_counts_combined)

annual_cols     <- ordered_bin_names(all_names, "annual")
long_rain_cols  <- ordered_bin_names(all_names, "long_rain")
short_rain_cols <- ordered_bin_names(all_names, "short_rain")
other_cols      <- ordered_bin_names(all_names, "other")

# Final column order
ordered_cols <- c("lon", "lat", "year", annual_cols, long_rain_cols, short_rain_cols, other_cols)

# Reorder columns
binned_counts_combined <- binned_counts_combined[, ordered_cols]

precip_grid_stat_output <- tp_with_dev_wide %>%
  left_join(binned_counts_combined, by = c("lon", "lat", "year")) %>% 
  left_join(grid_precip_stats_2000_2024, by = c("lon", "lat", "year"))
precip_grid_stat_output_2018_2024 <- file.path(clean_data_dir, "final_precip_grid_stats_2018_2024.csv")
write.csv(precip_grid_stat_output, precip_grid_stat_output_2018_2024, row.names = FALSE)






###### Memory clear ######
ls()
data_objects <- ls(envir = .GlobalEnv)[
  sapply(ls(envir = .GlobalEnv), function(x) {
    obj <- get(x, envir = .GlobalEnv)
    is.data.frame(obj) || is.matrix(obj) || is.array(obj)
  })
]

rm(list = data_objects, envir = .GlobalEnv)
gc()




########################################
##### Process Temperature Data #####
########################################
start_year_temp <- 1940
end_year_temp <- 2025
temp_years <- start_year_temp:end_year_temp
temperature_list <- list()

leap_year <- function(year) {
  return((year %% 4 == 0 & year %% 100 != 0) | (year %% 400 == 0))
}

for (year in temp_years) {
  file_path <- file.path(raw_data_dir, paste0("era5_kenya_temperature_", year, ".nc"))
  
  if (file.exists(file_path)) {
    nc_data <- nc_open(file_path)
    
    valid_time_vals <- ncvar_get(nc_data, "valid_time")  
    valid_time_units <- ncatt_get(nc_data, "valid_time", "units")$value  
    lon_vals <- ncvar_get(nc_data, "longitude")
    lon_units <- ncatt_get(nc_data, "longitude", "units")$value
    lat_vals <- ncvar_get(nc_data, "latitude")
    lat_units <- ncatt_get(nc_data, "latitude", "units")$value
    
    temperature <- ncvar_get(nc_data, "t2m") 
    
    df <- expand.grid(lon = lon_vals, lat = lat_vals, valid_time = valid_time_vals)
    df$temperature <- as.vector(temperature)
    df$temp_celsius <- df$temperature - 273.15    
    df$year <- year
    
    df$base_date <- as.Date(ifelse(df$year == 1940, 
                                   paste0(df$year, "-01-02"), 
                                   paste0(df$year, "-01-01")))
    df$date <- df$base_date + df$valid_time   
    
    # df$lon_units <- lon_units
    # df$lat_units <- lat_units
    # df$valid_time_units <- valid_time_units    
    
    df$date <- as.Date(df$date)
    
    bin_levels <- c("below_0_C", "0_5_C", "5_10_C", "10_15_C", "15_20_C", 
                    "20_25_C", "25_30_C", "30_over_C")
    
    df$category <- cut(df$temp_celsius, 
                       breaks = c(-Inf, 0, 5, 10, 15, 20, 25, 30, Inf), 
                       labels = bin_levels,
                       ordered_result = TRUE)
    
    yearly_mean <- df %>%
      group_by(lat, lon) %>%
      summarise(yearly_mean = mean(temp_celsius))
    
    df$month <- format(df$date, "%Y-%m")
    
    monthly_mean <- df %>%
      group_by(lat, lon, month) %>%
      summarise(monthly_mean = mean(temp_celsius))
    
    monthly_mean_pivot <- monthly_mean %>%
      pivot_wider(names_from = "month", values_from = "monthly_mean")
    
    month_cols <- if (year == 2025) paste0("m", 1:4) else paste0("m", 1:12)
    names(monthly_mean_pivot)[3:ncol(monthly_mean_pivot)] <- month_cols
    
    
    full_year_grid <- expand.grid(
      lat = unique(df$lat),
      lon = unique(df$lon),
      category = factor(bin_levels, levels = bin_levels, ordered = TRUE)
    )
    
    yearly_counts <- df %>%
      group_by(lat, lon, category) %>%
      summarise(count = n(), .groups = "drop")
    
    category_counts_pivot <- full_year_grid %>%
      left_join(yearly_counts, by = c("lat", "lon", "category")) %>%
      mutate(count = replace_na(count, 0)) %>%
      pivot_wider(names_from = category, values_from = count)    
    
    category_columns <- names(category_counts_pivot)[3:ncol(category_counts_pivot)]
    category_counts_pivot$sum_dates <- rowSums(category_counts_pivot[, category_columns])
    
    # Monthly category counts with full grid
    df$month_num <- as.integer(format(df$date, "%m"))
    month_range <- if (year == 2025) 1:4 else 1:12      
    
    full_month_grid <- expand.grid(
      lat = unique(df$lat),
      lon = unique(df$lon),
      month_num = month_range,
      category = factor(bin_levels, levels = bin_levels, ordered = TRUE)
    )
    
    monthly_category_counts <- df %>%
      group_by(lat, lon, month_num, category) %>%
      summarise(count = n(), .groups = "drop")
    
    monthly_category_counts_full <- full_month_grid %>%
      left_join(monthly_category_counts, by = c("lat", "lon", "month_num", "category")) %>%
      mutate(count = replace_na(count, 0)) %>%
      mutate(category = paste0("t_m", month_num, "_", category))
    
    monthly_category_counts_wide <- monthly_category_counts_full %>%
      dplyr::select(-month_num) %>%
      pivot_wider(names_from = category, values_from = count)
    
    combined_data <- yearly_mean %>%
      left_join(monthly_mean_pivot, by = c("lat", "lon")) %>%
      left_join(category_counts_pivot, by = c("lat", "lon")) %>%
      left_join(monthly_category_counts_wide, by = c("lat", "lon"))
    
    
    combined_data$year <- year
    
    max_days <- if (leap_year(year)) 366 else 365
    combined_data$check <- ifelse(combined_data$sum_dates == max_days, 0, 1)
    
    bin_order <- bin_levels
    if (year == 2025 && "30_over_C" %!in% names(combined_data)) {
      bin_order <- setdiff(bin_order, "30_over_C")
    }
    t_month_category_cols <- unlist(lapply(month_range, function(m) {
      paste0("t_m", m, "_", bin_order)
    }))
    t_month_category_cols <- t_month_category_cols[t_month_category_cols %in% names(combined_data)]
    
    final_columns_order <- c("year", "lat", "lon", "yearly_mean", month_cols, 
                             bin_levels[bin_levels %in% names(combined_data)], 
                             "sum_dates", "check", t_month_category_cols)
    
    combined_data <- combined_data[, final_columns_order]
    
    combined_data <- combined_data %>%
      rename_with(~ paste0("t_", .), c("below_0_C", "0_5_C", "5_10_C", "10_15_C", "15_20_C", 
                                       "20_25_C", "25_30_C", "30_over_C"))
    
    
    combined_file <- file.path(temp_data_dir, paste0("grid_temp_stat_", year, ".csv"))
    write.csv(combined_data, combined_file, row.names = FALSE)
    
    check_count <- sum(combined_data$check == 1)
    message(paste0("Number of grids with incomplete data for year ", year, ": ", check_count))
    
    temperature_list[[as.character(year)]] <- combined_data # list(original = df, combined = combined_data)
    
    df_sf <- st_as_sf(combined_data, coords = c("lon", "lat"), crs = 4326)
    africa_map <- ne_countries(scale = "medium", continent = "Africa", returnclass = "sf")
    kenya_map <- ne_countries(scale = "medium", country = "Kenya", returnclass = "sf")
    
    
    
    p1 <- ggplot(combined_data, aes(x = lon, y = lat, fill = yearly_mean)) +
      geom_tile() +
      geom_sf(data = africa_map, fill = NA, color = "gray70", linewidth = 0.4, inherit.aes = FALSE) +
      geom_sf(data = kenya_map, fill = NA, color = "black", linewidth = 0.5, inherit.aes = FALSE) +
      scale_fill_gradientn(colors = c("blue", "white", "yellow", "red"),
                           values = scales::rescale(c(-10, 5, 20, 35)),
                           name = "Yearly Mean Temp (°C)") +
      coord_sf(xlim = c(31, 44), ylim = c(-7, 6), expand = FALSE) +
      theme_bw(base_size = 14) +
      theme(
        legend.position = "right",
        legend.title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size = 11),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA)
      ) +
      labs(title = paste("Kenya - Yearly Mean Temperature -", year),
           x = "Longitude",
           y = "Latitude")
    
    ggsave(file.path(figure_data_dir, paste0("temp_yearly_mean_", year, ".png")), p1)
    
    if (year != 2025 && "t_30_over_C" %in% names(combined_data)) {
      p2 <- ggplot(combined_data, aes(x = lon, y = lat, fill = `t_30_over_C`)) +
        geom_tile() +
        geom_sf(data = africa_map, fill = NA, color = "gray70", linewidth = 0.4, inherit.aes = FALSE) +
        geom_sf(data = kenya_map, fill = NA, color = "black", linewidth = 0.5, inherit.aes = FALSE) +
        scale_fill_gradient2(low = "green", mid = "yellow", high = "red", midpoint = 28,
                             name = "Days in 30°C+ Bin") +
        coord_sf(xlim = c(31, 44), ylim = c(-7, 6), expand = FALSE) +
        theme_bw(base_size = 14) +
        theme(
          legend.position = "right",
          legend.title = element_text(size = 13, face = "bold"),
          legend.text = element_text(size = 11),
          panel.grid = element_blank(),
          panel.background = element_rect(fill = "white", color = NA),
          plot.background = element_rect(fill = "white", color = NA)
        ) +
        labs(title = paste("Kenya - Days in 30°C+ Bin -", year),
             x = "Longitude",
             y = "Latitude")
      
      ggsave(file.path(figure_data_dir, paste0("temp_hottest_bin_", year, ".png")), p2)
    } else {
      message("No '30_over_C' bin found for year ", year, ". Skipping hottest bin plot.")
    }
    
    
    nc_close(nc_data)
    gc()
    
  } else {
    message("File not found: ", file_path)
  }
}

temperature_all <- bind_rows(temperature_list)

output_file_temp <- file.path(clean_data_dir, paste0("grid_temp_stat_", start_year_temp, "_", end_year_temp, ".csv"))
write.csv(temperature_all, output_file_temp, row.names = FALSE)
message("Saved combined temperature stat data to: ", output_file_temp)

##### Check with Maps #####
start_year_temp_draw <- 1940
end_year_temp_draw <- 2024
temp_data <- read.csv(output_file_temp) %>% dplyr::filter(year >= start_year_temp_draw & year <= end_year_temp_draw)

africa_map <- ne_countries(scale = "medium", continent = "Africa", returnclass = "sf")
kenya_map <- ne_countries(scale = "medium", country = "Kenya", returnclass = "sf")


# Average temperature over all years
temp_mean_all <- temp_data %>%
  group_by(lat, lon) %>%
  summarise(avg_temp = mean(yearly_mean, na.rm = TRUE), .groups = "drop")

p_avg_all <- ggplot(temp_mean_all, aes(x = lon, y = lat, fill = avg_temp)) +
  geom_tile() +
  geom_sf(data = africa_map, fill = NA, color = "gray70", linewidth = 0.4, inherit.aes = FALSE) +
  geom_sf(data = kenya_map, fill = NA, color = "black", linewidth = 0.5, inherit.aes = FALSE) +
  scale_fill_gradientn(colors = c("blue", "white", "yellow", "red"),
                       values = scales::rescale(c(-10, 5, 20, 35)),
                       name = "Avg Temp (°C)") +
  coord_sf(xlim = c(31, 44), ylim = c(-7, 6), expand = FALSE) +
  theme_bw(base_size = 14)

ggsave(file.path(figure_data_dir, paste0("temp_grid_mean_all_", start_year_temp_draw, "_", end_year_temp_draw, ".png")), p_avg_all, width = 10, height = 6, dpi = 300)

# Average temperature in random 5 years
random_years <- sample(unique(temp_data$year), 5)
temp_random <- temp_data %>%
  dplyr::filter(year %in% random_years) %>%
  group_by(lat, lon) %>%
  summarise(avg_temp = mean(yearly_mean, na.rm = TRUE), .groups = "drop")

p_avg_random <- ggplot(temp_random, aes(x = lon, y = lat, fill = avg_temp)) +
  geom_tile() +
  geom_sf(data = africa_map, fill = NA, color = "gray70", linewidth = 0.4, inherit.aes = FALSE) +
  geom_sf(data = kenya_map, fill = NA, color = "black", linewidth = 0.5, inherit.aes = FALSE) +
  scale_fill_gradientn(colors = c("blue", "white", "yellow", "red"),
                       values = scales::rescale(c(-10, 5, 20, 35)),
                       name = "Avg Temp (°C)") +
  coord_sf(xlim = c(31, 44), ylim = c(-7, 6), expand = FALSE) +
  theme_bw(base_size = 14)

ggsave(file.path(figure_data_dir, paste0("temp_grid_mean_random_", start_year_temp_draw, "_", end_year_temp_draw, ".png")), p_avg_random, width = 10, height = 6, dpi = 300)

# Mean number of days in hottest bin (30°C+) over all years
temp_hottest_all <- temp_data %>%
  group_by(lat, lon) %>%
  summarise(avg_hottest_days = mean(t_30_over_C, na.rm = TRUE), .groups = "drop")

p_hottest_all <- ggplot(temp_hottest_all, aes(x = lon, y = lat, fill = avg_hottest_days)) +
  geom_tile() +
  geom_sf(data = africa_map, fill = NA, color = "gray70", linewidth = 0.4, inherit.aes = FALSE) +
  geom_sf(data = kenya_map, fill = NA, color = "black", linewidth = 0.5, inherit.aes = FALSE) +
  scale_fill_gradientn(colors = c("green", "yellow", "red"),
                       name = "Avg Days in 30°C+ Bin") +
  coord_sf(xlim = c(31, 44), ylim = c(-7, 6), expand = FALSE) +
  theme_bw(base_size = 14)

ggsave(file.path(figure_data_dir, paste0("temp_grid_hottest_bin_mean_all_", start_year_temp_draw, "_", end_year_temp_draw, ".png")), p_hottest_all, width = 10, height = 6, dpi = 300)


# Map of mean number of days in the hottest bin for chosen years
chosen_years <- c(2019, 2020, 2021, 2022, 2024)

temp_hottest_chosen <- temp_data %>%
  dplyr::filter(year %in% chosen_years) %>%
  group_by(lat, lon) %>%
  summarise(avg_hottest_days = mean(t_30_over_C, na.rm = TRUE), .groups = "drop")

p_hottest_chosen <- ggplot(temp_hottest_chosen, aes(x = lon, y = lat, fill = avg_hottest_days)) +
  geom_tile() +
  geom_sf(data = africa_map, fill = NA, color = "gray70", linewidth = 0.4, inherit.aes = FALSE) +
  geom_sf(data = kenya_map, fill = NA, color = "black", linewidth = 0.5, inherit.aes = FALSE) +
  scale_fill_gradientn(colors = c("green", "yellow", "red"),
                       name = "Avg Days in 30°C+ Bin") +
  coord_sf(xlim = c(31, 44), ylim = c(-7, 6), expand = FALSE) +
  theme_bw(base_size = 14) +
  labs(
    title = paste("Kenya - Avg Days in 30°C+ Bin (", paste(chosen_years, collapse = ", "), ")"),
    x = "Longitude",
    y = "Latitude"
  )

ggsave(file.path(figure_data_dir,
                 paste0("temp_grid_hottest_bin_chosen_", min(chosen_years), "_", max(chosen_years), ".png")),
       p_hottest_chosen, width = 10, height = 6, dpi = 300)




# Select 20 random grids in Kenya bounds

if ("geometry" %in% colnames(temp_data)) {
  temp_data <- temp_data %>%
    mutate(lon = sf::st_coordinates(.)[, "X"],
           lat = sf::st_coordinates(.)[, "Y"]) %>%
    st_drop_geometry()
}

set.seed(123)
selected_grids <- temp_data %>%
  dplyr::filter(lon >= 34 & lon <= 40, lat >= -4 & lat <= 4) %>%
  distinct(lat, lon) %>%
  sample_n(20)


for (i in 1:nrow(selected_grids)) {
  grid_lat <- selected_grids$lat[i]
  grid_lon <- selected_grids$lon[i]
  grid_data <- temp_data %>% dplyr::filter(lat == grid_lat, lon == grid_lon)
  
  # Yearly trend of yearly_mean
  p_yearly_mean <- ggplot(grid_data, aes(x = year, y = yearly_mean)) +
    geom_line(color = "blue") +
    geom_point(color = "blue") +
    theme_minimal() +
    labs(title = paste("Yearly Mean Temp at", round(grid_lat, 2), "°N,", round(grid_lon, 2), "°E"),
         x = "Year", y = "Yearly Mean Temp (°C)")
  
  ggsave(file.path(figure_data_dir, paste0("temp_grid_mean_year_", grid_lat, "_", grid_lon, "_", start_year_temp_draw, "_", end_year_temp_draw, ".png")), p_yearly_mean, width = 10, height = 6, dpi = 300)
  
  # Yearly trend of hottest bin days (30°C+)
  p_hottest_bin <- ggplot(grid_data, aes(x = year, y = t_30_over_C)) +
    geom_line(color = "red") +
    geom_point(color = "red") +
    theme_minimal() +
    labs(title = paste("30°C+ Bin Days at", round(grid_lat, 2), "°N,", round(grid_lon, 2), "°E"),
         x = "Year", y = "Days in 30°C+ Bin")
  
  ggsave(file.path(figure_data_dir, paste0("temp_grid_hottest_bin_", grid_lat, "_", grid_lon, "_", start_year_temp_draw, "_", end_year_temp_draw, ".png")), p_hottest_bin, width = 10, height = 6, dpi = 300)
  
  # Monthly temperature trends
  grid_data_long <- grid_data %>%
    dplyr::select(year, starts_with("m")) %>%
    pivot_longer(cols = starts_with("m"), names_to = "month", values_to = "monthly_temp") %>%
    mutate(month_num = as.numeric(sub("m", "", month)),
           date = as.Date(paste(year, month_num, "01", sep = "-")))
  
  p_monthly_trend <- ggplot(grid_data_long, aes(x = date, y = monthly_temp)) +
    geom_line(color = "darkorange") +
    theme_minimal() +
    labs(title = paste("Monthly Temp Trend at", round(grid_lat, 2), "°N,", round(grid_lon, 2), "°E"),
         x = "Date", y = "Monthly Avg Temp (°C)")
  
  ggsave(file.path(figure_data_dir, paste0("temp_grid_mean_month_", grid_lat, "_", grid_lon, "_", start_year_temp_draw, "_", end_year_temp_draw, ".png")), p_monthly_trend, width = 10, height = 6, dpi = 300)
}


##### Grid-level Temperature Stats Generation (2000-2024) #####

start_year_temp_raw <- 1940
end_year_temp_raw <- 2025
temp_years <- start_year_temp_raw:end_year_temp_raw
temperature_list_raw <- list()

leap_year <- function(year) {
  return((year %% 4 == 0 & year %% 100 != 0) | (year %% 400 == 0))
}

for (year in temp_years) {
  file_path <- file.path(raw_data_dir, paste0("era5_kenya_temperature_", year, ".nc"))
  
  if (file.exists(file_path)) {
    nc_data <- nc_open(file_path)
    
    valid_time_vals <- ncvar_get(nc_data, "valid_time")  
    valid_time_units <- ncatt_get(nc_data, "valid_time", "units")$value  
    lon_vals <- ncvar_get(nc_data, "longitude")
    lon_units <- ncatt_get(nc_data, "longitude", "units")$value
    lat_vals <- ncvar_get(nc_data, "latitude")
    lat_units <- ncatt_get(nc_data, "latitude", "units")$value
    
    temperature <- ncvar_get(nc_data, "t2m") 
    
    df <- expand.grid(lon = lon_vals, lat = lat_vals, valid_time = valid_time_vals)
    df$temperature <- as.vector(temperature)
    df$temp_celsius <- df$temperature - 273.15    
    df$year <- year
    
    df$base_date <- as.Date(ifelse(df$year == 1940, 
                                   paste0(df$year, "-01-02"), 
                                   paste0(df$year, "-01-01")))
    df$date <- df$base_date + df$valid_time   
    
    
    df$date <- as.Date(df$date)
    
    df <- df %>% dplyr::select(-valid_time, -base_date)
    
    temperature_list_raw[[as.character(year)]] <- df
    
    nc_close(nc_data)
    gc()
    
  } else {
    message("File not found: ", file_path)
  }
}

temperature_all_raw <- bind_rows(temperature_list_raw)

# output_file_temp_raw <- file.path(clean_data_dir, paste0("grid_temp_stat_raw_", start_year_temp_raw, "_", end_year_temp, ".csv"))
# write.csv(temperature_all_raw, output_file_temp_raw, row.names = FALSE)
# message("Saved combined temperature raw stat data to: ", output_file_temp_raw)

# Split into 3 parts
temperature_part1 <- temperature_all_raw %>% filter(year <= 1973)
temperature_part2 <- temperature_all_raw %>% filter(year > 1973 & year <= 2005)
temperature_part3 <- temperature_all_raw %>% filter(year > 2005)

# Paths
output_temp1 <- file.path(temp_data_dir, "grid_temperature_raw_part1_1940_1973.fst")
output_temp2 <- file.path(temp_data_dir, "grid_temperature_raw_part2_1974_2005.fst")
output_temp3 <- file.path(temp_data_dir, "grid_temperature_raw_part3_2006_2025.fst")

# Save
write_fst(temperature_part1, output_temp1)
write_fst(temperature_part2, output_temp2)
write_fst(temperature_part3, output_temp3)

rm(temperature_all_raw, temperature_part1, temperature_part2, temperature_part3)
gc()


output_temp1 <- file.path(temp_data_dir, "grid_temperature_raw_part1_1940_1973.fst")
output_temp2 <- file.path(temp_data_dir, "grid_temperature_raw_part2_1974_2005.fst")
output_temp3 <- file.path(temp_data_dir, "grid_temperature_raw_part3_2006_2025.fst")

temperature_all_raw <- bind_rows(
  read_fst(output_temp1),
  read_fst(output_temp2),
  read_fst(output_temp3)
)



temperature_2000_2024 <- temperature_all_raw %>%
  dplyr::filter(year >= 2000, year <= 2024)

# Create yearly average temperature per grid
yearly_mean_temp <- temperature_2000_2024 %>%
  group_by(lat, lon, year) %>%
  summarise(yearly_avg_temp_c = mean(temp_celsius, na.rm = TRUE), .groups = "drop")

# Extract month for monthly means
temperature_2000_2024$month <- as.integer(format(temperature_2000_2024$date, "%m"))

# Create wide-format monthly average temperature
monthly_temp <- temperature_2000_2024 %>%
  group_by(lat, lon, month) %>%
  summarise(monthly_avg_temp_c = mean(temp_celsius, na.rm = TRUE), .groups = "drop") %>%
  mutate(month_label = paste0("monthly_avg_temp_c_m", month)) %>%
  dplyr::select(-month) %>%
  pivot_wider(names_from = month_label, values_from = monthly_avg_temp_c)

# Join yearly and monthly summary
grid_temp_stats_2000_2024 <- left_join(yearly_mean_temp, monthly_temp, by = c("lat", "lon"))

# Save to file
output_file_temp_stats <- file.path(clean_data_dir, "grid_temp_stats_2000_2024.csv")
write.csv(grid_temp_stats_2000_2024, output_file_temp_stats, row.names = FALSE)

message("Saved 2000–2024 grid-level temperature stats to: ", output_file_temp_stats)





target_years <- 2018:2024
reference_recent <- 2000:2024
period_recent <- 2018:2021

daily_temp <- temperature_all_raw %>%
  dplyr::filter(year %in% c(reference_recent, target_years)) %>%
  mutate(
    date = as.Date(date),
    year = year(date),
    month = month(date),
    season = case_when(
      month %in% 3:7 ~ "long_rain",
      month %in% 10:12 ~ "short_rain",
      TRUE ~ "other"
    )
  ) %>%
  dplyr::select(lon, lat, year, date, season, temp_celsius)


# Sub-annual
sub_annual_temp <- daily_temp %>%
  group_by(lon, lat, year, season) %>%
  summarise(mean_temp_c = mean(temp_celsius, na.rm = TRUE), .groups = "drop")

# Annual
annual_temp <- daily_temp %>%
  group_by(lon, lat, year) %>%
  summarise(mean_temp_c = mean(temp_celsius, na.rm = TRUE), .groups = "drop") %>%
  mutate(season = "annual")

# Combine
temp_by_period <- bind_rows(sub_annual_temp, annual_temp)


# Mean for 2000–2024
mean_temp_recent <- temp_by_period %>%
  filter(year %in% reference_recent) %>%
  group_by(lon, lat, season) %>%
  summarise(mean_ref_recent = mean(mean_temp_c), .groups = "drop")

# Deviation for 2018–2024
temp_with_dev <- temp_by_period %>%
  filter(year %in% target_years) %>%
  left_join(mean_temp_recent, by = c("lon", "lat", "season")) %>%
  mutate(dev_from_recent = mean_temp_c - mean_ref_recent)

temp_with_dev_wide <- temp_with_dev %>%
  pivot_wider(names_from = season, values_from = c(mean_temp_c, mean_ref_recent, dev_from_recent))


# Filter for bin reference region
temp_for_bin <- daily_temp %>%
  filter(
    year %in% period_recent,
    # lat >= -3, lat <= 3,
    # lon >= 32, lon <= 38
    lat >= -1.5, lat <= 1,
    lon >= 34, lon <= 35
  )

# Define fixed bin breaks
fixed_breaks2 <- c(-Inf, 15, 17, 20, 25, Inf)
bin_labels2 <- c("bin_1", "bin_2", "bin_3", "bin_4", "bin_5")

# Create bin definition dataframe
temp_bin_df <- tibble::tibble(
  bin = bin_labels2,
  annual_lower = fixed_breaks2[-length(fixed_breaks2)],
  annual_upper = fixed_breaks2[-1],
  long_rain_lower = fixed_breaks2[-length(fixed_breaks2)],
  long_rain_upper = fixed_breaks2[-1],
  short_rain_lower = fixed_breaks2[-length(fixed_breaks2)],
  short_rain_upper = fixed_breaks2[-1],
  other_lower = fixed_breaks2[-length(fixed_breaks2)],
  other_upper = fixed_breaks2[-1]
)


write_csv(temp_bin_df, file.path(clean_data_dir, "temp_bin_western_kenya_2018_2021_fixed.csv"))


plot_and_save_temp_histogram <- function(data, season_name, output_dir) {
  season_data <- if (season_name == "annual") {
    data
  } else {
    data %>% filter(season == season_name)
  }
  
  p <- ggplot(season_data, aes(x = temp_celsius)) +
    geom_histogram(bins = 50, fill = "tomato", color = "black") +
    labs(
      title = paste("Histogram of Daily Temperature -", season_name),
      x = "Daily Temperature (°C)",
      y = "Frequency"
    ) +
    theme_minimal()
  
  output_file <- file.path(output_dir, paste0("histogram_temp_", season_name, ".png"))
  ggsave(output_file, plot = p, width = 8, height = 5, dpi = 300)
  message("Saved: ", output_file)
}

seasons <- c("annual", "long_rain", "short_rain", "other")

for (s in seasons) {
  plot_and_save_temp_histogram(temp_for_bin, s, figure_data_dir)
}


daily_temp_binned <- daily_temp %>%
  mutate(
    annual_bin = as.character(cut(temp_celsius, breaks = fixed_breaks2, include.lowest = TRUE, labels = bin_labels2)),
    season_bin = case_when(
      season == "long_rain"  ~ as.character(cut(temp_celsius, breaks = fixed_breaks2,  include.lowest = TRUE, labels = bin_labels2)),
      season == "short_rain" ~ as.character(cut(temp_celsius, breaks = fixed_breaks2, include.lowest = TRUE, labels = bin_labels2)),
      season == "other"      ~ as.character(cut(temp_celsius, breaks = fixed_breaks2,      include.lowest = TRUE, labels = bin_labels2)),
      TRUE ~ NA_character_
    )
  )

# Annual bin counts
annual_bin_counts_temp <- daily_temp_binned %>%
  dplyr::filter(year %in% target_years, !is.na(annual_bin)) %>%
  group_by(lon, lat, year, annual_bin) %>%
  summarise(day_count = n(), .groups = "drop") %>%
  mutate(var_name = paste0("annual_", annual_bin)) %>%
  dplyr::select(-annual_bin) %>%
  pivot_wider(names_from = var_name, values_from = day_count, values_fill = 0)

# Seasonal bin counts
season_bin_counts_temp <- daily_temp_binned %>%
  dplyr::filter(year %in% target_years, !is.na(season_bin)) %>%
  group_by(lon, lat, year, season, season_bin) %>%
  summarise(day_count = n(), .groups = "drop") %>%
  mutate(var_name = paste0(season, "_", season_bin)) %>%
  dplyr::select(-season_bin, -season) %>%
  pivot_wider(names_from = var_name, values_from = day_count, values_fill = 0)

# Combine
binned_counts_temp_combined <- full_join(annual_bin_counts_temp, season_bin_counts_temp, by = c("lon", "lat", "year"))

binned_counts_temp_combined <- binned_counts_temp_combined %>%
  rowwise() %>%
  mutate(
    annual_check     = sum(c_across(starts_with("annual_")), na.rm = TRUE),
    long_rain_check  = sum(c_across(starts_with("long_rain_")), na.rm = TRUE),
    short_rain_check = sum(c_across(starts_with("short_rain_")), na.rm = TRUE),
    other_check      = sum(c_across(starts_with("other_")), na.rm = TRUE)
  ) %>%
  ungroup()

# Function to get ordered names: bin_1 to bin_5, then check
ordered_bin_names <- function(df_names, prefix) {
  bin_part <- paste0(prefix, "_bin_", 1:5)
  check_part <- paste0(prefix, "_check")
  intersect(bin_part, df_names) %>%    # keep only existing ones
    c(check_part)
}

# Apply to each group
all_names <- names(binned_counts_temp_combined)

annual_cols     <- ordered_bin_names(all_names, "annual")
long_rain_cols  <- ordered_bin_names(all_names, "long_rain")
short_rain_cols <- ordered_bin_names(all_names, "short_rain")
other_cols      <- ordered_bin_names(all_names, "other")

# Final column order
ordered_cols <- c("lon", "lat", "year", annual_cols, long_rain_cols, short_rain_cols, other_cols)

# Reorder dataframe
binned_counts_temp_combined <- binned_counts_temp_combined[, ordered_cols]


temp_grid_stat_output <- temp_with_dev_wide %>%
  left_join(binned_counts_temp_combined, by = c("lon", "lat", "year")) %>% 
  left_join(grid_temp_stats_2000_2024, by = c("lon", "lat", "year"))
temp_grid_stat_output_2018_2024 <- file.path(clean_data_dir, "final_temp_grid_stats_2018_2024.csv")
write.csv(temp_grid_stat_output, temp_grid_stat_output_2018_2024, row.names = FALSE)


ls()
data_objects <- ls(envir = .GlobalEnv)[
  sapply(ls(envir = .GlobalEnv), function(x) {
    obj <- get(x, envir = .GlobalEnv)
    is.data.frame(obj) || is.matrix(obj) || is.array(obj)
  })
]

rm(list = data_objects, envir = .GlobalEnv)
gc()



##############################################
##### Sub-county aggregation (2018-2024) #####
##############################################

precip_data <- temp_data <- read_csv(file.path(clean_data_dir, "final_precip_grid_stats_2018_2024.csv"))
temp_data <- read_csv(file.path(clean_data_dir, "final_temp_grid_stats_2018_2024.csv"))

# Add "tp_" prefix to precipitation columns except lon, lat, year
precip_data <- precip_data %>%
  rename_with(
    ~ paste0("tp_", .),
    .cols = -c(lon, lat, year)
  )

# Add "tm_" prefix to temperature columns except lon, lat, year
temp_data <- temp_data %>%
  rename_with(
    ~ paste0("tm_", .),
    .cols = -c(lon, lat, year)
  )

# Perform the join
climate_data <- precip_data %>%
  left_join(temp_data, by = c("lon", "lat", "year"))
names(climate_data)


africa_map <- ne_countries(scale = "medium", continent = "Africa", returnclass = "sf")
kenya_map_path <- file.path(geo_data_dir, "humdata", "ken_admbnda_adm0_iebc_20191031.shp")
kenya_map <- st_read(kenya_map_path)

kenya_villages_path <- file.path(geo_data_dir, "kenya_villages.shp")
kenya_villages <- st_read(kenya_villages_path)
st_crs(kenya_map)
st_crs(kenya_villages)


kenya_counties_path <- file.path(geo_data_dir, "humdata", "ken_admbnda_adm1_iebc_20191031.shp")
kenya_counties <- st_read(kenya_counties_path)
st_crs(kenya_counties)

kenya_subcounties_path <- file.path(geo_data_dir, "humdata", "ken_admbnda_adm2_iebc_20191031.shp")
kenya_subcounties <- st_read(kenya_subcounties_path)
st_crs(kenya_subcounties)


location_file_path <- file.path(farmer_location_dir, "fin_counties_subcounties.dta")
location_df <- read_dta(location_file_path)

fin_counts <- location_df %>%
  group_by(county_ID, county, sub_county) %>%
  summarise(n_farmers = n_distinct(fin), .groups = "drop")

kenya_subcounties_clean <- kenya_subcounties %>%
  mutate(ADM1_EN = str_to_title(ADM1_EN),
         ADM2_EN = str_to_title(ADM2_EN))

kenya_subcounties_clean_filter <- kenya_subcounties_clean %>% dplyr::filter(ADM1_EN == "Bungoma")
# Bungoma South -> Kanduyi , Webuye -> Webuye East & Webuye West

fin_counts_clean <- fin_counts %>%
  mutate(
    county = str_to_title(county),
    sub_county = str_to_title(sub_county),
    # Prepare for matching
    join_key = case_when(
      sub_county == "Bungoma South" ~ "Kanduyi",
      sub_county == "Webuye" ~ "Webuye",  # handle later
      TRUE ~ sub_county
    )
  )


# Regular one-to-one join first
joined_normal <- kenya_subcounties_clean %>%
  inner_join(
    fin_counts_clean %>% dplyr::filter(join_key != "Webuye"),
    by = c("ADM1_EN" = "county", "ADM2_EN" = "join_key")
  ) %>%
  mutate(multiple_match = 0)

# Handle special case for "Webuye" joining to both "Webuye East" and "Webuye West"
webuye_matches <- kenya_subcounties_clean %>%
  dplyr::filter(ADM2_EN %in% c("Webuye East", "Webuye West")) %>%
  left_join(
    fin_counts_clean %>% dplyr::filter(sub_county == "Webuye"),
    by = c("ADM1_EN" = "county")
  ) %>%
  mutate(multiple_match = 1)

# Combine 
kenya_subcounties_merged <- bind_rows(joined_normal, webuye_matches) %>% dplyr::select(-join_key)



grid_polygons <- climate_data %>%
  distinct(lat, lon) %>%
  mutate(
    geometry = purrr::pmap(list(lon, lat), function(x, y) {
      st_polygon(list(matrix(c(
        x - 0.125, y - 0.125,
        x + 0.125, y - 0.125,
        x + 0.125, y + 0.125,
        x - 0.125, y + 0.125,
        x - 0.125, y - 0.125
      ), ncol = 2, byrow = TRUE)))
    })
  ) %>%
  st_sf(crs = 4326)

grid_polygons <- grid_polygons %>%
  mutate(lat = as.numeric(lat), lon = as.numeric(lon))

kenya_crs <- 21037  # UTM Zone 37S (Kenya)

grid_polygons_area <- grid_polygons %>%
  st_transform(crs = kenya_crs) %>%
  mutate(pixel_area = st_area(geometry))


##### Sub County-level summary stat and figure #####
highlighted_subcounties <- kenya_subcounties_merged 

highlighted_subcounties_valid <- st_make_valid(highlighted_subcounties)

# Retry the spatial join
villages_in_subcounty <- st_join(
  kenya_villages, 
  highlighted_subcounties_valid, 
  join = st_within, 
  left = FALSE
)

# Save village-subcounty matches
village_subcounty_file <- file.path(clean_data_dir, "village_list_by_subcounty.csv")
st_drop_geometry(villages_in_subcounty) %>% write_csv(village_subcounty_file)

# Count villages by subcounty
village_counts_subcounty <- villages_in_subcounty %>%
  st_drop_geometry() %>%
  count(ADM1_EN, ADM2_EN, name = "num_villages")

print(village_counts_subcounty)


highlighted_subcounties_proj <- highlighted_subcounties_valid %>%
  st_transform(crs = kenya_crs)

# Intersect grid cells with subcounty polygons
grid_intersections_subcounty <- st_intersection(grid_polygons_area, highlighted_subcounties_proj) %>%
  mutate(intersection_area = st_area(geometry))

grid_intersections_subcounty_check <- 
  grid_intersections_subcounty %>% 
  summarise(
    min_lat = min(lat, na.rm = TRUE),
    max_lat = max(lat, na.rm = TRUE),
    min_lon = min(lon, na.rm = TRUE),
    max_lon = max(lon, na.rm = TRUE)
  )


# Compute relative intersection weight for each grid within each sub-county
subcounty_weights <- grid_intersections_subcounty %>%
  mutate(weight = as.numeric(intersection_area / pixel_area)) %>%
  dplyr::select(lon, lat, ADM1_EN, ADM2_EN, weight)

# Merge sub-county weights with precipitation
climate_weighted_subcounty <- climate_data %>%
  inner_join(subcounty_weights, by = c("lon", "lat"), relationship = "many-to-many") 

names(climate_weighted_subcounty)


climate_weighted_subcounty_clean <- climate_weighted_subcounty %>%
  st_drop_geometry() 

# Identify variables to weight (excluding lon, lat, year, ADM1_EN, ADM2_EN, weight)
vars_to_average <- setdiff(
  names(climate_weighted_subcounty_clean),
  c("lon", "lat", "year", "ADM1_EN", "ADM2_EN", "weight", "geometry")
)

# Compute weighted averages by ADM1_EN, ADM2_EN, year
climate_subcounty_stats <- climate_weighted_subcounty_clean %>%
  group_by(ADM1_EN, ADM2_EN, year) %>%
  summarise(
    across(
      all_of(vars_to_average),
      ~ weighted.mean(.x, weight, na.rm = TRUE),
      .names = "{.col}"
    ),
    .groups = "drop"
  )


output_file_subcounty <- file.path(clean_data_dir, "final_cliimate_subcounty_stats_2018_2024.csv")
write_csv(climate_subcounty_stats, output_file_subcounty)
message("Saved sub-county climate stats to: ", output_file_subcounty)