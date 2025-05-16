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


user_path <- "/Users/sanghwa/Library/CloudStorage/Box-Box/" 

main_dir <- here(paste0(user_path, "Kenya_land_rental"))
raw_data_dir <- file.path(main_dir, "data", "raw", "era5")
geo_data_dir <- file.path(main_dir, "data", "raw", "geospatial")
temp_data_dir <- file.path(main_dir, "data", "temp")
clean_data_dir <- file.path(main_dir, "data", "clean")
figure_data_dir <- file.path(main_dir, "figure", "check")




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
message("Saved combined monsoon onset dates data to: ", output_file_precip)



##### Check with Maps #####

start_year_precip_draw <- 1940
end_year_precip_draw <- 2024
precip_data <- read_csv(output_file_precip) %>% dplyr::filter(year >= start_year_precip_draw & year <= end_year_precip_draw)


africa_map <- ne_countries(scale = "medium", continent = "Africa", returnclass = "sf")
kenya_map_path <- file.path(geo_data_dir, "humdata", "ken_admbnda_adm0_iebc_20191031.shp")
kenya_map <- st_read(kenya_map_path)

kenya_villages_path <- file.path(geo_data_dir, "kenya_villages.shp")
kenya_villages <- st_read(kenya_villages_path)
st_crs(kenya_map)
st_crs(kenya_villages)


target_counties <- c("Bungoma", "Kakamega", "Migori", "Siaya")

kenya_counties_path <- file.path(geo_data_dir, "humdata", "ken_admbnda_adm1_iebc_20191031.shp")
kenya_counties <- st_read(kenya_counties_path)
st_crs(kenya_counties)

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

# Intersect with highlighted counties
grid_intersections_all <- st_intersection(grid_polygons, highlighted_counties) %>%
  mutate(intersection_area = st_area(.))

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


grid_intersections_all <- st_intersection(grid_polygons, highlighted_counties) %>%
  mutate(intersection_area = st_area(.))

# Area of full pixel (approx. in m^2, use st_area on projected CRS if needed)
pixel_area <- st_area(grid_polygons[1, ])

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


##### Process Temperature Data #####
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


