package_tmp <- c("readxl", "openxlsx", "dplyr", "stringr", "data.table", "fst",
                 "tidyr", "readxl", "writexl", "lubridate", "tidyverse", "future.apply", "signal",   
                 "sp", "ncdf4", "raster", "here", "zoo", "sf", "purrr", "furrr", "crsuggest", "progressr", "exactextractr", 
                 "viridis", "rnaturalearth", "rnaturalearthdata", "rnaturalearthhires")
packages <- unique(package_tmp)
lapply(packages, library, character.only = TRUE)
'%!in%' <- function(x,y)!('%in%'(x,y))

#devtools::install_github("ropensci/rnaturalearthhires")


user_path <- "/Users/sanghwa/Library/CloudStorage/Box-Box/" 

main_dir <- here(paste0(user_path, "Monsoon_onset"))
raw_data_dir <- file.path(main_dir, "data", "era5", "raw")
temp_data_dir <- file.path(main_dir, "data", "era5", "temp")
clean_data_dir <- file.path(main_dir, "data", "era5", "clean")
figure_data_dir <- file.path(main_dir, "figure", "era5")
shrug_state_data_dir <- file.path(main_dir, "data", "shrug", "shrug-pc11state-poly-shp")
shrug_district_data_dir <- file.path(main_dir, "data", "shrug", "shrug-pc11dist-poly-shp")
shrug_state_gpkg_dir <- file.path(main_dir, "data", "shrug", "shrug-pc11state-poly-gpkg")
shrug_codebook_data_dir <- file.path(main_dir, "data", "shrug", "codebook")

# if (!dir.exists(clean_data_dir)) {
#   dir.create(clean_data_dir, recursive = TRUE)
# }

##### Check data structure #####
# check_precip <- nc_open(file.path(raw_data_dir,"era5_india_total_precipitation_1997.nc"))
# str(check_precip)
# 
# check_temp <- nc_open(file.path(raw_data_dir,"era5_india_temperature_1997.nc"))
# str(check_temp)
# 
# df_save <- df
# df <- df_save
#
# state_shp_path <- file.path(shrug_state_data_dir, "state.shp")
# state_sf <- st_read(state_shp_path)
# colnames(state_sf)
# str(state_sf)
#
# district_shp_path <- file.path(shrug_district_data_di, "district.shp")
# district_sf <- st_read(district_shp_path)
# colnames(district_sf)
# str(district_sf) 
# 
# shrug_state_gpkg_path <- file.path(shrug_state_gpkg_dir, "state.gpkg")
# state_sf <- st_read(shrug_state_gpkg_path)
# colnames(state_sf)
# str(state_sf)

state_shp_path <- file.path(shrug_state_data_dir, "state.shp")
state_codebook_file <- file.path(shrug_codebook_data_dir, "shrug_state_codebook.csv")
state_sf <- st_read(state_shp_path)

state_codebook <- state_sf %>%
  st_drop_geometry() %>%
  dplyr::select(pc11_s_id, s_name) %>%
  distinct()
write.csv(state_codebook, state_codebook_file, row.names = FALSE)

district_shp_path <- file.path(shrug_district_data_dir, "district.shp")
district_sf <- st_read(district_shp_path)
district_codebook <- district_sf %>%
  st_drop_geometry() %>%
  dplyr::select(pc11_s_id, pc11_d_id, d_name) %>%
  left_join(state_codebook, by = "pc11_s_id") %>% 
  distinct() %>% 
  dplyr::select(pc11_s_id, pc11_d_id, s_name, d_name) %>%
  arrange(pc11_s_id, pc11_d_id, s_name, d_name)
  
dist_cb <- file.path(shrug_codebook_data_dir, "shrug_district_codebook.xlsx")
write_xlsx(district_codebook, path = dist_cb)


##### New Addition : Monsoon K-means #####
## Low-Pass Filtering and K-Means Clustering ###

low_pass_filter <- function(data, cutoff = 1/30) {
  normalized_cutoff <- min(cutoff / 0.5, 0.99) # Normalizing by Nyquist frequency
  butter_filter <- signal::butter(1, normalized_cutoff, type = "low")
  return(signal::filtfilt(butter_filter$b, butter_filter$a, data))
}

perform_kmeans_clustering <- function(df, n_clusters = 5) {
  
  wet_day_summary <- df %>%
    mutate(wet_day = ifelse(total_precipitation >= 0.001, 1, 0)) %>%
    group_by(lat, lon) %>%
    summarise(
      wet_day_count = sum(wet_day, na.rm = TRUE),
      total_rainfall = sum(total_precipitation, na.rm = TRUE),
      mean_rain_intensity = ifelse(wet_day_count > 0, total_rainfall / wet_day_count, NA),
      .groups = "drop"
    )

  df <- df %>%
    left_join(wet_day_summary, by = c("lat", "lon"))

  df <- df %>%
    group_by(lat, lon) %>%
    mutate(filtered_precip = low_pass_filter(total_precipitation)) %>%
    ungroup()

  df <- df %>%
    group_by(lat, lon) %>%
    mutate(
      standardized_precip = (filtered_precip - mean(filtered_precip, na.rm = TRUE)) / sd(filtered_precip, na.rm = TRUE)
    ) %>%
    ungroup()

  eof_matrix <- df %>%
    dplyr::select(lat, lon, date, standardized_precip) %>%
    pivot_wider(names_from = date, values_from = standardized_precip) %>%
    na.omit()  
  
  eof_data <- as.matrix(eof_matrix[,-c(1,2)])  # Remove lat, lon
  lat_lon_data <- eof_matrix[,c("lat", "lon")]
  
  # Singular Value Decomposition (SVD)
  svd_result <- svd(eof_data)
  
  # Leading EOF (first principal component)
  leading_eof <- svd_result$u[,1]
  
  leading_eof_scaled <- scale(leading_eof)
  kmeans_result <- kmeans(leading_eof_scaled, centers = n_clusters, nstart = 25)
  
  cluster_df <- data.frame(lat_lon_data, cluster = kmeans_result$cluster)
  
  df_result <- df %>%
    left_join(cluster_df, by = c("lat", "lon"))  

  return(df_result)
}

plot_cluster_results <- function(df, year) {
  
  india_map <- ne_countries(scale = "medium", country = "India", returnclass = "sf")
  india_states <- ne_states(country = "India", returnclass = "sf")
  
  df_sf <- st_as_sf(df, coords = c("lon", "lat"), crs = 4326)
  
  # Cluster Plot
  p1 <- ggplot() +
    geom_sf(data = india_map, fill = "gray90", color = "black", alpha = 0.5) +
    geom_sf(data = df_sf, aes(color = as.factor(cluster)), size = 0.8) +
    scale_color_viridis_d(name = "Cluster") +
    geom_sf(data = india_states, fill = NA, color = "black", size = 0.6) +
    coord_sf(xlim = c(65, 100), ylim = c(5, 40), expand = FALSE) +
    theme_minimal() +
    labs(title = paste("K-Means Clustering Results -", year))
  
  # Rainfall Mean and Std Dev Plot
  cluster_summary <- df %>%
    group_by(cluster, date) %>%
    summarise(
      mean_rainfall = mean(filtered_precip, na.rm = TRUE),
      sd_rainfall = sd(filtered_precip, na.rm = TRUE)
    )
  
  cluster_summary <- cluster_summary %>%
    mutate(date = as.Date(paste0("2000-", date))) # Add dummy year for plotting
  
  p2 <- ggplot(cluster_summary, aes(x = date, y = mean_rainfall, color = as.factor(cluster), group = cluster)) +
    geom_line() +
    geom_ribbon(aes(ymin = mean_rainfall - sd_rainfall, ymax = mean_rainfall + sd_rainfall, fill = as.factor(cluster)), alpha = 0.2) +
    scale_color_viridis_d(name = "Cluster") +
    scale_fill_viridis_d(name = "Cluster") +
    scale_x_date(labels = function(x) format(x, "%m-%d")) + # Format using base R
    theme_minimal() +
    labs(title = paste("Mean and Standard Deviation of Rainfall -", year),
         x = "Date (MM-DD)", y = "Rainfall (mm/day)")
  
  ggsave(file.path(figure_data_dir, paste0("kmeans_cluster_map_", year, ".png")), plot = p1, width = 10, height = 6, dpi = 300)
  ggsave(file.path(figure_data_dir, paste0("rainfall_summary_", year, ".png")), plot = p2, width = 10, height = 6, dpi = 300)

  for (cl in unique(cluster_summary$cluster)) {
    
    cluster_data <- cluster_summary %>% dplyr::filter(cluster == cl)
    
    p <- ggplot(cluster_data, aes(x = date, y = mean_rainfall, group = cluster)) +
      geom_line(color = viridis::viridis_pal()(5)[cl]) +
      geom_ribbon(aes(ymin = mean_rainfall - sd_rainfall, ymax = mean_rainfall + sd_rainfall),
                  fill = viridis::viridis_pal()(5)[cl], alpha = 0.2) +
      scale_x_date(labels = function(x) format(x, "%m-%d")) +
      theme_minimal() +
      labs(title = paste("Mean and Standard Deviation of Rainfall - Cluster", cl, "(", year, ")"),
           x = "Date (MM-DD)", y = "Rainfall (mm/day)")
    
    ggsave(file.path(figure_data_dir, paste0("rainfall_summary_cluster_", cl, "_", year, ".png")),
           plot = p, width = 10, height = 6, dpi = 300)
  }  
  
}


start_year_import <- 1940
end_year_import <- 2025
years <- start_year_import:end_year_import

for (year in years) {
  file_path <- file.path(raw_data_dir, paste0("era5_india_total_precipitation_", year, ".nc"))

  if (file.exists(file_path)) {
    nc_data <- nc_open(file_path)

    valid_time_vals <- ncvar_get(nc_data, "valid_time")
    lon_vals <- ncvar_get(nc_data, "longitude")
    lat_vals <- ncvar_get(nc_data, "latitude")
    total_precipitation <- ncvar_get(nc_data, "tp")

    df <- expand.grid(lon = lon_vals, lat = lat_vals, valid_time = valid_time_vals)
    df$total_precipitation <- as.vector(total_precipitation)
    df$year <- year

    df$base_date <- as.Date(ifelse(year == 1940, paste0(year, "-01-02"), paste0(year, "-01-01")))
    df$date <- df$base_date + df$valid_time

    df <- df %>% dplyr::select(lon, lat, total_precipitation, year, date)

    write_fst(df, path = file.path(temp_data_dir, paste0("precipitation_", year, ".fst")))
    #write_csv(df, file.path(temp_data_dir, paste0("precipitation_", year, ".csv")))

    nc_close(nc_data)
    rm(df, valid_time_vals, lon_vals, lat_vals, total_precipitation, nc_data)
    gc()

    message("Year ", year, " is done.")

  } else {
    message("File not found: ", file_path)
  }
}


start_year_kmeans <- 1940
end_year_kmeans <- 2024
years <- start_year_kmeans:end_year_kmeans

year_chunks <- split(years, ceiling(seq_along(years) / 5))

summary_chunks <- list()

for (chunk in year_chunks) {
  chunk_list <- list()
  
  for (year in chunk) {
    fst_path <- file.path(temp_data_dir, paste0("precipitation_", year, ".fst"))
    
    if (file.exists(fst_path)) {
      df <- read_fst(fst_path)
      
      df <- df %>%
        dplyr::filter(month(date) >= 4 & month(date) <= 11,
               !(month(date) == 11 & day(date) > 30))
      
      df <- df %>% mutate(day = format(as.Date(date), "%m-%d"))
      
      chunk_list[[as.character(year)]] <- df
      
      rm(df)
      gc()
      
      message("Processed year ", year)
    }
  }
  
  chunk_df <- bind_rows(chunk_list) %>%
    group_by(lat, lon, day) %>%
    summarize(total_precipitation = mean(total_precipitation, na.rm = TRUE), .groups = "drop")
  
  summary_chunks[[paste0("chunk_", chunk[1], "_", tail(chunk, 1))]] <- chunk_df
  
  rm(chunk_list, chunk_df)
  gc()
  
  message("Finished chunk: ", chunk[1], "-", tail(chunk, 1))
}

df_collapsed <- bind_rows(summary_chunks) %>%
  group_by(lat, lon, day) %>%
  summarize(mean_precip = mean(total_precipitation, na.rm = TRUE), .groups = "drop") %>% 
  dplyr::rename(total_precipitation = mean_precip)

df_unique <- df_collapsed %>% distinct(lat, lon)

half_cell <- 0.125
equal_area_crs <- 7755

unique_grids <- df_unique %>%
  dplyr::mutate(grid_id = row_number()) %>%
  rowwise() %>%
  mutate(geometry = st_as_sfc(st_bbox(c(
    xmin = lon - half_cell,
    xmax = lon + half_cell,
    ymin = lat - half_cell,
    ymax = lat + half_cell
  ), crs = st_crs(4326)))) %>%
  ungroup() %>%
  st_as_sf(crs = 4326) %>%
  st_transform(equal_area_crs) %>%
  dplyr::mutate(pixel_area = st_area(geometry)) %>%
  dplyr::select(grid_id, lon, lat, pixel_area, geometry)

district_shp_path <- file.path(shrug_district_data_dir, "district.shp")
district_sf <- st_read(district_shp_path) %>%
  st_transform(equal_area_crs) %>%
  st_make_valid() %>%
  dplyr::mutate(district_area = st_area(.)) %>%
  dplyr::select(pc11_s_id, pc11_d_id, d_name, geometry, district_area)

df_land_unique <- sf::st_join(unique_grids, district_sf, join = st_intersects, left = FALSE) %>% dplyr::select(lat, lon) %>% distinct() #5080
df_land <- dplyr::inner_join(df_collapsed, df_land_unique, by = c("lat", "lon"))

df_land_filter <- df_land %>%
  dplyr::filter(day >= "04-01" & day <= "11-30") %>%
  dplyr::rename(date = day)
df_cluster <- perform_kmeans_clustering(df_land_filter)

year_range <- paste0(start_year_kmeans, "-", end_year_kmeans)
plot_cluster_results(df_cluster, year_range)

message(paste0("K-means clustering completed and plot saved for year ", year_range))

df_cluster_clear <- df_cluster %>% st_drop_geometry() %>% dplyr::select(-geometry)

kmeans_file <- file.path(clean_data_dir, paste0("k_means_monsoon_", start_year_kmeans, "_", end_year_kmeans, ".csv"))
write.csv(df_cluster_clear, kmeans_file, row.names = FALSE)
cat("Combined results saved at:", kmeans_file, "\n")


##### Monsoon Onset Algorithm #####
determine_monsoon_onset <- function(df) {
  
  df <- df %>% 
    dplyr::filter(format(date, "%m") >= "04" & format(date, "%m") <= "10") # 3782022
  
  check_count_zero <- sum(df$total_precipitation == 0, na.rm = TRUE)
  message(paste0("Number of grids with ZERO total precipitation for year ", year, ": ", check_count_zero))
  check_count_na <- sum(is.na(df$total_precipitation))
  message(paste0("Number of grids with NA total precipitation for year ", year, ": ", check_count_na))
  
  # 5-day moving sum (instead of mean)
  df <- df %>%
    arrange(lat, lon, date) %>%
    group_by(lat, lon) %>%
    mutate(moving_sum = zoo::rollapply(total_precipitation, 5, sum, fill = NA, align = "left"))
  
  # 5-day wet spell interannual mean (April - October)
  climatology <- df %>%
    group_by(lat, lon) %>%
    summarise(mean_5day_rain = mean(moving_sum, na.rm = TRUE), .groups = "drop")
  
  df <- df %>%
    left_join(climatology, by = c("lat", "lon"))
  
  # potential onset candidates (p*_{i*d*y*})
  df <- df %>%
    mutate(onset_candidate = 0) %>%
    mutate(onset_candidate = ifelse(total_precipitation >= 0.004 & moving_sum >= mean_5day_rain, 1, 0))
  
  df <- df %>%
    arrange(lat, lon, date) %>%
    group_by(lat, lon) %>%
    mutate(
      dryspell_10d_sum = zoo::rollapply(total_precipitation, 10, sum, fill = NA, align = "left"),
      dryspell_10d = ifelse(is.na(dryspell_10d_sum), NA, 0),
      dryspell_10d = ifelse(!is.na(dryspell_10d_sum) & dryspell_10d_sum < 0.005, 1, dryspell_10d),      
      
      dryspell_30pre = zoo::rollapply(dryspell_10d, 20, sum, fill = NA, align = "left") / 20,
      dryspell_30 = ifelse(is.na(dryspell_30pre), NA, 0),
      dryspell_30 = ifelse(!is.na(dryspell_30pre) & dryspell_30pre > 0, 1, dryspell_30)
    ) %>%
    ungroup()  

  
  df_final <- df %>%
    mutate(onset_candidate4 = ifelse(is.na(onset_candidate) | is.na(dryspell_30), NA,
                                     ifelse(onset_candidate == 1 & dryspell_30 == 0, 1, 0))) %>% 
    group_by(lat, lon, year) %>%
    summarise(onset_date4_p = ifelse(any(onset_candidate4 == 1), min(date[onset_candidate4 == 1], na.rm = TRUE), NA),
              .groups = "drop") %>%
    mutate(onset_date4_p = as.Date(onset_date4_p))
  
  # Additional processing for groups without onset_candidate4 = 1
  df_no_onset <- df %>%
    mutate(onset_candidate4 = 0) %>%
    mutate(onset_candidate4 = ifelse(onset_candidate == 1 & dryspell_30 == 0, 1, 0)) %>%
    group_by(lat, lon, year) %>%
    dplyr::filter(all(onset_candidate4 == 0 | is.na(onset_candidate4))) %>% # must check this part
    ungroup() %>%
    arrange(lat, lon, date) %>%
    # group_by(lat, lon) %>%
    # mutate(
    #   dryspell_5d_sum = zoo::rollapply(total_precipitation, 5, sum, fill = NA, align = "left"),
    #   dryspell_5d = ifelse(is.na(dryspell_5d_sum), NA, 0),
    #   dryspell_5d = ifelse(!is.na(dryspell_5d_sum) & dryspell_10d_sum < 0.0025, 1, dryspell_5d),
    #   dryspell_30pre_v2 = zoo::rollapply(dryspell_5d, 20, sum, fill = NA, align = "left") / 20,
    #   dryspell_30_v2 = ifelse(is.na(dryspell_30pre), NA, 0),
    #   dryspell_30_v2 = ifelse(!is.na(dryspell_30pre) & dryspell_30pre > 0, 1, dryspell_30)
    # ) %>%
    # ungroup() %>%
    mutate(onset_candidate4_v2 = ifelse(is.na(onset_candidate) | is.na(dryspell_30), NA,
                                        ifelse(onset_candidate == 1, 1, 0))) %>% # is.na(dryspell_30_v2) / dryspell_30_v2 == 0    
    group_by(lat, lon, year) %>%
    summarise(onset_date4_p_v2 = ifelse(any(onset_candidate4_v2 == 1), min(date[onset_candidate4_v2 == 1], na.rm = TRUE), NA),
              .groups = "drop") %>%
    mutate(onset_date4_p_v2 = as.Date(onset_date4_p_v2))
  
  # Merge results
  df_final <- df_final %>%
    left_join(df_no_onset, by = c("lat", "lon", "year")) %>%
    mutate(onset_date4_p_final = ifelse(is.na(onset_date4_p), onset_date4_p_v2, onset_date4_p)) %>%
    mutate(onset_date4_p_final = as.Date(onset_date4_p_final))
  
  return(df_final)
}  


##### Process Precipitation Data #####
start_year_precip <- 1940
end_year_precip <- 2025
years <- start_year_precip:end_year_precip
monsoon_onset_dates_list <- list()

for (year in years) {
  file_path <- file.path(raw_data_dir, paste0("era5_india_total_precipitation_", year, ".nc"))
  
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
    
    # df$lon_units <- lon_units
    # df$lat_units <- lat_units
    # df$valid_time_units <- valid_time_units
    
    # assign(paste0("precip_", year), df, envir = .GlobalEnv)
    # year_output_file <- file.path(temp_data_dir, paste0("precipitation_", year, ".csv"))
    # write.csv(df, year_output_file, row.names = FALSE)

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
    
        
    monsoon_onset_dates <- determine_monsoon_onset(df) %>%
      left_join(yearly_sum_tp, by = c("lat", "lon"))
    
    monsoon_onset_dates <- monsoon_onset_dates %>%
      left_join(monthly_tp, by = c("lat", "lon"))
    
    monsoon_onset_dates_list[[as.character(year)]] <- monsoon_onset_dates

    interim_output_file_onset <- file.path(temp_data_dir, paste0("grid_monsoon_onset_dates_", year, ".csv"))
    write.csv(monsoon_onset_dates, interim_output_file_onset, row.names = FALSE)
    
    nc_close(nc_data)

  
    # start_date <- as.Date(paste0(year, "-04-01"))
    # end_date <- as.Date(paste0(year, "-10-31"))
    # selected_dates <- seq(start_date, end_date, by = "1 days")
    #
    # df_filtered <- monsoon_onset_dates %>%
    #   filter(format(onset_date4_p_final, "%Y") == as.character(year) & onset_date4_p_final %in% selected_dates)
    #
    # df_sf <- st_as_sf(df_filtered, coords = c("lon", "lat"), crs = 4326)
    #
    # india_map <- ne_countries(scale = "medium", country = "India", returnclass = "sf")
    #
    # india_states <- ne_states(country = "India", returnclass = "sf")
    #
    # df_grouped <- df_sf %>%
    #   group_by(onset_date4_p_final) %>%
    #   summarise(geometry = st_combine(geometry)) %>%
    #   ungroup()
    #
    # df_grouped$label_date <- format(df_grouped$onset_date4_p_final, "%m-%d")
    #
    # p <- ggplot() +
    #   geom_sf(data = india_map, fill = "gray90", color = "black", alpha = 0.5) +
    #   geom_sf(data = df_grouped, aes(color = label_date), size = 0.8) +
    #   scale_color_viridis_d(name = "Onset Date (MM-DD)") +
    #   geom_sf(data = india_states, fill = NA, color = "black", size = 0.6) +
    #   coord_sf(xlim = c(65, 100), ylim = c(5, 40), expand = FALSE) +
    #   theme_minimal() +
    #   labs(title = paste("Monsoon Onset Dates Over India (Main - No dry spell condition, empty pixel) -", year),
    #        subtitle = "Points with the same onset date",
    #        x = "Longitude (East)", y = "Latitude (North)") +
    #   theme(
    #     legend.position = "right",
    #     legend.text = element_text(size = 5),
    #     legend.title = element_text(size = 7),
    #     axis.text = element_text(size = 9),
    #     axis.title = element_text(size = 10),
    #     plot.title = element_text(size = 12, face = "bold"),
    #     plot.subtitle = element_text(size = 10)
    #   )
    #
    # output_filename <- paste0("monsoon_onset_map_", year, "_main.png")
    # output_path <- file.path(figure_data_dir, output_filename)
    # ggsave(output_path, plot = p, width = 10, height = 6, dpi = 300)
    #
    # cat("Map saved at:", output_path, "\n")

    # p2 <- ggplot() +
    #   geom_sf(data = india_map, fill = "gray90", color = "black", alpha = 0.5) +  # India boundary
    #   geom_sf(data = df_grouped, aes(color = as.factor(onset_date4_p)), size = 0.8) +  # Iso-date lines
    #   scale_color_viridis_d(name = "Onset Date") +
    #   coord_sf(xlim = c(65, 100), ylim = c(5, 35), expand = FALSE) +  # Focus on India
    #   theme_minimal() +
    #   labs(title = paste("Monsoon Onset Dates Over India -", year),
    #        subtitle = "Lines connect points with the same onset date",
    #        x = "Longitude (East)", y = "Latitude (North)") +
    #   theme(legend.position = "right")
    #
    # # Save the plot with year in the filename
    # output_filename <- paste0("monsoon_onset_map_int2_", year, ".png")
    # output_path <- file.path(figure_data_dir, output_filename)
    # ggsave(output_path, plot = p1, width = 10, height = 6, dpi = 300)
    #

    rm(df, monthly_tp, yearly_sum_tp, monsoon_onset_dates, total_precipitation,
       valid_time_vals, valid_time_units, lon_vals, lon_units,
       lat_vals, lat_units, nc_data)
    gc(verbose = FALSE)
    
    
           
  } else {
    message("File not found: ", file_path)
  }
}

monsoon_onset_all <- bind_rows(monsoon_onset_dates_list)

output_file_onset <- file.path(clean_data_dir, paste0("grid_monsoon_onset_dates_", start_year_precip, "_", end_year_precip, ".csv"))
write.csv(monsoon_onset_all, output_file_onset, row.names = FALSE)
message("Saved combined monsoon onset dates data to: ", output_file_onset)


## Grid- District Match 

# All from above codes
start_year_precip <- 1940
end_year_precip <- 2025
years <- start_year_precip:end_year_precip
output_file_onset <- file.path(clean_data_dir, paste0("grid_monsoon_onset_dates_", start_year_precip, "_", end_year_precip, ".csv"))
state_shp_path <- file.path(shrug_state_data_dir, "state.shp")
district_shp_path <- file.path(shrug_district_data_dir, "district.shp")


equal_area_crs <- 7755
half_cell <- 0.125

# Process shapefiles
district_sf <- st_read(district_shp_path) %>%
  st_transform(equal_area_crs) %>%
  st_make_valid() %>%
  dplyr::mutate(district_area = st_area(.)) %>%
  dplyr::select(pc11_s_id, pc11_d_id, d_name, geometry, district_area)

state_codebook <- st_read(state_shp_path) %>%
  st_drop_geometry() %>%
  dplyr::distinct(pc11_s_id, s_name)

# Read monsoon data and create pixel grid
monsoon_onset_all <- readr::read_csv(output_file_onset)

unique_grids <- monsoon_onset_all %>%
  dplyr::distinct(lon, lat) %>%
  dplyr::mutate(grid_id = dplyr::row_number()) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(geometry = st_as_sfc(st_bbox(c(
    xmin = lon - half_cell,
    xmax = lon + half_cell,
    ymin = lat - half_cell,
    ymax = lat + half_cell
  ), crs = st_crs(4326)))) %>%
  dplyr::ungroup() %>%
  st_as_sf(crs = 4326) %>%
  st_transform(equal_area_crs) %>%
  dplyr::mutate(pixel_area = st_area(geometry)) %>%
  dplyr::select(grid_id, lon, lat, pixel_area, geometry)

# Match grid to district
intersection_matrix <- st_intersects(unique_grids, district_sf, sparse = TRUE)

candidate_pairs_df <- purrr::imap_dfr(intersection_matrix, function(district_idxs, grid_idx) {
  if (length(district_idxs) == 0) return(NULL)
  grid_row <- unique_grids[grid_idx, ]
  tibble::tibble(
    grid_id = grid_row$grid_id,
    lon = grid_row$lon,
    lat = grid_row$lat,
    pc11_d_id = district_sf$pc11_d_id[district_idxs]
  )
})

# Check
n_candidate_pairs <- nrow(candidate_pairs_df)
message("Number of candidate grid-district intersecting pairs: ", n_candidate_pairs) # 10446 

grid_district_counts <- candidate_pairs_df %>%
  dplyr::group_by(grid_id) %>%
  dplyr::summarise(n_districts = dplyr::n_distinct(pc11_d_id), .groups = "drop") %>%
  dplyr::filter(n_districts >= 2)

message("Number of grid cells intersecting with >= 2 districts: ", nrow(grid_district_counts)) # 3505

# Compute intersection weights 
plan(multisession, workers = availableCores() - 1)
with_progress({
  p <- progressor(steps = nrow(candidate_pairs_df))
  
  grid_district_weights <- candidate_pairs_df %>%
    future_pmap_dfr(function(grid_id, lon, lat, pc11_d_id) {
      p()
      
      grid_geom <- unique_grids %>% dplyr::filter(grid_id == !!grid_id)
      dist_geom <- district_sf %>% dplyr::filter(pc11_d_id == !!pc11_d_id)
      
      inter <- tryCatch(st_intersection(grid_geom, dist_geom), error = function(e) NULL)
      if (is.null(inter) || nrow(inter) == 0) return(NULL)
      
      inter <- st_make_valid(inter)
      inter <- inter[st_is_valid(inter), ]
      inter <- inter[!st_is_empty(inter), ]
      if (nrow(inter) == 0) return(NULL)
      
      intersection_area <- tryCatch(st_area(inter), error = function(e) NA_real_)
      if (is.na(intersection_area)) return(NULL)
      
      inter %>%
        dplyr::mutate(
          intersection_area = intersection_area,
          pixel_area = st_area(grid_geom),
          weight = round(as.numeric(intersection_area / pixel_area), 4)
        ) %>%
        st_drop_geometry() %>%
        dplyr::mutate(lon = lon, lat = lat) %>%
        dplyr::select(grid_id, lon, lat, pc11_d_id, intersection_area, weight)
    }, .options = furrr_options(seed = TRUE))
})
plan(sequential)

# Check Coverage
coverage_check <- grid_district_weights %>%
  dplyr::group_by(pc11_d_id) %>%
  dplyr::summarise(total_intersection = sum(as.numeric(intersection_area), na.rm = TRUE), .groups = "drop") %>%
  dplyr::left_join(district_sf %>% st_drop_geometry() %>% dplyr::select(pc11_d_id, district_area), by = "pc11_d_id") %>%
  dplyr::mutate(coverage_ratio = total_intersection / as.numeric(district_area)) %>%
  dplyr::arrange(coverage_ratio)

# districts with <99% coverage
coverage_check %>%
  dplyr::filter(coverage_ratio < 0.99999999) %>%
  print()
coverage_check %>%
  dplyr::filter(coverage_ratio > 1.0000001) %>%
  print()

check <- coverage_check %>%
  dplyr::filter(pc11_d_id == "345") 
check2 <- grid_district_weights %>%
  dplyr::filter(pc11_d_id == "345") 

write_csv(grid_district_weights, file.path(clean_data_dir,paste0("grid_district_weights_key.csv")))


monsoon_onset_all <- read_csv(file.path(clean_data_dir, paste0("grid_monsoon_onset_dates_", 1940, "_", 2025, ".csv")))
grid_district_weights <- read_csv(file.path(clean_data_dir,paste0("grid_district_weights_key.csv")))

# Expand to all years 
grid_district_weights_check <- grid_district_weights %>% arrange(grid_id, weight)
grid_year_expanded <- grid_district_weights %>%
  mutate(
    pc11_d_id = as.character(pc11_d_id)
  ) %>%
  tidyr::expand(nesting(grid_id, lon, lat, pc11_d_id, intersection_area, weight), year = years) %>% 
  arrange(grid_id, lon, lat, year, weight)

# check_district <- district_sf %>% st_drop_geometry() %>% dplyr::select(pc11_d_id, pc11_s_id, d_name, district_area) %>% dplyr::filter(pc11_d_id == 84)

grid_district_onset_data <- monsoon_onset_all %>%
  dplyr::left_join(grid_year_expanded, by = c("lon", "lat", "year")) %>%
  dplyr::filter(weight > 0) %>%
  dplyr::left_join(
    district_sf %>%
      st_drop_geometry() %>%
      dplyr::select(pc11_d_id, pc11_s_id, d_name, district_area),
    by = "pc11_d_id"
  ) %>%
  dplyr::left_join(state_codebook, by = "pc11_s_id") %>%
  dplyr::select(pc11_d_id, lat, lon, year, dplyr::everything(), -grid_id) %>% 
  mutate(area_weight = intersection_area/district_area) 

# check_grid_district_onset_data <- grid_district_onset_data %>% arrange(d_name, year)

grid_district_onset_output_path <- file.path(clean_data_dir, paste0("grid_district_weights_", min(years), "_", max(years), ".csv")) # Be careful with max(years)
write_csv(grid_district_onset_data, grid_district_onset_output_path)
message("Saved grid_district_onset dataset with ", nrow(grid_district_onset_data), " rows to:\n", grid_district_onset_output_path)


##### District-level dataset for onset dates #####
grid_district_onset_load <- fread(file.path(clean_data_dir, paste0("grid_district_weights_", 1940, "_", 2025, ".csv")))
kmeans_file_load <- fread(file.path(clean_data_dir, paste0("k_means_monsoon_", 1940, "_", 2024, ".csv"))) 

# grid_district_onset_load_prev <- fread(file.path(clean_data_dir, paste0("grid_district_weights_", 1940, "_", 2025, "_prev", ".csv")))
# onset_grid_check_1 <- grid_district_onset_load %>% dplyr::filter(is.na(onset_date4_p_final))
# onset_grid_check_2 <- grid_district_onset_load_prev %>% dplyr::filter(is.na(onset_date4_p_final)) 
# 
# grid_district_onset_data_check <- grid_district_onset_data %>% dplyr::filter(is.na(pc11_s_id))


sum(is.na(grid_district_onset_data$pc11_s_id)) #119425 -> 0 
sum(is.na(grid_district_onset_data$pc11_d_id))
# 
# sum(is.na(grid_district_onset_load$pc11_s_id)) #119425
# sum(is.na(grid_district_onset_load$pc11_d_id))
# 
# sum(is.na(grid_district_onset_load_prev$pc11_s_id)) #0
# sum(is.na(grid_district_onset_load_prev$pc11_d_id))

# grid_district_onset_load <- grid_district_onset_load_prev

kmeans_unique <- kmeans_file_load %>% dplyr::select(lat, lon, cluster) %>% dplyr::distinct() 

df_sf_precip_drop <- grid_district_onset_load %>%
  dplyr::left_join(kmeans_unique, by = c("lat", "lon"))

df_sf_precip_drop_check <- df_sf_precip_drop %>% dplyr::filter(is.na(cluster) & !is.na(s_name)) %>% dplyr::select(lat, lon, s_name, d_name, cluster) %>% dplyr::distinct() #should be zero



##### District-level dataset for onset dates #####

# Check NA counts for ID/location variables
sapply(df_sf_precip_drop[, c("pc11_s_id", "pc11_d_id", "d_name", "s_name")], function(x) sum(is.na(x)))

# So there is 000 in pc11_d_id 
na_check <- df_sf_precip_drop %>%
  group_by(pc11_d_id) %>%
  summarise(
    total_rows = n(),
    na_d_name = sum(is.na(d_name)),
    non_na_d_name = sum(!is.na(d_name)),
    .groups = "drop"
  ) %>%
  mutate(case_type = case_when(
    na_d_name == 0 ~ "All present",
    non_na_d_name == 0 ~ "All missing",
    TRUE ~ "Mixed (some NA, some not)"
  ))
na_check %>% dplyr::filter(case_type != "All present")


# Create `na_count` and `pixel_count`
df_sf_precip_drop$na_count <- ifelse(is.na(df_sf_precip_drop$onset_date4_p), 1, 0)
df_sf_precip_drop$pixel_count <- 1

# Drop rows with missing district info
df_filtered_onset <- df_sf_precip_drop[complete.cases(df_sf_precip_drop[, c("pc11_s_id", "pc11_d_id")]), ]
df_filtered_onset <- as.data.frame(df_filtered_onset)
date_vars <- c("onset_date4_p", "onset_date4_p_v2", "onset_date4_p_final")
df_filtered_onset[date_vars] <- lapply(df_filtered_onset[date_vars], as.Date)


# Collapse to district-year level
district_level_df_onset <- df_filtered_onset %>%
  group_by(pc11_s_id, pc11_d_id, d_name, s_name, year) %>%
  summarise(
    # Simple mean for yearly precipitation
    sim_mean_yearly_sum_tp = mean(yearly_sum_tp, na.rm = TRUE),
    # Weighted means for yearly precipitation
    mean_yearly_sum_tp = weighted.mean(yearly_sum_tp, weight, na.rm = TRUE),
    area_mean_yearly_sum_tp = weighted.mean(yearly_sum_tp, area_weight, na.rm = TRUE),

    # Weighted means for monthly precipitation
    across(starts_with("monthly_sum_tp_m"), ~weighted.mean(.x, weight, na.rm = TRUE), .names = "mean_{.col}"),
    across(starts_with("monthly_sum_tp_m"), ~weighted.mean(.x, area_weight, na.rm = TRUE), .names = "area_mean_{.col}"),
        
    # Weighted mean with `weight`
    mean_onset_date4_p = as.Date(weighted.mean(as.numeric(onset_date4_p), weight, na.rm = TRUE), origin = "1970-01-01"),
    mean_onset_date4_p_v2 = as.Date(weighted.mean(as.numeric(onset_date4_p_v2), weight, na.rm = TRUE), origin = "1970-01-01"),
    mean_onset_date4_p_final = as.Date(weighted.mean(as.numeric(onset_date4_p_final), weight, na.rm = TRUE), origin = "1970-01-01"),
    
    # Weighted mean with `area_weight`
    area_mean_onset_date4_p = as.Date(weighted.mean(as.numeric(onset_date4_p), area_weight, na.rm = TRUE), origin = "1970-01-01"),
    area_mean_onset_date4_p_v2 = as.Date(weighted.mean(as.numeric(onset_date4_p_v2), area_weight, na.rm = TRUE), origin = "1970-01-01"),
    area_mean_onset_date4_p_final = as.Date(weighted.mean(as.numeric(onset_date4_p_final), area_weight, na.rm = TRUE), origin = "1970-01-01"),
    
    # Sum of intersection area, unique district area
    sum_intersection_area = sum(intersection_area, na.rm = TRUE),
    district_area = first(district_area),
    
    # NA and pixel counts
    sum_na_count = sum(na_count, na.rm = TRUE),
    sum_pixel_count = sum(pixel_count, na.rm = TRUE),
    
    # Weighted sums for NA and pixel using `weight`
    sum_na_weight = sum(weight[na_count == 1], na.rm = TRUE),
    sum_pixel_weight = sum(weight[pixel_count == 1], na.rm = TRUE),
    
    # Area-based NA
    sum_na_area = sum(intersection_area[na_count == 1], na.rm = TRUE),
    
    cluster_1 = sum(area_weight[cluster == 1], na.rm = TRUE),
    cluster_2 = sum(area_weight[cluster == 2], na.rm = TRUE),
    cluster_3 = sum(area_weight[cluster == 3], na.rm = TRUE),
    cluster_4 = sum(area_weight[cluster == 4], na.rm = TRUE),
    cluster_5 = sum(area_weight[cluster == 5], na.rm = TRUE),
    cluster_na = sum(area_weight[is.na(cluster)], na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  mutate(
    proportion_na = sum_na_count / sum_pixel_count,
    proportion_na_weight = sum_na_weight / sum_pixel_weight,
    proportion_na_area = sum_na_area / district_area,
    area_difference_rate = (district_area - sum_intersection_area) / district_area,
    cluster_sum = cluster_1 + cluster_2 + cluster_3 + cluster_4 + cluster_5 + cluster_na
  )

district_level_df_onset_check <- district_level_df_onset %>% dplyr::filter(cluster_sum < 0.9999) # There was Purba Medinipur, West Bengal ; Bathinda, Punjab ; Sirsa, Haryana
district_level_df_onset_check2 <- district_level_df_onset %>% dplyr::filter(cluster_sum > 1.0000001) # 0

district_onset_output_file <- file.path(clean_data_dir, paste0("district_monsoon_onset_dates_", start_year_precip, "_", end_year_precip, ".csv"))
write.csv(district_level_df_onset, district_onset_output_file, row.names = FALSE)


check_na_proportion <- district_level_df_onset %>% dplyr::filter(year >= 1940 & year <= 2024) %>% 
  group_by(pc11_s_id, pc11_d_id, d_name, s_name) %>%
  summarise(mean_proportion_na = mean(proportion_na, na.rm = TRUE)) %>%
  ungroup()

# Number of rows with mean_proportion_NA > 0
num_with_na <- check_na_proportion %>%
  dplyr::filter(mean_proportion_na > 0) %>%
  nrow()

num_with_na2 <- check_na_proportion %>%
  dplyr::filter(mean_proportion_na > 0.01) %>%
  nrow()

row_with_na2 <- check_na_proportion %>%
  dplyr::filter(mean_proportion_na > 0.01) 
row_with_na2 %>% distinct(s_name)

total_rows <- nrow(check_na_proportion)

cat("Rows with mean_proportion_NA > 0:", num_with_na, "\n") # 178
cat("Rows with mean_proportion_NA > 0.01:", num_with_na2, "\n") # 88
cat("Total rows:", total_rows, "\n") # 641 


##### Check with Maps #####
precip_data <- read.csv(output_file_onset) %>% dplyr::filter(year >= start_year_precip & year <= 2024)
precip_data$onset_date4_p <- as.Date(precip_data$onset_date4_p, format="%Y-%m-%d")

# Convert onset_date4_p to days since April 1st of each year
precip_data$day_of_year <- as.numeric(format(precip_data$onset_date4_p, "%j"))
april_1_day <- as.numeric(format(as.Date(paste0(precip_data$year, "-04-01")), "%j"))
precip_data$onset_day_since_april_1 <- precip_data$day_of_year - april_1_day

df_sf <- st_as_sf(precip_data, coords = c("lon", "lat"), crs = 4326)
india_map <- ne_countries(scale = "medium", country = "India", returnclass = "sf")
india_states <- ne_states(country = "India", returnclass = "sf")

# Map of mean onset date (relative to April 1st)
mean_onset <- precip_data %>% group_by(lat, lon) %>% summarise(mean_onset = mean(onset_day_since_april_1, na.rm = TRUE), .groups = "drop")
mean_onset_sf <- st_as_sf(mean_onset, coords = c("lon", "lat"), crs = 4326)

#gray90
mean_onset_cont <- ggplot() +
  geom_sf(data = india_map, fill = NA, color = "black", alpha = 0.5) +
  geom_sf(data = mean_onset_sf, aes(color = mean_onset), size = 0.9) +
  geom_sf(data = india_states, fill = NA, color = "black", size = 0.6) +
  scale_color_viridis_c(name = "Mean Onset (1940-2024, Days since Apr 1)") +
  coord_sf(xlim = c(65, 100), ylim = c(5, 40), expand = FALSE) +
  theme_minimal()
ggsave(file.path(figure_data_dir, paste0("precip_grid_onset_mean_april_", start_year_precip, "_2024", ".png")), plot = mean_onset_cont, width = 10, height = 6, dpi = 300)

# Map of standard deviation of onset date (relative to its mean onset per grid)
# sd_onset <- precip_data %>% 
#   group_by(lat, lon) %>% 
#   mutate(deviation = onset_day_since_april_1 - mean(onset_day_since_april_1, na.rm = TRUE)) %>% 
#   summarise(sd_onset = sd(deviation, na.rm = TRUE), .groups = "drop") 
sd_onset <- precip_data %>% 
  group_by(lat, lon) %>% 
  summarise(sd_onset = sd(onset_day_since_april_1, na.rm = TRUE), .groups = "drop")


sd_onset_sf <- st_as_sf(sd_onset, coords = c("lon", "lat"), crs = 4326)

sd_onset_cont <- ggplot() +
  geom_sf(data = india_map, fill = NA, color = "black", alpha = 0.5) +
  geom_sf(data = sd_onset_sf, aes(color = sd_onset), size = 0.9) +
  geom_sf(data = india_states, fill = NA, color = "black", size = 0.6) +
  scale_color_viridis_c(name = "SD Onset (1940-2024, Days from Mean Onset)") +
  coord_sf(xlim = c(65, 100), ylim = c(5, 40), expand = FALSE) +
  theme_minimal()
ggsave(file.path(figure_data_dir, paste0("precip_grid_onset_sd_april_", start_year_precip, "_2024", ".png")), plot = sd_onset_cont, width = 10, height = 6, dpi = 300)



##### Replication #####
precip_data2 <- read.csv(output_file_onset) %>% dplyr::filter(year >= 1979 & year <= 2019)

precip_data2$onset_date4_p <- as.Date(precip_data2$onset_date4_p, format="%Y-%m-%d")

# Convert onset_date4_p to days since May 1st of each year
precip_data2$day_of_year <- as.numeric(format(precip_data2$onset_date4_p, "%j"))
may_1_day <- as.numeric(format(as.Date(paste0(precip_data2$year, "-05-01")), "%j"))
precip_data2$onset_day_since_may_1 <- precip_data2$day_of_year - may_1_day

df_sf2 <- st_as_sf(precip_data2, coords = c("lon", "lat"), crs = 4326)
india_map <- ne_countries(scale = "medium", country = "India", returnclass = "sf")
india_states <- ne_states(country = "India", returnclass = "sf")

# Map of mean onset date (relative to May 1st)
mean_onset2 <- precip_data2 %>% group_by(lat, lon) %>% summarise(mean_onset = mean(onset_day_since_may_1, na.rm = TRUE), .groups = "drop")
mean_onset_sf2 <- st_as_sf(mean_onset2, coords = c("lon", "lat"), crs = 4326)

mean_onset_sf2$color_label <- cut(mean_onset_sf2$mean_onset,
                                 breaks = c(-Inf, 0, 9, 17, 26, 34, 43, 51, 60, 69, 78, 87, Inf),
                                 labels = c("April 1-30", "May 1-9", "May 10-17", "May 18-26", "May 27-June 4", "June 5-13", "June 14-21", "June 22-30", "July 1-9", "July 10-17", "July 18-27", "After July 28"),
                                 include.lowest = TRUE, right = FALSE
)

color_palette <- c("purple", "#08306b", "#084594", "#2171b5", "#4292c6", "#6baed6", "#9ecae1","#add8e6","#c6dbef", "#deebf7", "#f7fbff", "yellow")


mean_onset_79_19 <- ggplot() +
  geom_sf(data = india_map, fill = NA, color = "black", alpha = 0.5) +
  geom_sf(data = mean_onset_sf2, aes(color = color_label), size = 0.9) +
  geom_sf(data = india_states, fill = NA, color = "black", size = 0.6) +
  scale_color_manual(name = "Mean Onset (1979-2019, Days since May 1)", values = color_palette) +
  coord_sf(xlim = c(65, 100), ylim = c(5, 40), expand = FALSE) +
  theme_minimal()
ggsave(file.path(figure_data_dir, "precip_grid_onset_mean_may_1979_2019.png"), plot = mean_onset_79_19, width = 10, height = 6, dpi = 300)

# Map of standard deviation of onset date (relative to May 1st) #
sd_onset2 <- precip_data2 %>%
  group_by(lat, lon) %>%
  summarise(sd_onset = sd(onset_day_since_may_1, na.rm = TRUE), .groups = "drop")

sd_onset_sf2 <- st_as_sf(sd_onset2, coords = c("lon", "lat"), crs = 4326)

sd_onset_sf2$color_label <- cut(sd_onset_sf2$sd_onset,
                               breaks = c(-Inf, 10.7, 13.0, 14.6, 17.3, Inf),
                               labels = c("< 10.7", "10.8-13.0", "13.0-14.6", "14.7-17.3", "> 17.4"),
                               include.lowest = TRUE, right = FALSE
)

brown_palette <- c("#f5deb3", "#d2b48c", "#a0522d", "#8b4513", "#654321")

sd_onset_79_19 <- ggplot() +
  geom_sf(data = india_map, fill = NA, color = "black", alpha = 0.5) +
  geom_sf(data = sd_onset_sf2, aes(color = color_label), size = 0.9) +
  geom_sf(data = india_states, fill = NA, color = "black", size = 0.6) +
  scale_color_manual(name = "SD Onset (1979-2019, Days from Mean)", values = brown_palette) +
  coord_sf(xlim = c(65, 100), ylim = c(5, 40), expand = FALSE) +
  theme_minimal()
ggsave(file.path(figure_data_dir, "precip_grid_onset_sd_may_1979_2019.png"), plot = sd_onset_79_19, width = 10, height = 6, dpi = 300)

# Map of average total rainfall (1997-2019)
avg_tp2 <- precip_data2 %>% group_by(lat, lon) %>% summarise(avg_tp = mean(yearly_sum_tp, na.rm = TRUE), .groups = "drop")
avg_tp_sf2 <- st_as_sf(avg_tp2, coords = c("lon", "lat"), crs = 4326)

avg_tp_sf2$color_label <- cut(avg_tp_sf2$avg_tp,
                              breaks = c(0, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2, 3, 4, 5, 6, 8, 10, Inf),
                              labels = c("0–0.25", "0.25–0.5", "0.5–0.75", "0.75–1", "1–1.25", "1.25–1.5",
                                         "1.5–1.75", "1.75–2", "2–3", "3–4", "4–5", "5–6", "6–8", "8–10", "10+"),
                              include.lowest = TRUE, right = FALSE
)

blue_palette <- c(
  "#f7fbff", "#deebf7", "#c6dbef", "#9ecae1", "#6baed6",
  "#4292c6", "#2171b5", "#08519c", "#08306b", "#081d58",
  "#061646", "#041a4a", "#031433", "#02102b", "#010a1a"
)
#blue_palette <- c("#f7fbff", "#c6dbef", "#6baed6", "#3182bd", "#08519c", "#08306b", "#081d58", "#041a4a", "#02102b")
#blue_palette <- c("#f7fbff", "#deebf7", "#c6dbef", "#9ecae1", "#6baed6", "#4292c6", "#2171b5", "#08519c", "#08306b")

total_rainfall_79_19 <- ggplot() +
  geom_sf(data = india_map, fill = NA, color = "black", alpha = 0.5) +
  geom_sf(data = avg_tp_sf2, aes(color = color_label), size = 0.9) +
  geom_sf(data = india_states, fill = NA, color = "black", size = 0.6) +
  scale_color_manual(name = "Avg Total Rainfall (1979-2019, m)", values = blue_palette) +
  coord_sf(xlim = c(65, 100), ylim = c(5, 40), expand = FALSE) +
  theme_minimal()
ggsave(file.path(figure_data_dir, "precip_grid_yearly_mean_1979_2019.png"), plot = total_rainfall_79_19, width = 10, height = 6, dpi = 300)


##### 1979-2024 #####
precip_data3 <- read.csv(output_file_onset) %>% dplyr::filter(year >= 1940 & year <= 2024)

# Ensure onset_date4_p is in Date format
precip_data3$onset_date4_p <- as.Date(precip_data3$onset_date4_p, format="%Y-%m-%d")

# Convert onset_date4_p to days since May 1st of each year
precip_data3$day_of_year <- as.numeric(format(precip_data3$onset_date4_p, "%j"))
may_1_day <- as.numeric(format(as.Date(paste0(precip_data3$year, "-05-01")), "%j"))
precip_data3$onset_day_since_may_1 <- precip_data3$day_of_year - may_1_day

df_sf3 <- st_as_sf(precip_data3, coords = c("lon", "lat"), crs = 4326)
india_map <- ne_countries(scale = "medium", country = "India", returnclass = "sf")
india_states <- ne_states(country = "India", returnclass = "sf")

# Map of mean onset date (relative to May 1st)
mean_onset3 <- precip_data3 %>% group_by(lat, lon) %>% summarise(mean_onset = mean(onset_day_since_may_1, na.rm = TRUE), .groups = "drop")
mean_onset_sf3 <- st_as_sf(mean_onset2, coords = c("lon", "lat"), crs = 4326)

mean_onset_sf3$color_label <- cut(mean_onset_sf3$mean_onset,
                                  breaks = c(-Inf, 0, 9, 17, 26, 34, 43, 51, 60, 69, 78, 87, Inf),
                                  labels = c("April 1-30", "May 1-9", "May 10-17", "May 18-26", "May 27-June 4", "June 5-13", "June 14-21", "June 22-30", "July 1-9", "July 10-17", "July 18-27", "After July 28"),
                                  include.lowest = TRUE, right = FALSE
)

color_palette <- c("purple", "#08306b", "#084594", "#2171b5", "#4292c6", "#6baed6", "#9ecae1","#add8e6","#c6dbef", "#deebf7", "#f7fbff", "yellow")


mean_onset_40_24 <- ggplot() +
  geom_sf(data = india_map, fill = NA, color = "black", alpha = 0.5) +
  geom_sf(data = mean_onset_sf3, aes(color = color_label), size = 0.9) +
  geom_sf(data = india_states, fill = NA, color = "black", size = 0.6) +
  scale_color_manual(name = "Mean Onset (1940-2024, Days since May 1)", values = color_palette) +
  coord_sf(xlim = c(65, 100), ylim = c(5, 40), expand = FALSE) +
  theme_minimal()
ggsave(file.path(figure_data_dir, "precip_grid_onset_mean_may_1940_2024.png"), plot = mean_onset_40_24, width = 10, height = 6, dpi = 300)

# Map of standard deviation of onset date (relative to May 1st) #
sd_onset3 <- precip_data3 %>%
  group_by(lat, lon) %>%
  summarise(sd_onset = sd(onset_day_since_may_1, na.rm = TRUE), .groups = "drop")

sd_onset_sf3 <- st_as_sf(sd_onset3, coords = c("lon", "lat"), crs = 4326)

sd_onset_sf3$color_label <- cut(sd_onset_sf3$sd_onset,
                                breaks = c(-Inf, 10.7, 13.0, 14.6, 17.3, Inf),
                                labels = c("< 10.7", "10.8-13.0", "13.0-14.6", "14.7-17.3", "> 17.4"),
                                include.lowest = TRUE, right = FALSE
)

brown_palette <- c("#f5deb3", "#d2b48c", "#a0522d", "#8b4513", "#654321")

sd_onset_40_24 <- ggplot() +
  geom_sf(data = india_map, fill = NA, color = "black", alpha = 0.5) +
  geom_sf(data = sd_onset_sf3, aes(color = color_label), size = 0.9) +
  geom_sf(data = india_states, fill = NA, color = "black", size = 0.6) +
  scale_color_manual(name = "SD Onset (1940-2024, Days from Mean)", values = brown_palette) +
  coord_sf(xlim = c(65, 100), ylim = c(5, 40), expand = FALSE) +
  theme_minimal()
ggsave(file.path(figure_data_dir, "precip_grid_onset_sd_may_1940_2024.png"), plot = sd_onset_40_24, width = 10, height = 6, dpi = 300)

# Map of average total rainfall (1940-2024)
avg_tp3 <- precip_data3 %>% group_by(lat, lon) %>% summarise(avg_tp = mean(yearly_sum_tp, na.rm = TRUE), .groups = "drop")
avg_tp_sf3 <- st_as_sf(avg_tp3, coords = c("lon", "lat"), crs = 4326)

avg_tp_sf3$color_label <- cut(avg_tp_sf3$avg_tp,
                              breaks = c(0, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2, 3, 4, 5, 6, 8, 10, Inf),
                              labels = c("0–0.25", "0.25–0.5", "0.5–0.75", "0.75–1", "1–1.25", "1.25–1.5",
                                         "1.5–1.75", "1.75–2", "2–3", "3–4", "4–5", "5–6", "6–8", "8–10", "10+"),
                              include.lowest = TRUE, right = FALSE
)

blue_palette <- c(
  "#f7fbff", "#deebf7", "#c6dbef", "#9ecae1", "#6baed6",
  "#4292c6", "#2171b5", "#08519c", "#08306b", "#081d58",
  "#061646", "#041a4a", "#031433", "#02102b", "#010a1a"
)

total_rainfall_40_24 <- ggplot() +
  geom_sf(data = india_map, fill = NA, color = "black", alpha = 0.5) +
  geom_sf(data = avg_tp_sf3, aes(color = color_label), size = 0.9) +
  geom_sf(data = india_states, fill = NA, color = "black", size = 0.6) +
  scale_color_manual(name = "Avg Total Rainfall (1940-2024, m)", values = blue_palette) +
  coord_sf(xlim = c(65, 100), ylim = c(5, 40), expand = FALSE) +
  theme_minimal()
ggsave(file.path(figure_data_dir, "precip_grid_yearly_mean_1940_2024.png"), plot = total_rainfall_40_24, width = 10, height = 6, dpi = 300)



##### Lethe : Clear memory #####

rm(monsoon_onset_dates_list)
rm(monsoon_onset_all)
gc()



##### Process Temperature Data #####
start_year_temp <- 1940
end_year_temp <- 2025
temp_years <- start_year_temp:end_year_temp
temperature_list <- list()

leap_year <- function(year) {
  return((year %% 4 == 0 & year %% 100 != 0) | (year %% 400 == 0))
}

for (year in temp_years) {
  file_path <- file.path(raw_data_dir, paste0("era5_india_temperature_", year, ".nc"))
  
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
                    "20_25_C", "25_30_C", "30_35_C", "35_over_C")
    
    df$category <- cut(df$temp_celsius, 
                       breaks = c(-Inf, 0, 5, 10, 15, 20, 25, 30, 35, Inf), 
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
    
    # month_cols <- if (year == 2025) paste0("m", 1:3) else paste0("m", 1:12)
    # if (length(month_cols) == (ncol(monthly_mean_pivot) - 2)) {
    #   names(monthly_mean_pivot)[3:ncol(monthly_mean_pivot)] <- month_cols
    # } else {
    #   warning("Mismatch between expected and actual month columns. Column renaming skipped.")
    # }
    # 
    
    # assign(paste0("temp_month_", year), monthly_mean, envir = .GlobalEnv) 
    # assign(paste0("temp_month_pivot_", year), monthly_mean_pivot, envir = .GlobalEnv)        
    
    month_cols <- if (year == 2025) paste0("m", 1:3) else paste0("m", 1:12)
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
    month_range <- if (year == 2025) 1:3 else 1:12        
    
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
    if (year == 2025) {
      bin_order <- setdiff(bin_order, "35_over_C")
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
                                       "20_25_C", "25_30_C", "30_35_C", "35_over_C"))
    
    
    combined_file <- file.path(temp_data_dir, paste0("grid_temp_stat_", year, ".csv"))
    write.csv(combined_data, combined_file, row.names = FALSE)

    check_count <- sum(combined_data$check == 1)
    message(paste0("Number of grids with incomplete data for year ", year, ": ", check_count))

    temperature_list[[as.character(year)]] <- combined_data # list(original = df, combined = combined_data)

    # df_sf <- st_as_sf(combined_data, coords = c("lon", "lat"), crs = 4326)
    # india_map <- ne_countries(scale = "medium", country = "India", returnclass = "sf")
    # india_states <- ne_states(country = "India", returnclass = "sf")
    # 
    # p1 <- ggplot() +
    #   geom_sf(data = india_map, fill = "gray90", color = "black", alpha = 0.5) +
    #   geom_sf(data = df_sf, aes(color = yearly_mean), size = 0.9) +
    #   scale_color_gradientn(colors = c("blue", "white", "yellow", "red"), 
    #                         values = scales::rescale(c(-10, 5, 20, 35)), 
    #                         name = "Yearly Mean Temp (°C)") +
    #   # scale_color_gradient2(low = "blue", mid = "yellow", high = "red", midpoint = 10, name = "Yearly Mean Temp (°C)") +      
    #   # scale_color_gradient(low = "yellow", high = "red", name = "Yearly Mean Temp (°C)") +
    #   geom_sf(data = india_states, fill = NA, color = "black", size = 0.6) +
    #   theme_minimal() +
    #   labs(title = paste("Yearly Mean Temperature -", year))
    # 
    # ggsave(file.path(figure_data_dir, paste0("temp_yearly_mean_", year, ".png")), p1)
    # 
    # if (year != 2025) {
    #   p2 <- ggplot() +
    #     geom_sf(data = india_map, fill = "gray90", color = "black", alpha = 0.5) +
    #     geom_sf(data = df_sf, aes(color = `35_over_C`), size = 0.9) +
    #     scale_color_gradient2(low = "green", mid = "yellow", high = "red", midpoint = 28, name = "Yearly Mean Temp (°C)") +      
    #     # scale_color_gradientn(colors = c("green", "yellow", "red"), name = "Days in 35°C+ bin") +      
    #     # scale_color_gradient(low = "yellow", high = "red", name = "Days in 35°C+ bin") +
    #     geom_sf(data = india_states, fill = NA, color = "black", size = 0.6) +
    #     theme_minimal() +
    #     labs(title = paste("Days in 35°C+ Bin -", year))
    #   
    #   ggsave(file.path(figure_data_dir, paste0("temp_hottest_bin_", year, ".png")), p2)
    # }    
    # 
    
    nc_close(nc_data)
    gc()
    
  } else {
    message("File not found: ", file_path)
  }
}

temperature_all <- bind_rows(temperature_list)
# temperature_all <- temperature_all %>%
#   rename_with(~ paste0("t_", .), c("below_0_C", "0_5_C", "5_10_C", "10_15_C", "15_20_C", 
#                                    "20_25_C", "25_30_C", "30_35_C", "35_over_C"))


output_file_temp <- file.path(clean_data_dir, paste0("grid_temp_stat_", start_year_temp, "_", end_year_temp, ".csv"))
write.csv(temperature_all, output_file_temp, row.names = FALSE)
message("Saved combined temperature stat data to: ", output_file_temp)


# start_year_temp <- 1940
# end_year_temp <- 2025
# 
# years <- start_year_temp:end_year_temp
# file_paths <- file.path(temp_data_dir, paste0("grid_temp_stat_", years, ".csv"))
# 
# temperature_list <- lapply(file_paths, function(file) {
#   tryCatch({
#     read.csv(file)
#   }, error = function(e) {
#     warning(paste("Error reading:", file))
#     NULL
#   })
# })
# 
# temperature_all <- bind_rows(temperature_list)
# 
# output_file_temp <- file.path(clean_data_dir, paste0("grid_temp_stat_", start_year_temp, "_", end_year_temp, ".csv"))
# write.csv(temperature_all, output_file_temp, row.names = FALSE)


temperature_all <- read_csv(output_file_temp)

grid_district_weights <- read_csv(file.path(clean_data_dir,paste0("grid_district_weights_key.csv")))
grid_district_weights_check <- grid_district_weights %>% arrange(grid_id, weight)
grid_year_expanded <- grid_district_weights %>%
  mutate(
    pc11_d_id = as.character(pc11_d_id)
  ) %>%
  tidyr::expand(nesting(grid_id, lon, lat, pc11_d_id, intersection_area, weight), year = years) %>% 
  arrange(grid_id, lon, lat, year, weight)


grid_district_temp_data <- temperature_all %>%
  dplyr::left_join(grid_year_expanded, by = c("lon", "lat", "year")) %>%
  dplyr::filter(weight > 0) %>%
  dplyr::left_join(
    district_sf %>%
      st_drop_geometry() %>%
      dplyr::select(pc11_d_id, pc11_s_id, d_name, district_area),
    by = "pc11_d_id"
  ) %>%
  dplyr::left_join(state_codebook, by = "pc11_s_id") %>%
  dplyr::select(pc11_d_id, lat, lon, year, dplyr::everything(), -grid_id) %>% 
  mutate(area_weight = intersection_area/district_area) 


grid_temp_data_check <-grid_district_temp_data %>% dplyr::filter(is.na(pc11_s_id))

output_file_temp_with_district <- file.path(clean_data_dir, paste0("grid_temp_stat_with_district_", start_year_temp, "_", end_year_temp, ".csv"))
write.csv(grid_district_temp_data, output_file_temp_with_district, row.names = FALSE)
message("Saved temperature data with district information to: ", output_file_temp_with_district)




##### District-level dataset for temperature ####
df_sf_temp_drop <- read.csv(output_file_temp_with_district) 
sapply(df_sf_temp_drop[, c("pc11_s_id", "pc11_d_id", "d_name", "s_name")], function(x) sum(is.na(x)))


names(df_sf_temp_drop)[names(df_sf_temp_drop) == "yearly_mean"] <- "yearly_mean_temp"
t_monthly_bin_vars <- grep("^t_m\\d+_", names(df_sf_temp_drop), value = TRUE)

temp_vars <- c(
  "yearly_mean_temp",
  paste0("m", 1:12),
  "t_below_0_C", "t_0_5_C", "t_5_10_C", "t_10_15_C", "t_15_20_C",
  "t_20_25_C", "t_25_30_C", "t_30_35_C", "t_35_over_C",
  t_monthly_bin_vars
)


bin_cols <- c(
  "t_below_0_C", "t_0_5_C", "t_5_10_C", "t_10_15_C", "t_15_20_C",
  "t_20_25_C", "t_25_30_C", "t_30_35_C", "t_35_over_C"
)


df_temp_filtered <- df_sf_temp_drop %>%
  dplyr::filter(complete.cases(pc11_s_id, pc11_d_id))

district_temp_df <- df_temp_filtered %>%
  group_by(pc11_s_id, pc11_d_id, d_name, s_name, year) %>%
  summarise(
    # Weighted mean using 'weight'
    across(all_of(temp_vars), 
           ~ weighted.mean(.x, w = weight, na.rm = TRUE), 
           .names = "mean_{.col}"),
    
    # Weighted mean using 'area_weight'
    across(all_of(temp_vars), 
           ~ weighted.mean(.x, w = area_weight, na.rm = TRUE), 
           .names = "area_mean_{.col}"),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    # Sum of bin values for weight-weighted means
    sum_bins_mean = round(sum(c_across(paste0("mean_", bin_cols)), na.rm = TRUE), 2),
    
    # Sum of bin values for area_weight-weighted means
    sum_bins_area_mean = round(sum(c_across(paste0("area_mean_", bin_cols)), na.rm = TRUE), 2),
    
    is_leap = (year >= 1940 & year <= 2025) & 
      ((year %% 4 == 0 & year %% 100 != 0) | (year %% 400 == 0)),
    
    check = ifelse(is_leap, sum_bins_area_mean == 366, sum_bins_area_mean == 365)
  ) %>%
  ungroup() %>%
  dplyr::select(-is_leap)

district_level_output_file_temp <- file.path(clean_data_dir,paste0("district_temperature_", start_year_temp, "_", end_year_temp, ".csv"))
write.csv(district_temp_df, district_level_output_file_temp, row.names = FALSE)




##### Check with Maps #####
temp_data <- read.csv(output_file_temp) %>% dplyr::filter(year >= start_year_temp & year <= 2024)

df_sf <- st_as_sf(temp_data, coords = c("lon", "lat"), crs = 4326)
india_map <- ne_countries(scale = "medium", country = "India", returnclass = "sf")
india_states <- ne_states(country = "India", returnclass = "sf")

# Map of average temperature over all years for each grid cell
temp_mean_all <- temp_data %>%
  group_by(lat, lon) %>%
  summarise(avg_temp = mean(yearly_mean, na.rm = TRUE))

df_sf_mean_all <- st_as_sf(temp_mean_all, coords = c("lon", "lat"), crs = 4326)

p_avg_all <- ggplot() +
  geom_sf(data = india_map, fill = "gray90", color = "black", alpha = 0.5) +
  geom_sf(data = df_sf_mean_all, aes(color = avg_temp), size = 0.9) +
  scale_color_gradientn(colors = c("blue", "white", "yellow", "red"), 
                        values = scales::rescale(c(-10, 5, 20, 35)), 
                        name = "Avg Temp (°C)") +
  geom_sf(data = india_states, fill = NA, color = "black", size = 0.6) +
  theme_minimal() +
  labs(title = "Average Temperature (All Years : 1940-2024)")

ggsave(file.path(figure_data_dir, paste0("temp_grid_mean_all_", start_year_temp, "_2024.png")), p_avg_all, width = 10, height = 6, dpi = 300)

# Map of average temperature in randomly chosen years
random_years <- sample(unique(temp_data$year), 5)
temp_random <- temp_data %>% dplyr::filter(year %in% random_years) %>%
  group_by(lat, lon) %>%
  summarise(avg_temp = mean(yearly_mean, na.rm = TRUE))

df_sf_random <- st_as_sf(temp_random, coords = c("lon", "lat"), crs = 4326)

p_avg_random <- ggplot() +
  geom_sf(data = india_map, fill = "gray90", color = "black", alpha = 0.5) +
  geom_sf(data = df_sf_random, aes(color = avg_temp), size = 0.9) +
  scale_color_gradientn(colors = c("blue", "white", "yellow", "red"), 
                        values = scales::rescale(c(-10, 5, 20, 35)), 
                        name = "Avg Temp (°C)") +
  geom_sf(data = india_states, fill = NA, color = "black", size = 0.6) +
  theme_minimal() +
  labs(title = "Average Temperature (Random 5 Years)")

ggsave(file.path(figure_data_dir, paste0("temp_grid_mean_random_", start_year_temp, "_2024.png")), p_avg_random, width = 10, height = 6, dpi = 300)

# Map of mean number of days in the hottest bin over all years
temp_hottest_all <- temp_data %>%
  group_by(lat, lon) %>%
  summarise(avg_hottest_days = mean(t_35_over_C, na.rm = TRUE))

df_sf_hottest_all <- st_as_sf(temp_hottest_all, coords = c("lon", "lat"), crs = 4326)

p_hottest_all <- ggplot() +
  geom_sf(data = india_map, fill = "gray90", color = "black", alpha = 0.5) +
  geom_sf(data = df_sf_hottest_all, aes(color = avg_hottest_days), size = 0.9) +
  scale_color_gradientn(colors = c("green", "yellow", "red"), 
                        name = "Avg Days in 35°C+ Bin") +
  geom_sf(data = india_states, fill = NA, color = "black", size = 0.6) +
  theme_minimal() +
  labs(title = "Mean Number of Days in 35°C+ Bin (All Years)")

ggsave(file.path(figure_data_dir, paste0("temp_grid_hottest_bin_mean_all_", start_year_temp, "_2024.png")), p_hottest_all, width = 10, height = 6, dpi = 300)

# Map of mean number of days in the hottest bin for chosen years
temp_hottest_chosen <- temp_data %>% dplyr::filter(year %in% c(2016, 2019, 2020, 2023, 2024)) %>%
  group_by(lat, lon) %>%
  summarise(avg_hottest_days = mean(t_35_over_C, na.rm = TRUE))

df_sf_hottest_chosen <- st_as_sf(temp_hottest_chosen, coords = c("lon", "lat"), crs = 4326)

p_hottest_chosen <- ggplot() +
  geom_sf(data = india_map, fill = "gray90", color = "black", alpha = 0.5) +
  geom_sf(data = df_sf_hottest_chosen, aes(color = avg_hottest_days), size = 0.9) +
  scale_color_gradientn(colors = c("green", "yellow", "red"), 
                        name = "Avg Days in 35°C+ Bin") +
  geom_sf(data = india_states, fill = NA, color = "black", size = 0.6) +
  theme_minimal() +
  labs(title = paste("Mean Number of Days in 35°C+ Bin (2016, 2019, 2020, 2023, 2024)"))  # Use static values
  # labs(title = paste("Mean Number of Days in 35°C+ Bin (", paste(unique(temp_hottest_chosen$year), collapse=", "), ")"))

ggsave(file.path(figure_data_dir, paste0("temp_grid_hottest_bin_chosen_all_", start_year_temp, "_2024.png")), p_hottest_chosen, width = 10, height = 6, dpi = 300)


# Select 20 random grids within the defined lon-lat range
set.seed(123)
selected_grids <- temp_data %>%
  dplyr::filter(lon >= 80 & lon <= 90, lat >= 20 & lat <= 35) %>%
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
    labs(title = paste("Yearly Mean Temperature at", grid_lat, "°N,", grid_lon, "°E"),
         x = "Year", y = "Yearly Mean Temp (°C)")
  
  ggsave(file.path(figure_data_dir, paste0("temp_grid_mean_year_", grid_lat, "_", grid_lon, "_", start_year_temp, "_2024.png")), p_yearly_mean, width = 10, height = 6, dpi = 300)
  
  # Yearly trend of hottest bin days
  p_hottest_bin <- ggplot(grid_data, aes(x = year, y = t_35_over_C)) +
    geom_line(color = "red") +
    geom_point(color = "red") +
    theme_minimal() +
    labs(title = paste("Hottest Bin Days at", grid_lat, "°N,", grid_lon, "°E"),
         x = "Year", y = "Days in 35°C+ Bin")
  
  ggsave(file.path(figure_data_dir, paste0("temp_grid_hottest_bin_", grid_lat, "_", grid_lon, "_", start_year_temp, "_2024.png")), p_hottest_bin, width = 10, height = 6, dpi = 300)
  
  # Monthly trend of average temperature
  grid_data_long <- grid_data %>%
    dplyr::select(year, starts_with("m")) %>%
    pivot_longer(cols = starts_with("m"), names_to = "month", values_to = "monthly_temp") %>%
    mutate(month_num = as.numeric(sub("m", "", month)),
           date = as.Date(paste(year, month_num, "01", sep = "-")))
  
  p_monthly_trend <- ggplot(grid_data_long, aes(x = date, y = monthly_temp)) +
    geom_line(color = "darkorange") +
    theme_minimal() +
    labs(title = paste("Monthly Temperature Trend at", grid_lat, "°N,", grid_lon, "°E"),
         x = "Date", y = "Avg Temp (°C)")
  
  ggsave(file.path(figure_data_dir, paste0("temp_grid_mean_month_", grid_lat, "_", grid_lon, "_", start_year_temp, "_2024.png")), p_monthly_trend, width = 10, height = 6, dpi = 300)
}





