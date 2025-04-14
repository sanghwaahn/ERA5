#####Combine#####
package_tmp <- c("readxl", "openxlsx", "dplyr", "stringr", 
                 "tidyr", "readxl", "writexl", "lubridate", "tidyverse",
                 "sp", "ncdf4", "raster", "here", "zoo", "sf",
                 "viridis", "rnaturalearth", "rnaturalearthdata", "rnaturalearthhires", 
                 "stringdist", "fuzzyjoin", "haven", "Hmisc")
packages <- unique(package_tmp)
lapply(packages, library, character.only = TRUE)
'%!in%' <- function(x,y)!('%in%'(x,y))

user_path <- "/Users/sanghwa/Library/CloudStorage/Box-Box/Monsoon_onset"

clean_data_era5 <- file.path(user_path, "data", "era5", "clean")
clean_data_apy <- file.path(user_path, "data", "crop_apy", "clean")
apy_downloaded <- file.path(user_path, "data", "crop_apy", "raw_from_web")
district_match_temp_dir <- file.path(user_path, "data", "district_match")
final_dir <- file.path(user_path, "data", "final")


df_onset <- read.csv(file.path(clean_data_era5, "district_monsoon_onset_dates_1940_2025.csv"))
df_temp <- read.csv(file.path(clean_data_era5, "district_temperature_1940_2025.csv")) 
df_apy <- read.csv(file.path(clean_data_apy, "apy_all_clean.csv"))
df_apy_downloaded <- read.csv(file.path(apy_downloaded, "apy_downloaded.csv"))

df_onset_prev <- read.csv(file.path(clean_data_era5, "district_monsoon_onset_dates_1940_2025_prev.csv"))

onset_check1 <- df_onset %>% dplyr::filter(is.na(mean_onset_date4_p_final)) #45985 -> 54485
onset_check2 <- df_onset_prev %>% dplyr::filter(is.na(mean_onset_date4_p_final)) #54485

df_apy <- df_apy %>%
  mutate(
    Crop = str_remove(Crop, "^\\s*\\d+\\.*\\s*"),
    District = str_remove(District, "^\\s*\\d+\\.*\\s*")
  ) %>%
  dplyr::filter(!str_detect(District, "Total \\("))

df_onset_check <- df_onset %>% arrange("pc11_d_id")


merge_keys <- c("pc11_s_id", "pc11_d_id", "d_name", "s_name", "year")
df_onset$d_name[is.na(df_onset$d_name)] <- ""
df_temp$d_name[is.na(df_temp$d_name)] <- ""
df_onset[merge_keys] <- lapply(df_onset[merge_keys], as.character)
df_temp[merge_keys] <- lapply(df_temp[merge_keys], as.character)

# only difference : 2025 data in temperature
df_climate <- left_join(df_temp, df_onset, by = merge_keys)
onset_only_cols <- setdiff(names(df_onset), merge_keys)
df_unmatched <- df_climate %>%
  dplyr::filter(if_all(all_of(onset_only_cols), is.na))
#df_climate <- merge(df_onset, df_temp, by = merge_keys, all = FALSE)


# check_district <- df_climate %>% dplyr::filter(d_name == "Kamrup metropolitan")

df_climate <- df_climate %>%
  mutate(
    d_name = str_trim(as.character(d_name)),
    s_name = str_trim(as.character(s_name)),
    year = as.integer(year)
  )


df_apy <- df_apy %>%
  mutate(
    District = str_trim(as.character(District)),
    State = str_trim(as.character(State)),
    Start_Year = as.integer(Start_Year)
  )


## From here : Making District-District Matchbook (You can skip running this part) ##

apy_unmatched <- anti_join(
  df_apy,
  df_climate,
  by = c("District" = "d_name", "State" = "s_name", "Start_Year" = "year")
)


print(apy_unmatched)
unmatch_list <- apy_unmatched %>% distinct(District, State)


district_list_apy <- df_apy %>%
  distinct(District, State) %>%
  dplyr::select(State, District) %>%
  arrange(State, District)

district_list_era5 <- df_climate %>%
  distinct(d_name, s_name) %>%
  dplyr::select(s_name, d_name) %>%
  arrange(s_name, d_name)



dist_apy <- file.path(district_match_temp_dir, "district_list_apy.xlsx")
dist_era5 <- file.path(district_match_temp_dir, "district_list_era5.xlsx")
write_xlsx(district_list_apy, path = dist_apy)
write_xlsx(district_list_era5, path = dist_era5)



all_years <- as.character(1997:2022)

apy_wide <- df_apy %>%
  distinct(District, State, Start_Year) %>%
  dplyr::select(State, District, Start_Year) %>%
  arrange(State, District, Start_Year) %>% 
  mutate(State_clean = str_to_lower(str_trim(State)),
         District_clean = str_to_lower(str_trim(District))) %>%
  mutate(value = 1) %>%
  pivot_wider(names_from = Start_Year, values_from = value, values_fill = list(value = 0)) %>%
  arrange(State_clean, District_clean) %>%
  mutate(across(all_of(setdiff(all_years, names(.))), ~0)) %>%
  dplyr::select(State, District, State_clean, District_clean, all_of(all_years))

# fwrite(apy_wide, file.path(district_match_temp_dir,paste0("apy_district_panel_availability.csv"))) # check fwrite
write_xlsx(apy_wide, file.path(district_match_temp_dir,paste0("apy_district_panel_availability.xlsx")))

df_apy_downloaded <- df_apy_downloaded %>% arrange(Crop_Year)
downloaded_years <- as.character(1997:2015)
apy_wide_downloaded <- df_apy_downloaded %>% 
  distinct(District_Name, State_Name, Crop_Year) %>%
  dplyr::select(State_Name, District_Name, Crop_Year) %>%
  arrange(State_Name, District_Name, Crop_Year) %>% 
  mutate(State_clean = str_to_lower(str_trim(State_Name)),
         District_clean = str_to_lower(str_trim(District_Name))) %>%
  mutate(value = 1) %>%
  pivot_wider(names_from = Crop_Year, values_from = value, values_fill = list(value = 0)) %>%
  arrange(State_clean, District_clean) %>%
  mutate(across(all_of(setdiff(downloaded_years, names(.))), ~0)) %>%
  dplyr::select(State_Name, District_Name, State_clean, District_clean, all_of(downloaded_years))  

write_xlsx(apy_wide_downloaded, file.path(district_match_temp_dir,paste0("apy_downloaded_district_panel_availability.xlsx")))


apy_base <- apy_wide 
# %>% dplyr::select(-State, -District)

apy_downloaded_base <- apy_wide_downloaded %>%
  dplyr::select(-State_Name, -District_Name)

joined_df <- left_join(
  apy_base,
  apy_downloaded_base,
  by = c("State_clean", "District_clean"),
  suffix = c("_base", "_dl")
)

for (year in 1997:2015) {
  col_base <- paste0(year, "_base")
  col_dl <- paste0(year, "_dl")
  check_col <- paste0("check_", year)
  
  joined_df[[check_col]] <- case_when(
    joined_df[[col_base]] == 1 & joined_df[[col_dl]] == 1 ~ "Match",
    joined_df[[col_base]] == 0 & joined_df[[col_dl]] == 0 ~ "Match",
    joined_df[[col_base]] == 1 & joined_df[[col_dl]] == 0 ~ "More in the Base",
    joined_df[[col_base]] == 0 & joined_df[[col_dl]] == 1 ~ "More in the Downloaded",
    joined_df[[col_base]] == 1 & is.na(joined_df[[col_dl]]) ~ "Only in the Base",
    joined_df[[col_base]] == 1 & is.na(joined_df[[col_dl]]) ~ "Also empty in the Base",
    TRUE ~ NA_character_
  )
}

write_xlsx(joined_df, file.path(district_match_temp_dir,paste0("apy_combined_district_panel_availability.xlsx"))) # Conclusion : Just use apy_wide

apy_clean <- district_list_apy %>%
  mutate(State_clean = str_to_lower(str_trim(State)),
         District_clean = str_to_lower(str_trim(District)))



era5_clean <- district_list_era5 %>%
  mutate(s_name_clean = str_to_lower(str_trim(s_name)),
         d_name_clean = str_to_lower(str_trim(d_name)))

combo_df <- tidyr::crossing(apy_clean, era5_clean)

combo_df <- combo_df %>%
  mutate(
    state_dist = stringdist(State_clean, s_name_clean, method = "jw"),
    district_dist = stringdist(District_clean, d_name_clean, method = "jw")
  )

fuzzy_matches <- combo_df %>%
  dplyr::filter(state_dist <= 0.1, district_dist <= 0.1) %>%
  arrange(State, District, district_dist, state_dist) %>%
  group_by(State, District) %>%
  slice_min(order_by = district_dist + state_dist, n = 1, with_ties = FALSE) %>%
  ungroup()

fuzzy_unmatched_apy <- anti_join(
  apy_clean,
  fuzzy_matches,
  by = c("State", "District")
) %>%
  dplyr::select(State, District)

fuzzy_unmatched_era5 <- anti_join(
  era5_clean,
  fuzzy_matches,
  by = c("s_name" = "s_name", "d_name" = "d_name")
) %>%
  dplyr::select(s_name, d_name)


fuzzy_second_tier <- combo_df %>%
  dplyr::filter(state_dist <= 0.1, district_dist >= 0.1, district_dist <= 0.2)

fuzzy_second_tier <- anti_join(
  fuzzy_second_tier,
  fuzzy_matches,
  by = c("State", "District")
)

fuzzy_second_tier <- fuzzy_second_tier %>%
  arrange(State, District, district_dist, state_dist) %>%
  group_by(State, District) %>%
  slice_min(order_by = district_dist + state_dist, n = 1, with_ties = FALSE) %>%
  ungroup()



write_xlsx(
  fuzzy_matches %>%
    dplyr::select(State, District, s_name, d_name, state_dist, district_dist),
  path = file.path(district_match_temp_dir, "fuzzy_district_matching.xlsx")
)
write_xlsx(
  fuzzy_unmatched_apy,
  path = file.path(district_match_temp_dir, "fuzzy_district_unmatched_apy.xlsx")
)
write_xlsx(
  fuzzy_unmatched_era5,
  path = file.path(district_match_temp_dir, "fuzzy_district_unmatched_era5.xlsx")
)

write_xlsx(
  fuzzy_second_tier %>%
    dplyr::select(State, District, s_name, d_name, state_dist, district_dist),
  path = file.path(district_match_temp_dir, "fuzzy_district_matching_second.xlsx")
)


year_cols <- as.character(1997:2022)
match_book_df <- left_join(apy_wide, fuzzy_matches, by = c("State", "District", "State_clean", "District_clean")) %>%
  mutate(match_type = NA_character_, formation_time = NA_integer_, memo = NA_character_) %>%
  mutate(match_type = ifelse(!is.na(s_name) & !is.na(d_name), "first match", match_type)) %>%
  rowwise() %>%
  mutate(
    start_data_year = {
      vals <- c_across(all_of(year_cols))
      years <- as.integer(year_cols)[vals == 1]
      if (length(years) > 0) min(years) else NA_integer_
    },
    end_data_year = {
      vals <- c_across(all_of(year_cols))
      years <- as.integer(year_cols)[vals == 1]
      if (length(years) > 0) max(years) else NA_integer_
    }
  ) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(
    empty_mid_year = {
      if (is.na(start_data_year) || is.na(end_data_year)) {
        NA_character_
      } else {
        mid_cols <- as.character(start_data_year:end_data_year)
        vals <- unlist(cur_data()[mid_cols])
        if (any(vals == 0, na.rm = TRUE)) "yes" else "no"
      }
    }
  ) %>%
  ungroup() %>%
  dplyr::select(State, District, s_name, d_name, state_dist, district_dist, match_type, formation_time, start_data_year, end_data_year, empty_mid_year, memo, all_of(year_cols))

write_xlsx(match_book_df, file.path(district_match_temp_dir,paste0("match_book_with_apy_availability.xlsx"))) 





### If you skipped above start from here ###
## Weights and matchbook ##

weights_data_path <- file.path(clean_data_era5, paste0("grid_district_weights_", 1940, "_", 2025, ".csv"))
weights_data <- read.csv(weights_data_path)

unique_weights <- weights_data %>%
  dplyr::select(s_name, d_name, district_area) %>%
  distinct()

matchbook <- read_xlsx(file.path(district_match_temp_dir,paste0("match_book_with_apy_availability_worked.xlsx")))

matchbook <- matchbook %>%
  left_join(unique_weights, by = c("s_name", "d_name"))




simple_match_types <- c("first match", "second match", "third match", "new union territory", "carved out of", "bifurcated into")

simple_matches <- matchbook %>%
  dplyr::filter(match_type %in% simple_match_types)

df_apy_simple <- df_apy %>%
  inner_join(simple_matches %>% dplyr::select(State, District, s_name, d_name, match_type, district_area), 
            by = c("State", "District"))

na_count <- df_apy_simple %>%dplyr::filter(is.na(d_name)) %>% nrow()
print(paste("Number of rows with NA in d_name:", na_count))
na_count2 <- df_apy_simple %>% dplyr::filter(is.na(s_name)) %>% nrow()
print(paste("Number of rows with NA in s_name:", na_count2))
s_name_na_rows <- df_apy_simple %>% dplyr::filter(is.na(s_name))
if (na_count > 0 || na_count2 > 0) {
  stop("NA values detected in s_name or d_name. Exiting the script.")
}




complex_match_types <- c("carved out and combined", "multiple to one match")

complex_matches <- matchbook %>%
  dplyr::filter(match_type %in% complex_match_types)


expanded_df <- df_apy %>%
  inner_join(complex_matches %>% dplyr::select(State, District, s_name, d_name, match_type, district_area),
             by = c("State", "District"),
             relationship = "many-to-many")

expanded_df <- expanded_df %>%
  group_by(State, Crop, District, Season, Start_Year, End_Year, Area, Production, Yield) %>%
  mutate(related_district_sum = sum(district_area, na.rm = TRUE),
         root_district_weight = district_area / related_district_sum) %>%
  ungroup()

expanded_df <- expanded_df %>%
  rename(Area_base = Area, Production_base = Production, Yield_base = Yield) %>%
  mutate(Area = round(Area_base * root_district_weight, 2),
         Production = round(Production_base * root_district_weight, 2),
         Yield = round(Production/Area,2))

expanded_df <- expanded_df %>% 
  dplyr::select(-Area_base, -Production_base, -Yield_base, -related_district_sum, -root_district_weight)

na_count <- expanded_df %>%dplyr::filter(is.na(d_name)) %>% nrow()
print(paste("Number of rows with NA in d_name:", na_count))

na_count2 <- expanded_df %>% dplyr::filter(is.na(s_name)) %>% nrow()
print(paste("Number of rows with NA in s_name:", na_count2))
s_name_na_rows <- expanded_df %>% dplyr::filter(is.na(s_name))
if (na_count > 0 || na_count2 > 0) {
  stop("NA values detected in s_name or d_name. Exiting the script.")
}


final_df <- bind_rows(df_apy_simple, expanded_df)

final_df <- final_df %>%
  arrange(State, District, Crop, Start_Year, End_Year)

colnames(final_df) <- tolower(colnames(final_df))

na_count <- final_df %>%dplyr::filter(is.na(d_name)) %>% nrow()
print(paste("Number of rows with NA in d_name:", na_count))
na_count2 <- final_df %>% dplyr::filter(is.na(s_name)) %>% nrow()
print(paste("Number of rows with NA in s_name:", na_count2))
s_name_na_rows <- final_df %>% dplyr::filter(is.na(s_name))
if (na_count > 0 || na_count2 > 0) {
  stop("NA values detected in s_name or d_name. Exiting the script.")
}




final_df <- final_df %>%
  dplyr::select(-state, -district, -district_area, -yield)


final_df <- final_df %>%
  group_by(s_name, d_name, start_year, end_year, season, crop) %>%
  summarise(area = sum(area, na.rm = TRUE),
            production = sum(production, na.rm = TRUE),
            yield = round(production / area, 2)) %>%
  ungroup()


final_df <- final_df %>%
  left_join(df_climate, by = c("s_name", "d_name", "start_year" = "year"))

na_count <- final_df %>%dplyr::filter(is.na(pc11_d_id)) %>% nrow()
print(paste("Number of rows with NA in pc11_d_id:", na_count))
na_count2 <- final_df %>% dplyr::filter(is.na(pc11_s_id)) %>% nrow()
print(paste("Number of rows with NA in pc11_s_id:", na_count2))
pc11_na_rows <- final_df %>% dplyr::filter(is.na(pc11_s_id))
if (na_count > 0 || na_count2 > 0) {
  stop("NA values detected in pc11_s_id or pc11_d_id. Exiting the script.")
}



ordered_df <- final_df %>%
  dplyr::select(
    pc11_s_id, pc11_d_id, s_name, d_name, start_year, end_year, season, crop, area, production, yield,
    sim_mean_yearly_sum_tp,
    starts_with("mean_"),
    starts_with("area_mean_"),
    district_area,
    starts_with("proportion_"),
    starts_with("cluster_"),
    area_difference_rate,
    sum_na_area, sum_intersection_area, sum_na_count, sum_pixel_count,
    sum_na_weight, sum_pixel_weight, sum_bins_mean, sum_bins_area_mean,
    matches("^sum_(?!na_area|intersection_area|na_count|pixel_count|na_weight|pixel_weight|bins_mean|bins_area_mean).*", perl = TRUE),
    everything()
  ) 

cleaned_df <- ordered_df %>%
  rename_with(~ str_replace(., "^mean_yearly_mean_temp$", "temp_year")) %>%
  rename_with(~ str_replace_all(., "^area_mean_yearly_mean_temp$", "area_temp_year")) %>% 
  rename_with(~ str_replace_all(., "^mean_m([0-9]+)$", "temp_m\\1")) %>%
  rename_with(~ str_replace_all(., "^area_mean_m([0-9]+)$", "area_temp_m\\1")) %>%
  rename_with(~ str_replace_all(., "^mean_t_", "t_")) %>%
  rename_with(~ str_replace_all(., "^area_mean_t_", "area_t_")) %>%
  rename_with(~ str_replace(., "^sim_mean_yearly_sum_tp$", "sim_tp_year")) %>%
  rename_with(~ str_replace(., "^mean_yearly_sum_tp$", "tp_year")) %>%
  rename_with(~ str_replace_all(., "^mean_monthly_sum_tp_", "tp_")) %>%
  rename_with(~ str_replace_all(., "^area_mean_yearly_sum_tp$", "area_tp_year")) %>%
  rename_with(~ str_replace_all(., "^area_mean_monthly_sum_tp_", "area_tp_")) %>%
  rename_with(~ str_replace_all(., "^mean_onset_", "onset_")) %>%
  rename_with(~ str_replace_all(., "^area_mean_onset_", "area_onset_")) %>%
  rename_with(~ str_replace_all(., "^onset_date4_p$", "onset_date")) %>%
  rename_with(~ str_replace_all(., "^onset_date4_p_v2$", "onset_date_na")) %>%
  rename_with(~ str_replace_all(., "^onset_date4_p_final$", "onset_date_v2")) %>%
  rename_with(~ str_replace_all(., "^area_onset_date4_p$", "area_onset_date")) %>%
  rename_with(~ str_replace_all(., "^area_onset_date4_p_v2$", "area_onset_date_na")) %>%
  rename_with(~ str_replace_all(., "^area_onset_date4_p_final$", "area_onset_date_v2"))

names(cleaned_df)

label(cleaned_df$pc11_s_id) <- "State ID (from SHRUG)"
label(cleaned_df$pc11_d_id) <- "District ID (from SHRUG)"
label(cleaned_df$d_name) <- "District name (from SHRUG)"
label(cleaned_df$s_name) <- "State name (from SHRUG)"
label(cleaned_df$start_year) <- "Year of data (from ERA5) and start year of the crop year (e.g., 2022–2023)"
label(cleaned_df$end_year) <- "End year of the crop year (e.g., 2022–2023)"
label(cleaned_df$season) <- "Season (from crop data)"
label(cleaned_df$crop) <- "Crop (from crop data)"
label(cleaned_df$area) <- "Area (unit: hectares; from crop data)"
label(cleaned_df$production) <- "Production (unit: tonnes; except for nuts – coconut, bales – cotton, jute, mesta; from crop data)"
label(cleaned_df$yield) <- "Production divided by area (unit: typically tonnes per hectare)"

label(cleaned_df$sim_tp_year) <- "Mean of yearly total precipitation (unit: meters; simple average across pixel grids)"
label(cleaned_df$tp_year) <- "Mean of yearly total precipitation (unit: meters; weight = intersection area / pixel grid area)"
label(cleaned_df$onset_date) <- "Mean monsoon onset date (Version 1; weight = intersection area / pixel grid area)"
label(cleaned_df$onset_date_na) <- "Mean monsoon onset date (NA values from Version 1 filled using relaxed dry-spell condition; weight = intersection area / pixel grid area)"
label(cleaned_df$onset_date_v2) <- "Mean monsoon onset date (Version 2; NA pixels filled using relaxed dry-spell condition; weight = intersection area / pixel grid area)"
label(cleaned_df$area_tp_year) <- "Mean of yearly total precipitation (unit: meters; weight = intersection area / total district area)"
label(cleaned_df$area_onset_date) <- "Mean monsoon onset date (Version 1; weight = intersection area / total district area)"
label(cleaned_df$area_onset_date_na) <- "Mean monsoon onset date (NA values from Version 1 filled using relaxed dry-spell condition; weight = intersection area / total district area)"
label(cleaned_df$area_onset_date_v2) <- "Mean monsoon onset date (Version 2; NA pixels filled using relaxed dry-spell condition; weight = intersection area / total district area)"

label(cleaned_df$district_area) <- "Total district area (unit: square meters; from SHRUG)"
label(cleaned_df$proportion_na) <- "Proportion of overlapped pixel grids with NA onset date (sum_na_count / sum_pixel_count)"
label(cleaned_df$proportion_na_weight) <- "Proportion of NA pixels in terms of weights (sum_na_weight / sum_pixel_weight)"
label(cleaned_df$proportion_na_area) <- "Proportion of overlapped area with NA pixel grids (sum_na_area / sum_intersection_area)"

label(cleaned_df$cluster_1) <- "Sum of pixel area weights (intersection area / total district area) where the pixel is assigned to cluster 1"
label(cleaned_df$cluster_2) <- "Sum of pixel area weights (intersection area / total district area) where the pixel is assigned to cluster 2"
label(cleaned_df$cluster_3) <- "Sum of pixel area weights (intersection area / total district area) where the pixel is assigned to cluster 3"
label(cleaned_df$cluster_4) <- "Sum of pixel area weights (intersection area / total district area) where the pixel is assigned to cluster 4"
label(cleaned_df$cluster_5) <- "Sum of pixel area weights (intersection area / total district area) where the pixel is assigned to cluster 5"
label(cleaned_df$cluster_na) <- "Sum of pixel area weights (intersection area / total district area) where the pixel is not assigned to any cluster (NA)"
label(cleaned_df$cluster_sum) <- "Total sum of pixel area weights across all clusters (1–5 and NA; intersection area / total district area)"

label(cleaned_df$temp_year) <- "Mean of yearly mean temperature (unit: meters; weight = intersection area / pixel grid area)"
label(cleaned_df$area_temp_year) <- "Mean of yearly mean temperature (unit: meters; weight = intersection area / total district area)"

label(cleaned_df$area_difference_rate) <- "Difference rate between SHRUG district area and aggregated pixel intersection area ((district_area - sum_intersection_area) / district_area)"
label(cleaned_df$sum_na_area) <- "Sum of intersection areas for pixel grids with NA onset date in Version 1 (unit: square meters)"
label(cleaned_df$sum_intersection_area) <- "Sum of intersection areas between district and pixel grids (unit: square meters)"
label(cleaned_df$sum_na_count) <- "Number of overlapped pixel grids with NA onset date in Version 1 (fully or partially overlapping)"
label(cleaned_df$sum_pixel_count) <- "Total number of overlapped pixel grids (fully or partially overlapping)"
label(cleaned_df$sum_na_weight) <- "Sum of weights for pixel grids with NA onset date in Version 1 (weight = intersection area / pixel grid area)"
label(cleaned_df$sum_pixel_weight) <- "Total sum of weights across all overlapped pixel grids (weight = intersection area / pixel grid area)"
label(cleaned_df$sum_bins_mean) <- "Sum of weighted means for bin values (weight = intersection area / pixel grid area)"
label(cleaned_df$sum_bins_area_mean) <- "Sum of weighted means for bin values (weight = intersection area / total district area)"
label(cleaned_df$check) <- "Whether sum_bins_area_mean is 365 or 366 (leap year)"

for (i in 1:12) {
  var <- paste0("tp_m", i)
  label(cleaned_df[[var]]) <- paste0("Mean of m", i, " monthly total precipitation (unit: meters; weight = intersection area / pixel grid area)")
}

for (i in 1:12) {
  var <- paste0("area_tp_m", i)
  label(cleaned_df[[var]]) <- paste0("Mean of m", i, " monthly total precipitation (unit: meters; weight = intersection area / total district area)")
}

for (i in 1:12) {
  var <- paste0("temp_m", i)
  label(cleaned_df[[var]]) <- paste0("Mean of m", i, " monthly mean temperature (unit: Celsius; weight = intersection area / pixel grid area)")
}

for (i in 1:12) {
  var <- paste0("area_temp_m", i)
  label(cleaned_df[[var]]) <- paste0("Mean of m", i, " monthly mean temperature (unit: Celsius; weight = intersection area / total district area)")
}

temp_bins <- c(
  "t_below_0_C", "t_0_5_C", "t_5_10_C", "t_10_15_C", "t_15_20_C",
  "t_20_25_C", "t_25_30_C", "t_30_35_C", "t_35_over_C"
)

for (var in temp_bins) {
  label(cleaned_df[[var]]) <- paste0(
    'Annual, weighted mean of days in temperature bin "', var,
    '" (unit: days; weight = intersection area / pixel grid area)'
  )
}


area_temp_bins <- c(
  "area_t_below_0_C", "area_t_0_5_C", "area_t_5_10_C", "area_t_10_15_C", "area_t_15_20_C",
  "area_t_20_25_C", "area_t_25_30_C", "area_t_30_35_C", "area_t_35_over_C"
)

for (var in area_temp_bins) {
  var_base <- sub("^area_", "", var)
  label(cleaned_df[[var]]) <- paste0(
    'Annual, weighted mean of days in temperature bin "', var_base,
    '" (unit: days; weight = intersection area / total district area)'
  )
}

monthly_temp_bins <- grep("^t_m\\d+_", names(cleaned_df), value = TRUE)

for (var in monthly_temp_bins) {
  month_num <- gsub("^t_m(\\d+)_.*", "\\1", var)
  label(cleaned_df[[var]]) <- paste0(
    'Month ', month_num, ', weighted mean of days in temperature bin "', var,
    '" (unit: days; weight = intersection area / pixel grid area)'
  )
}

monthly_area_temp_bins <- grep("^area_t_m\\d+_", names(cleaned_df), value = TRUE)

for (var in monthly_area_temp_bins) {
  month_num <- gsub("^area_t_m(\\d+)_.*", "\\1", var)
  var_base <- sub("^area_", "", var)
  
  label(cleaned_df[[var]]) <- paste0(
    'Month ', month_num, ', weighted mean of days in temperature bin "', var_base,
    '" (unit: days; weight = intersection area / total district area)'
  )
}



write.csv(cleaned_df, file.path(final_dir, "district_crop_climate_data.csv"), row.names = FALSE)
write_dta(cleaned_df, file.path(final_dir, "district_crop_climate_data.dta"))


#district_data <- read.csv(file.path(final_dir, "district_crop_climate_data.csv"))

# kmeans_path <- file.path(clean_data_era5, "k_means_monsoon_1940_2024.csv")
# file.copy(kmeans_path, file.path(final_dir, "k_means_monsoon_1940_2024.csv"), overwrite = TRUE)
# kmeans <- read.csv(file.path(clean_data_era5, "k_means_monsoon_1940_2024.csv"))
# haven::write_dta(kmeans, file.path(final_dir, "k_means_monsoon_1940_2024.dta"))

onset_path <- file.path(clean_data_era5, "district_monsoon_onset_dates_1940_2025.csv")
temp_path <- file.path(clean_data_era5, "district_temperature_1940_2025.csv")
file.copy(onset_path, file.path(final_dir, "district_monsoon_onset_dates_1940_2025.csv"), overwrite = TRUE)
file.copy(temp_path, file.path(final_dir, "district_temperature_1940_2025.csv"), overwrite = TRUE)


onset <- read.csv(file.path(clean_data_era5, "district_monsoon_onset_dates_1940_2025.csv"))
onset <- onset %>% 
  dplyr::select(
    pc11_s_id, pc11_d_id, s_name, d_name, year,
    sim_mean_yearly_sum_tp,
    starts_with("mean_"),
    starts_with("area_mean_"),
    district_area,
    starts_with("proportion_"),
    starts_with("cluster_"),
    area_difference_rate,
    sum_na_area, sum_intersection_area, sum_na_count, sum_pixel_count,
    sum_na_weight, sum_pixel_weight,
    matches("^sum_(?!na_area|intersection_area|na_count|pixel_count|na_weight|pixel_weight).*", perl = TRUE),
    everything()
  ) %>% 
  rename_with(~ str_replace(., "^sim_mean_yearly_sum_tp$", "sim_tp_year")) %>%
  rename_with(~ str_replace(., "^mean_yearly_sum_tp$", "tp_year")) %>%
  rename_with(~ str_replace_all(., "^mean_monthly_sum_tp_", "tp_")) %>%
  rename_with(~ str_replace_all(., "^area_mean_yearly_sum_tp$", "area_tp_year")) %>%
  rename_with(~ str_replace_all(., "^area_mean_monthly_sum_tp_", "area_tp_")) %>%
  rename_with(~ str_replace_all(., "^mean_onset_", "onset_")) %>%
  rename_with(~ str_replace_all(., "^area_mean_onset_", "area_onset_")) %>%
  rename_with(~ str_replace_all(., "^onset_date4_p$", "onset_date")) %>%
  rename_with(~ str_replace_all(., "^onset_date4_p_v2$", "onset_date_na")) %>%
  rename_with(~ str_replace_all(., "^onset_date4_p_final$", "onset_date_v2")) %>%
  rename_with(~ str_replace_all(., "^area_onset_date4_p$", "area_onset_date")) %>%
  rename_with(~ str_replace_all(., "^area_onset_date4_p_v2$", "area_onset_date_na")) %>%
  rename_with(~ str_replace_all(., "^area_onset_date4_p_final$", "area_onset_date_v2"))
  
names(onset)

label(onset$pc11_s_id) <- "State ID (from SHRUG)"
label(onset$pc11_d_id) <- "District ID (from SHRUG)"
label(onset$d_name) <- "District name (from SHRUG)"
label(onset$s_name) <- "State name (from SHRUG)"
label(onset$year) <- "Year of data (from ERA5)"
label(onset$sim_tp_year) <- "Mean of yearly total precipitation (unit: meters; simple average across pixel grids)"
label(onset$tp_year) <- "Mean of yearly total precipitation (unit: meters; weight = intersection area / pixel grid area)"
label(onset$onset_date) <- "Mean monsoon onset date (Version 1; weight = intersection area / pixel grid area)"
label(onset$onset_date_na) <- "Mean monsoon onset date (NA values from Version 1 filled using relaxed dry-spell condition; weight = intersection area / pixel grid area)"
label(onset$onset_date_v2) <- "Mean monsoon onset date (Version 2; NA pixels filled using relaxed dry-spell condition; weight = intersection area / pixel grid area)"
label(onset$area_tp_year) <- "Mean of yearly total precipitation (unit: meters; weight = intersection area / total district area)"
label(onset$area_onset_date) <- "Mean monsoon onset date (Version 1; weight = intersection area / total district area)"
label(onset$area_onset_date_na) <- "Mean monsoon onset date (NA values from Version 1 filled using relaxed dry-spell condition; weight = intersection area / total district area)"
label(onset$area_onset_date_v2) <- "Mean monsoon onset date (Version 2; NA pixels filled using relaxed dry-spell condition; weight = intersection area / total district area)"

label(cleaned_df$district_area) <- "Total district area (unit: square meters; from SHRUG)"
label(onset$proportion_na) <- "Proportion of overlapped pixel grids with NA onset date (sum_na_count / sum_pixel_count)"
label(onset$proportion_na_weight) <- "Proportion of NA pixels in terms of weights (sum_na_weight / sum_pixel_weight)"
label(onset$proportion_na_area) <- "Proportion of overlapped area with NA pixel grids (sum_na_area / sum_intersection_area)"

label(onset$cluster_1) <- "Sum of pixel area weights (intersection area / total district area) where the pixel is assigned to cluster 1"
label(onset$cluster_2) <- "Sum of pixel area weights (intersection area / total district area) where the pixel is assigned to cluster 2"
label(onset$cluster_3) <- "Sum of pixel area weights (intersection area / total district area) where the pixel is assigned to cluster 3"
label(onset$cluster_4) <- "Sum of pixel area weights (intersection area / total district area) where the pixel is assigned to cluster 4"
label(onset$cluster_5) <- "Sum of pixel area weights (intersection area / total district area) where the pixel is assigned to cluster 5"
label(onset$cluster_na) <- "Sum of pixel area weights (intersection area / total district area) where the pixel is not assigned to any cluster (NA)"
label(onset$cluster_sum) <- "Total sum of pixel area weights across all clusters (1–5 and NA; intersection area / total district area)"

label(onset$area_difference_rate) <- "Difference rate between SHRUG district area and aggregated pixel intersection area ((district_area - sum_intersection_area) / district_area)"
label(cleaned_df$sum_na_area) <- "Sum of intersection areas for pixel grids with NA onset date in Version 1 (unit: square meters)"
label(cleaned_df$sum_intersection_area) <- "Sum of intersection areas between district and pixel grids (unit: square meters)"
label(onset$sum_na_count) <- "Number of overlapped pixel grids with NA onset date in Version 1 (fully or partially overlapping)"
label(onset$sum_pixel_count) <- "Total number of overlapped pixel grids (fully or partially overlapping)"
label(onset$sum_na_weight) <- "Sum of weights for pixel grids with NA onset date in Version 1 (weight = intersection area / pixel grid area)"
label(onset$sum_pixel_weight) <- "Total sum of weights across all overlapped pixel grids (weight = intersection area / pixel grid area)"

for (i in 1:12) {
  var <- paste0("tp_m", i)
  label(onset[[var]]) <- paste0("Mean of m", i, " monthly total precipitation (unit: meters; weight = intersection area / pixel grid area)")
}

for (i in 1:12) {
  var <- paste0("area_tp_m", i)
  label(onset[[var]]) <- paste0("Mean of m", i, " monthly total precipitation (unit: meters; weight = intersection area / total district area)")
}

write.csv(onset, file = file.path(final_dir, "district_monsoon_onset_dates_1940_2025.csv"), row.names = FALSE)
haven::write_dta(onset, file.path(final_dir, "district_monsoon_onset_dates_1940_2025.dta"))

label(onset$area_onset_date_v2)

temp <- read.csv(file.path(clean_data_era5, "district_temperature_1940_2025.csv"))
temp <- temp %>% 
  dplyr::select(
    pc11_s_id, pc11_d_id, s_name, d_name, year, 
    starts_with("mean_"),
    starts_with("area_mean_"),
    sum_bins_mean, sum_bins_area_mean,
    everything()
  ) %>% 
  rename_with(~ str_replace(., "^mean_yearly_mean_temp$", "temp_year")) %>%
  rename_with(~ str_replace_all(., "^area_mean_yearly_mean_temp$", "area_temp_year")) %>%  
  rename_with(~ str_replace_all(., "^mean_m([0-9]+)$", "temp_m\\1")) %>%
  rename_with(~ str_replace_all(., "^area_mean_m([0-9]+)$", "area_temp_m\\1")) %>%
  rename_with(~ str_replace_all(., "^mean_t_", "t_")) %>%
  rename_with(~ str_replace_all(., "^area_mean_t_", "area_t_")) 

names(temp)


label(temp$pc11_s_id) <- "State ID (from SHRUG)"
label(temp$pc11_d_id) <- "District ID (from SHRUG)"
label(temp$d_name) <- "District name (from SHRUG)"
label(temp$s_name) <- "State name (from SHRUG)"
label(temp$year) <- "Year of data (from ERA5)"
label(temp$sum_bins_mean) <- "Sum of weighted means for bin values (weight = intersection area / pixel grid area)"
label(temp$sum_bins_area_mean) <- "Sum of weighted means for bin values (weight = intersection area / total district area)"
label(temp$check) <- "Whether sum_bins_area_mean is 365 or 366 (leap year)"

label(temp$temp_year) <- "Mean of yearly mean temperature (unit: meters; weight = intersection area / pixel grid area)"
label(temp$area_temp_year) <- "Mean of yearly mean temperature (unit: meters; weight = intersection area / total district area)"

for (i in 1:12) {
  var <- paste0("temp_m", i)
  label(temp[[var]]) <- paste0("Mean of m", i, " monthly mean temperature (unit: Celsius; weight = intersection area / pixel grid area)")
}

for (i in 1:12) {
  var <- paste0("area_temp_m", i)
  label(temp[[var]]) <- paste0("Mean of m", i, " monthly mean temperature (unit: Celsius; weight = intersection area / total district area)")
}

temp_bins <- c(
  "t_below_0_C", "t_0_5_C", "t_5_10_C", "t_10_15_C", "t_15_20_C",
  "t_20_25_C", "t_25_30_C", "t_30_35_C", "t_35_over_C"
)

for (var in temp_bins) {
  label(cleaned_df[[var]]) <- paste0(
    'Annual, weighted mean of days in temperature bin "', var,
    '" (unit: days; weight = intersection area / pixel grid area)'
  )
}


area_temp_bins <- c(
  "area_t_below_0_C", "area_t_0_5_C", "area_t_5_10_C", "area_t_10_15_C", "area_t_15_20_C",
  "area_t_20_25_C", "area_t_25_30_C", "area_t_30_35_C", "area_t_35_over_C"
)

for (var in area_temp_bins) {
  var_base <- sub("^area_", "", var)
  label(cleaned_df[[var]]) <- paste0(
    'Annual, weighted mean of days in temperature bin "', var_base,
    '" (unit: days; weight = intersection area / total district area)'
  )
}

monthly_temp_bins <- grep("^t_m\\d+_", names(cleaned_df), value = TRUE)

for (var in monthly_temp_bins) {
  month_num <- gsub("^t_m(\\d+)_.*", "\\1", var)
  label(cleaned_df[[var]]) <- paste0(
    'Month ', month_num, ', weighted mean of days in temperature bin "', var,
    '" (unit: days; weight = intersection area / pixel grid area)'
  )
}

monthly_area_temp_bins <- grep("^area_t_m\\d+_", names(cleaned_df), value = TRUE)

for (var in monthly_area_temp_bins) {
  month_num <- gsub("^area_t_m(\\d+)_.*", "\\1", var)
  var_base <- sub("^area_", "", var)
  
  label(cleaned_df[[var]]) <- paste0(
    'Month ', month_num, ', weighted mean of days in temperature bin "', var_base,
    '" (unit: days; weight = intersection area / total district area)'
  )
}


write.csv(temp, file = file.path(final_dir, "district_temperature_1940_2025.csv"), row.names = FALSE)
haven::write_dta(temp, file.path(final_dir, "district_temperature_1940_2025.dta"))





##### Simple Analysis / Visualization #####



## District level plot ##

final_dir <- file.path(user_path, "data", "final")
shrug_state_data_dir <- file.path(user_path, "data", "shrug", "shrug-pc11state-poly-shp")
state_shp_path <- file.path(shrug_state_data_dir, "state.shp")
shrug_district_data_dir <- file.path(user_path, "data", "shrug", "shrug-pc11dist-poly-shp")
district_shp_path <- file.path(shrug_district_data_dir, "district.shp")
figure_output_dir <- file.path(user_path, "figure", "paper")

crop_df <- read.csv(file.path(final_dir, "district_crop_climate_data.csv"))
class(crop_df$pc11_s_id) # integer 
class(crop_df$pc11_d_id) # integer

onset_df <- read.csv(file.path(final_dir, "district_monsoon_onset_dates_1940_2025.csv"))
temp_df <- read.csv(file.path(final_dir, "district_temperature_1940_2025.csv"))

state_sf <- st_read(state_shp_path) %>%
  mutate(pc11_s_id = as.integer(pc11_s_id))

district_sf <- st_read(district_shp_path) %>%
  st_transform(4326) %>%
  st_make_valid() %>%
  mutate(pc11_s_id = as.integer(pc11_s_id),
         pc11_d_id = as.integer(pc11_d_id)) %>%
  dplyr::select(pc11_s_id, pc11_d_id, geometry)


disputed_districts_sf <- district_sf %>%
  dplyr::left_join(onset_df %>% dplyr::select(pc11_s_id, pc11_d_id, d_name, s_name), 
                   by = c("pc11_s_id", "pc11_d_id")) %>%
  dplyr::filter(s_name == "Jammu and Kashmir", is.na(d_name)) %>% distinct()



india_states <- ne_states(country = "India", returnclass = "sf")
telangana_sf <- india_states %>% dplyr::filter(name == "Telangana")


prepare_onset_df <- function(onset_df, var_name) {
  onset_df %>%
    dplyr::mutate(onset_date = as.Date(.data[[var_name]]),
                  onset_doy = yday(onset_date))
}


# Plot mean onset
plot_avg_onset_general <- function(onset_df, start_year, end_year, var_name, version_tag, file_suffix) {
  onset_df_proc <- prepare_onset_df(onset_df, var_name) %>% dplyr::filter(!is.na(d_name))
  
  avg_onset_df <- onset_df_proc %>%
    dplyr::filter(year >= start_year, year <= end_year) %>%
    group_by(pc11_s_id, pc11_d_id) %>%
    summarise(mean_doy = mean(onset_doy, na.rm = TRUE), .groups = 'drop') %>%
    mutate(onset_bin = make_onset_bins(mean_doy))
  
  map_df_full <- district_sf %>%
    left_join(avg_onset_df, by = c("pc11_s_id", "pc11_d_id"))
  
  map_df_valid <- map_df_full %>%
    dplyr::filter(!is.na(onset_bin))
  
  map_df_disputed <- map_df_full %>%
    dplyr::filter(is.na(onset_bin))
  
  onset_colors <- c(
    "#08306B",  # deep navy
    "#08519C",
    "#2171B5",
    "#3579AD",  # darker than #4292C6
    "#5399C3",  # darker than #6BAED6
    "#6DBDA6",  # darker than #84C9B4
    "#83C299",  # darker than #9BD6A9
    "#9FCFB2",  # darker than #B4DAC0
    "#B1D9C1",  # darker than #C7E4CE
    "#C4E1D5"   # darker than #D9EEE2
  )  
  
  p <- ggplot() +
    geom_sf(data = map_df_disputed, fill = "gray95", color = NA, inherit.aes = FALSE) +  # mask disputed districts
    geom_sf(data = map_df_valid, aes(fill = onset_bin), color = NA) +  # only valid bins go to color scale
    geom_sf(data = telangana_sf, fill = NA, color = "black", linewidth = 1) +
    scale_fill_manual(
      values = onset_colors,
      name = "Onset mean (date)",
      guide = guide_legend(na.translate = FALSE)
    ) +
    coord_sf() +
    theme_void(base_size = 14) +
    theme(
      legend.position = "right",
      legend.title = element_text(size = 13, face = "bold"),
      legend.text = element_text(size = 11),
      panel.grid = element_blank(),
      plot.title = element_blank(),
      plot.subtitle = element_blank(), 
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA)
    )
  
  ggsave(file.path(figure_output_dir, paste0("onset_avg_binned_", version_tag, "_", file_suffix, ".png")),
         p, width = 10, height = 10, dpi = 300)
}

# Plot onset SD
plot_sd_onset_general <- function(onset_df, start_year, end_year, var_name, version_tag, file_suffix) {
  onset_df_proc <- prepare_onset_df(onset_df, var_name) %>% dplyr::filter(!is.na(d_name))
  
  sd_onset_df <- onset_df_proc %>%
    dplyr::filter(year >= start_year, year <= end_year) %>%
    group_by(pc11_s_id, pc11_d_id) %>%
    summarise(sd_doy = sd(onset_doy, na.rm = TRUE), .groups = 'drop') %>%
    mutate(sd_bin = make_sd_bins(sd_doy))
  
  map_df_full <- district_sf %>%
    left_join(sd_onset_df, by = c("pc11_s_id", "pc11_d_id"))
  
  map_df_valid <- map_df_full %>%
    dplyr::filter(!is.na(sd_bin))  # or !is.na(onset_bin)
  
  map_df_disputed <- map_df_full %>%
    dplyr::filter(is.na(sd_bin))  # this now isolates disputed districts
  
  sd_colors <- RColorBrewer::brewer.pal(5, "YlOrBr")
  
  p <- ggplot() +
    geom_sf(data = map_df_disputed, fill = "gray95", color = NA, inherit.aes = FALSE) +  # masks NA regions
    geom_sf(data = map_df_valid, aes(fill = sd_bin), color = NA) +  # plots valid districts only
    geom_sf(data = telangana_sf, fill = NA, color = "black", linewidth = 1) +
    scale_fill_manual(
      values = sd_colors,
      name = "Onset standard\ndeviation (days)",
      guide = guide_legend(na.translate = FALSE)
    ) +
    coord_sf() +
    theme_void(base_size = 14) +
    theme(
      legend.position = "right",
      legend.title = element_text(size = 13, face = "bold"),
      legend.text = element_text(size = 11),
      panel.grid = element_blank(),
      plot.title = element_blank(),
      plot.subtitle = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA)
    )
  
  ggsave(file.path(figure_output_dir, paste0("onset_sd_binned_", version_tag, "_", file_suffix, ".png")),
         p, width = 10, height = 10, dpi = 300)
}


# Version v1: onset_date
plot_avg_onset_general(onset_df, 1940, 2024, "onset_date", "v1", "1940_2024")
plot_avg_onset_general(onset_df, 1997, 2022, "onset_date", "v1", "1997_2022")
plot_avg_onset_general(onset_df, 1979, 2019, "onset_date", "v1", "1979_2019")

plot_sd_onset_general(onset_df, 1940, 2024, "onset_date", "v1", "1940_2024")
plot_sd_onset_general(onset_df, 1997, 2022, "onset_date", "v1", "1997_2022")
plot_sd_onset_general(onset_df, 1979, 2019, "onset_date", "v1", "1979_2019")

# Version v2: onset_date4_v2
plot_avg_onset_general(onset_df, 1940, 2024, "onset_date_v2", "v2", "1940_2024")
plot_avg_onset_general(onset_df, 1997, 2022, "onset_date_v2", "v2", "1997_2022")
plot_avg_onset_general(onset_df, 1979, 2019, "onset_date_v2", "v2", "1979_2019")

plot_sd_onset_general(onset_df, 1940, 2024, "onset_date_v2", "v2", "1940_2024")
plot_sd_onset_general(onset_df, 1997, 2022, "onset_date_v2", "v2", "1997_2022")
plot_sd_onset_general(onset_df, 1979, 2019, "onset_date_v2", "v2", "1979_2019")



plot_onset_vs_rainfall <- function(start_year, end_year, file_suffix) {
  onset_summary <- onset_df %>%
    dplyr::filter(year >= start_year, year <= end_year) %>%
    mutate(
      onset_doy = yday(ymd(as.character(onset_date_v2)))
    ) %>%
    group_by(pc11_s_id, pc11_d_id) %>%
    summarise(
      mean_onset_doy = mean(onset_doy, na.rm = TRUE),
      mean_tp_year_mm = mean(tp_year * 1000, na.rm = TRUE),  # convert m to mm
      .groups = "drop"
    )
  # For converting DOY to calendar labels (using 2023 as dummy non-leap year)
  doy_to_date <- function(doy) {
    format(as.Date(doy - 1, origin = "2023-01-01"), "%b %d")
  }
  
  p <- ggplot(onset_summary, aes(x = mean_onset_doy, y = mean_tp_year_mm)) +
    geom_point(alpha = 0.6, size = 2, color = "#2c7fb8") +
    theme_minimal(base_size = 14) +
    labs(
      x = "Mean Onset Date",
      y = "Mean Annual Rainfall (mm)",
      # title = paste("Onset vs Rainfall by District (", start_year, "-", end_year, ")", sep = ""),
      # subtitle = "Each point represents a district"
    ) +
    scale_x_continuous(
      breaks = yday(as.Date(c("2023-04-01", "2023-05-01", "2023-06-01", "2023-07-01"))),
      labels = doy_to_date
    ) +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 13)
    )
  
  ggsave(
    filename = file.path(figure_output_dir, paste0("onset_vs_rainfall_", file_suffix, ".png")),
    plot = p,
    width = 10, height = 7, dpi = 300
  )
}

plot_onset_vs_rainfall(1940, 2024, "1940_2024")
plot_onset_vs_rainfall(1997, 2022, "1997_2022")







## Grid level plot ##


# Load grid-level data
grid_district_onset_load <- file.path(clean_data_dir, paste0("grid_district_weights_", 1940, "_", 2025, ".csv"))
grid_precip_data <- read_csv(grid_district_onset_load) %>%
  dplyr::filter(year >= start_year_precip & year <= 2024)

# grid_precip_data <- read_csv(grid_district_onset_load) %>%
#   dplyr::filter(
#     year >= start_year_precip & year <= 2024,
#     !(s_name == "Jammu and Kashmir" & is.na(d_name))
#   )


# Rename variable to `onset_date` and compute DOY
prepare_grid_data <- function(df, var_name) {
  df %>%
    dplyr::rename(onset_date = !!sym(var_name)) %>%
    dplyr::mutate(onset_date = as.Date(onset_date),
                  onset_doy = yday(onset_date))
}


# disputed_sf <- grid_precip_data %>%
#   dplyr::filter(s_name == "Jammu and Kashmir", is.na(d_name)) %>%
#   dplyr::distinct(lon, lat) %>%
#   sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
# 
disputed_points <- grid_precip_data %>%
  dplyr::filter(s_name == "Jammu and Kashmir", is.na(d_name)) %>%
  dplyr::distinct(lon, lat)

# Binning helpers
make_onset_bins <- function(x) {
  breaks <- quantile(x, probs = seq(0, 1, length.out = 11), na.rm = TRUE)
  labels <- paste(
    format(as.Date(breaks[-length(breaks)], origin = "2023-01-01"), "%b %d"),
    "-",
    format(as.Date(breaks[-1] - 1, origin = "2023-01-01"), "%b %d")
  )
  cut(x, breaks = breaks, labels = labels, include.lowest = TRUE)
}

make_sd_bins <- function(x) {
  cut(x,
      breaks = c(-Inf, 10.7, 13.0, 14.6, 17.3, Inf),
      labels = c("< 10.7", "10.7 – 13.0", "13.0 – 14.6", "14.6 – 17.3", "> 17.3"),
      include.lowest = TRUE)
}

# Plot: Average onset (grid)
plot_avg_onset_grid <- function(df, start_year, end_year, var_name, version_tag, file_suffix) {
  grid_df <- prepare_grid_data(df, var_name) %>%
    dplyr::filter(year >= start_year, year <= end_year) %>%
    dplyr::group_by(lon, lat) %>%
    dplyr::summarise(mean_doy = mean(onset_doy, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(onset_bin = make_onset_bins(mean_doy))
 
  onset_colors <- c(
    "#08306B",  # deep navy
    "#08519C",
    "#2171B5",
    "#3579AD",  # darker than #4292C6
    "#5399C3",  # darker than #6BAED6
    "#6DBDA6",  # darker than #84C9B4
    "#83C299",  # darker than #9BD6A9
    "#9FCFB2",  # darker than #B4DAC0
    "#B1D9C1",  # darker than #C7E4CE
    "#C4E1D5"   # darker than #D9EEE2
  )  
   
  # onset_colors <- c(
  #   "#08306B",  # deep navy
  #   "#08519C",
  #   "#2171B5",
  #   "#4292C6",
  #   "#6BAED6",
  #   "#84C9B4",  # adjusted from #9BD3C2
  #   "#9BD6A9",  # adjusted from #B2E2B7
  #   "#B4DAC0",  # adjusted from #C8E6C5
  #   "#C7E4CE",  # adjusted from #D9F0D9
  #   "#D9EEE2"   # adjusted from #E8F7E8
  # )
  
  p <- ggplot(data = grid_df, mapping = aes(x = lon, y = lat, fill = onset_bin)) +
    geom_tile() +
    geom_tile(data = disputed_points, aes(x = lon, y = lat), fill = "gray95", inherit.aes = FALSE) +
    geom_sf(data = telangana_sf, fill = NA, color = "black", linewidth = 1, inherit.aes = FALSE) +
    scale_fill_manual(values = onset_colors, name = "Onset mean (date)") +
    coord_sf() +
    theme_void(base_size = 14) +
    theme(
      legend.position = "right",
      legend.title = element_text(size = 13, face = "bold"),
      legend.text = element_text(size = 11),
      panel.grid = element_blank(),
      plot.title = element_blank(),
      plot.subtitle = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA)
    )
  
  ggsave(file.path(figure_output_dir, paste0("grid_onset_avg_binned_", version_tag, "_", file_suffix, ".png")),
         p, width = 10, height = 10, dpi = 300)
}

# Plot: Onset SD (grid)
plot_sd_onset_grid <- function(df, start_year, end_year, var_name, version_tag, file_suffix) {
  grid_df <- prepare_grid_data(df, var_name) %>%
    dplyr::filter(year >= start_year, year <= end_year) %>%
    dplyr::group_by(lon, lat) %>%
    dplyr::summarise(sd_doy = sd(onset_doy, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(sd_bin = make_sd_bins(sd_doy))
  
  sd_colors <- RColorBrewer::brewer.pal(5, "YlOrBr")
  
  p <- ggplot(grid_df, aes(x = lon, y = lat, fill = sd_bin)) +
    geom_tile() +
    geom_tile(data = disputed_points, aes(x = lon, y = lat), fill = "gray95", inherit.aes = FALSE) +
    geom_sf(data = telangana_sf, fill = NA, color = "black", linewidth = 1, inherit.aes = FALSE) +
    scale_fill_manual(values = sd_colors, name = "Onset SD (days)") +
    coord_sf() +
    theme_void(base_size = 14) +
    theme(
      legend.position = "right",
      legend.title = element_text(size = 13, face = "bold"),
      legend.text = element_text(size = 11),
      panel.grid = element_blank(),
      plot.title = element_blank(),
      plot.subtitle = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA)
    )
  
  ggsave(file.path(figure_output_dir, paste0("grid_onset_sd_binned_", version_tag, "_", file_suffix, ".png")),
         p, width = 10, height = 10, dpi = 300)
}

# v1: onset_date4_p
plot_avg_onset_grid(grid_precip_data, 1979, 2019, "onset_date4_p", "v1", "1979_2019")
plot_sd_onset_grid(grid_precip_data, 1979, 2019, "onset_date4_p", "v1", "1979_2019")

# v2: onset_date4_p_final
plot_avg_onset_grid(grid_precip_data, 1979, 2019, "onset_date4_p_final", "v2", "1979_2019")
plot_sd_onset_grid(grid_precip_data, 1979, 2019, "onset_date4_p_final", "v2", "1979_2019")


plot_avg_onset_grid(grid_precip_data, 1940, 2024, "onset_date4_p", "v1", "1940_2024")
plot_sd_onset_grid(grid_precip_data, 1940, 2024, "onset_date4_p", "v1", "1940_2024")
plot_avg_onset_grid(grid_precip_data, 1940, 2024, "onset_date4_p_final", "v2", "1940_2024")
plot_sd_onset_grid(grid_precip_data, 1940, 2024, "onset_date4_p_final", "v2", "1940_2024")


plot_avg_onset_grid(grid_precip_data, 1997, 2022, "onset_date4_p", "v1", "1997_2022")
plot_sd_onset_grid(grid_precip_data, 1997, 2022, "onset_date4_p", "v1", "1997_2022")
plot_avg_onset_grid(grid_precip_data, 1997, 2022, "onset_date4_p_final", "v2", "1997_2022")
plot_sd_onset_grid(grid_precip_data, 1997, 2022, "onset_date4_p_final", "v2", "1997_2022")





ordinal_suffix <- function(n) {
  if (n %% 100 %in% 11:13) return(paste0(n, "th"))
  suffix <- c("st", "nd", "rd", rep("th", 7))
  return(paste0(n, suffix[n %% 10 + 1]))
}

plot_cluster_results <- function(df, year) {
  india_map <- rnaturalearth::ne_countries(scale = "medium", country = "India", returnclass = "sf")
  india_states <- rnaturalearth::ne_states(country = "India", returnclass = "sf")
  
  # Custom colors for clusters
  cluster_colors <- c(
    "1" = "green3",
    "2" = "gold",
    "3" = "gray50",
    "4" = "dodgerblue3",
    "5" = "firebrick3"
  )
  
  # Get unique grid cell-cluster combos
  grid_df <- df %>%
    dplyr::distinct(lat, lon, cluster) %>%
    dplyr::mutate(cluster = as.factor(cluster))
  
  disputed_points <- grid_precip_data %>% # this part is linked with the above grid_precip part (please import)
    dplyr::filter(s_name == "Jammu and Kashmir", is.na(d_name)) %>%
    dplyr::distinct(lon, lat)
  
  # Plot
  p <- ggplot(data = grid_df, mapping = aes(x = lon, y = lat, fill = cluster)) +
    geom_tile() +
    geom_tile(data = disputed_points, aes(x = lon, y = lat), fill = "gray95", inherit.aes = FALSE) +
    geom_sf(data = telangana_sf, fill = NA, color = "black", linewidth = 1, inherit.aes = FALSE) +
    scale_fill_manual(values = cluster_colors, name = "Cluster") +
    coord_sf(xlim = c(65, 100), ylim = c(5, 40), expand = FALSE) +
    theme_void(base_size = 14) +
    theme(
      legend.position = "right",
      legend.title = element_text(size = 13, face = "bold"),
      legend.text = element_text(size = 11),
      panel.grid = element_blank(),
      plot.title = element_blank(),
      plot.subtitle = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA)
    )
  
  ggsave(file.path(figure_output_dir, paste0("kmeans_cluster_map_grid_", year, ".png")),
         plot = p, width = 10, height = 6, dpi = 300)
  
  # Rainfall summary (mean & SD)
  cluster_summary <- df %>%
    group_by(cluster, date) %>%
    summarise(
      mean_rainfall = mean(filtered_precip, na.rm = TRUE),
      sd_rainfall = sd(filtered_precip, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(date = as.Date(paste0("2000-", date)))  # Dummy year for x-axis
  
  # Overall cluster line + ribbon plot
  p2 <- ggplot(cluster_summary, aes(x = date, y = mean_rainfall * 1000, color = as.factor(cluster))) +
    geom_line(linewidth = 1) +
    geom_ribbon(aes(
      ymin = (mean_rainfall - sd_rainfall) * 1000,
      ymax = (mean_rainfall + sd_rainfall) * 1000,
      fill = as.factor(cluster)
    ), alpha = 0.2, color = NA) +
    scale_color_manual(values = cluster_colors) +
    scale_fill_manual(values = cluster_colors) +
    scale_x_date(date_labels = "%b") +
    labs(x = "Month", y = "Rainfall (mm/day)") +
    theme(
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      axis.title = element_text(size = 13),
      axis.text = element_text(size = 11),
      legend.position = "none",
      plot.title = element_blank()
    )
  
  ggsave(file.path(figure_output_dir, paste0("rainfall_summary_", year, ".png")),
         plot = p2, width = 10, height = 6, dpi = 300)
  
  # Save summary plot
  ggsave(file.path(figure_output_dir, paste0("rainfall_summary_", year, ".png")),
         plot = p2, width = 10, height = 6, dpi = 300)
  
  # Save per-cluster line + ribbon plots
  for (cl in sort(unique(df$cluster))) {
    cluster_data <- cluster_summary %>% dplyr::filter(cluster == cl)
    
    p <- ggplot(cluster_data, aes(x = date, y = mean_rainfall * 1000)) +
      geom_line(color = cluster_colors[as.character(cl)], linewidth = 1) +
      geom_ribbon(aes(
        ymin = (mean_rainfall - sd_rainfall) * 1000,
        ymax = (mean_rainfall + sd_rainfall) * 1000
      ), fill = cluster_colors[as.character(cl)], alpha = 0.2) +
      scale_x_date(date_labels = "%b") +
      labs(
        title = paste0(ordinal_suffix(cl), " cluster"),
        x = "Month", y = "Rainfall (mm/day)"
      ) +
      theme(
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 11),
        legend.position = "none",
        plot.title = element_blank()
      )
    
    
    ggsave(file.path(figure_output_dir, paste0("rainfall_summary_cluster_", cl, "_", year, ".png")),
           plot = p, width = 6, height = 5, dpi = 300)
  }
}

df_cluster <- read_csv(file.path(clean_data_era5, "k_means_monsoon_1940_2024.csv"))
start_year_kmeans <- 1940
end_year_kmeans <- 2024
year_range <- paste0(start_year_kmeans, "-", end_year_kmeans)
plot_cluster_results(df_cluster, year_range)

