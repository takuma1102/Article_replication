# Spill over

# 1 prep: text file to csv ---------------------------------------------------------

# US census texts were converted to csv
# comment out if not needed after converting

# df2023text <- read_tsv('2023_Gaz_118CDs_national.txt')
# write_csv(df2023text, 'geo2023original.csv')

# after combining SS data
# geodf_2023updated <- read_csv('geo2023updated.csv')

# after combining population weighted data
geodf_2023updated <- read_csv('geo202332_updated_pw.csv')


# df2022text <- read_tsv('2022_Gaz_116CDs_national.txt')
# write_csv(df2022text, 'geo2022original.csv')

geodf_2022updated <- read_csv('geo201322_updated_pw.csv')

# df2012text <- read_tsv('Gaz_cd111_national.txt')
# write_csv(df2012text, 'geo2012original.csv')

geodf_2012updated <- read_csv('geo200312_updated_pw.csv')


## 1.1 (not used except for the last line) reading 2000 file --------------------------------------------------------------

# downloaded from IPUMS, not US Census Bureau


### 1.1.1 prep ------------------------------------------------------------



# Set environment variable for GDAL to regenerate .shx index files
# Sys.setenv(SHAPE_RESTORE_SHX = "YES")
# 
# # Read the shapefile
# geodf_2000raw <- st_read("US_cd106th_2000.shp")
# 
# # Print column names 
# print(names(geodf_2000raw))
# 
# # Check CRS if needed
# print(st_crs(geodf_2000raw))
# 
# # row
# print(nrow(geodf_2000raw))
# 
# # boundary
# print(st_bbox(geodf_2000raw))
# 
# # it turned out that this is projected coordinate system, not lat or long.
# # so, we have to convert to WGS84, and then, calculate centroid
# 
# # incorrect centroid example. this is based on the projection version.
# st_centroid(geodf_2000raw$geometry)
# st_coordinates(st_centroid(geodf_2000raw$geometry))
# 
# 

### 1.1.2 reading file ----------------------------------------------------
# 
# geodf_2000_fixed <- st_make_valid(geodf_2000raw) # have to do this
# gdf2000_wgs84 <- st_transform(geodf_2000_fixed, 4326)
# centroids2000 <- st_centroid(gdf2000_wgs84$geometry)
# coords <- st_coordinates(centroids2000)
# 
# gdf2000_wgs84$long <- coords[, "X"]
# gdf2000_wgs84$lat <- coords[, "Y"]
# 
# out2000 <- gdf2000_wgs84[, c("STATE", "DIST", "lat", "long")]
# # have to convert STATE to real state info later
# out2000 <- st_drop_geometry(out2000)
# write.csv(out2000, "cd106_centroids.csv", row.names = FALSE)
# 
# # check nrow
# nrow(out2000)
# head(out2000)
# 
# 
### 1.1.2A add state info -------------------------------------------------
# 
# 
# data("fips_codes", package = "tidycensus")
# mapdf_2000 <- fips_codes %>%
#   distinct(state_code, state) %>%
#     rename(STATE      = state_code,
#       state_abbr = state)
# 
# out2000$STATE <- sprintf("%02d", as.integer(out2000$STATE))
# 
# combined_geodf_2000 <- out2000 %>%
#   left_join(mapdf_2000, by = "STATE")
# 
# # summary(combined_geodf_2000)
# combined_geodf_2000_foranalysis <-  combined_geodf_2000 %>%
#   dplyr::select(state_abbr, DIST, lat, long) %>%
#   rename(USPS=state_abbr,
#          GEOID=DIST,
#          INTPTLAT=lat,
#          INTPTLONG=long)
# 
# # write_csv(combined_geodf_2000_foranalysis, 'geo2000original.csv')
# geodf_2000 <- read_csv('geo2000.csv')

# after combining SS data
# geodf_2000updated <- read_csv('geo2000updated.csv')


### 1.1.2B (used!) final line ---------------------------------------------


# after combining population weighted
geodf_2000updated <- read_csv('geo200002_updated_pw.csv')


# Use haversine and geo_distance_flat function to calculate distances



## 1.2 output --------------------------------------------------------------

distance_results_202324 <- geo_distance_flat(geodf_2023updated)
# distance_results_202324
# write.csv(distance_results_202324, "distance_results_202324.csv")

distance_results_201322 <- geo_distance_flat(geodf_2022updated)
# distance_results_201322


distance_results_200312 <- geo_distance_flat(geodf_2012updated)
# distance_results_200312

distance_results_200002 <- geo_distance_flat(geodf_2000updated)
# distance_results_200002

distance_results_combined <- rbind(distance_results_202324, distance_results_201322,
                                   distance_results_200312, distance_results_200002)

# write.csv(distance_results_combined, "distance_results_combined.csv")


# 2. Removed --------------------------------------------------------------


# 3. Removed --------------------------------------------------------------



# 4. descriptive --------------------------------------------------------

# be careful; it includes distance to all districts, including its original district
# also, no voting margin information on this stage, since the datasets are not combined here.

## 4.0 Basic Prep --------------------------------------------------------


ranked_distances_district <- distance_results_combined %>%
  group_by(origin_note) %>% # by shooting incidents
  arrange(distance_miles, .by_group = TRUE) %>%
  mutate(rank = row_number()) %>%
  ungroup()

top5_per_shooting <- ranked_distances_district %>%
  filter(rank <= 5)

rank_summary_distance <- top5_per_shooting %>%
  group_by(rank) %>%
  summarise(
    avg_distance_miles = mean(distance_miles, na.rm = TRUE),
    sd  = sd(distance_miles, na.rm = TRUE)
    # count_notes       = n_distinct(origin_note)
  ) %>%
  arrange(rank)

# rank_summary_distance



## 4.1 jitter plot -------------------------------------------------------


ggplot() +
  # no need of bars for average??
  # geom_col(  data = rank_summary_distance,
  #   aes(x = factor(rank), y = avg_distance_miles),
  #   width = 0.6,  fill = "gray50"
  # ) +
  geom_hline(
    yintercept = seq(25, 125, by = 25),
    linetype   = "dotted",
    color      = "gray50"
  ) +
  # jitter dot
  geom_jitter(data = top5_per_shooting,
              aes(x = factor(rank), y = distance_miles),
              width = 0.2,      size = 1.5,
              alpha = 0.6,    color = "steelblue"
  ) +
  labs(
    title    = "Average Distance from School Shooting Venues",
    subtitle = "Top 5 Closest House Districts; Unfiltered Whole Data",
    x        = "Rank in Distance",
    y        = "Distance (miles)",
    caption = "Including the district where a shooting happens."
  ) +
  scale_y_continuous(
    breaks = seq(0, 125, by = 25),
    limits = c(0, 150)
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(hjust = 0.5),
    plot.subtitle    = element_text(hjust = 0.5, size = 10),
    axis.title       = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )


# 5 Combine to dataset --------------------------------------------------


## 5.1 prep of distance file ---------------------------------------------

# after manual conversion
# be careful with ZZ and AL (0 district)

distance_revised <- read.csv("distance_results_combined_revised.csv",
                             stringsAsFactors = FALSE)

distance_revised2 <- distance_revised %>%
  tidyr::extract(origin_note,
                 into = c("year", "month"),
                 # using regex; skip state info (2 letters), 4-letter year and 1-2-letter for month
                 regex = "^[A-Z]{2}(\\d{4})(\\d{1,2})$",
                 convert = TRUE) %>%  # convert to number
  mutate(
    # convert info to new line, named dest_GEOID2
    dest_GEOID2 = case_when(
      # population weighted
      str_detect(dest_GEOID, "^[0-9]{1,2}pw$") ~ dest_GEOID,
      # already transformed
      str_detect(dest_GEOID, "^[0-9]{1,2}$")    ~ dest_GEOID,
      # state + district
      str_detect(dest_GEOID, "^[0-9]{3,4}$")    ~ str_sub(dest_GEOID, -2, -1),
      # ZZ (outside-district)
      str_detect(dest_GEOID, "^[0-9]{2}ZZ$")    ~ str_sub(dest_GEOID, -2, -1),
      # otherwise
      TRUE                                      ~ dest_GEOID
    )
  )

# write.csv(distance_revised2, "distance_revised2.csv")

# if no original csv file
# distance_revised2 <- read.csv("distance_revised2.csv",  stringsAsFactors = FALSE)


## 5.1A. osrm ------------------------------------------------------------

# it matters to use osrmTable()
# do not use osrmRoute one by one, which is too slow


### 5.1A.1 More efficient ver. ------------------------------------------------

# first, check
options(osrm.server = "https://router.project-osrm.org/")

# Use process_osrm_batch function fpr OSRM batch processing function with progress saving capability
 
# Function to resume processing from saved progress
resume_osrm_processing <- function(data, progress_data, batch_size, progress_file) {
  
  # Restore existing data
  all_results <- progress_data$completed_results
  unique_origins <- progress_data$unique_origins
  unique_destinations <- progress_data$unique_destinations
  last_batch <- progress_data$last_batch
  total_batches <- progress_data$total_batches
  batch_combinations <- progress_data$batch_combinations
  
  # Recreate batch data
  origin_batches <- split(unique_origins, ceiling(seq_len(nrow(unique_origins)) / batch_size))
  dest_batches <- split(unique_destinations, ceiling(seq_len(nrow(unique_destinations)) / batch_size))
  
  # Process remaining batches
  if(last_batch < total_batches) {
    for(batch_num in (last_batch + 1):total_batches) {
      origin_batch_idx <- batch_combinations$origin_batch[batch_num]
      dest_batch_idx <- batch_combinations$dest_batch[batch_num]
      
      cat(sprintf("Processing batch %d/%d... (resuming)\n", batch_num, total_batches))
      
      tryCatch({
        origins_sf <- st_as_sf(origin_batches[[origin_batch_idx]], 
                               coords = c("origin_lon", "origin_lat"), 
                               crs = 4326)
        destinations_sf <- st_as_sf(dest_batches[[dest_batch_idx]], 
                                    coords = c("dest_lon", "dest_lat"), 
                                    crs = 4326)
        
        osrm_result <- osrmTable(
          src = origins_sf,
          dst = destinations_sf,
          measure = c("distance", "duration")
        )
        
        origin_ids <- origin_batches[[origin_batch_idx]]$origin_id
        dest_ids <- dest_batches[[dest_batch_idx]]$dest_id
        
        batch_result <- data.frame()
        for(origin_idx in 1:length(origin_ids)) {
          for(dest_idx in 1:length(dest_ids)) {
            batch_result <- rbind(batch_result, data.frame(
              origin_idx = origin_ids[origin_idx],
              dest_idx = dest_ids[dest_idx],
              osrm_distance = osrm_result$distances[origin_idx, dest_idx],
              osrm_duration = osrm_result$durations[origin_idx, dest_idx]
            ))
          }
        }
        
        all_results[[batch_num]] <- batch_result
        
        if(batch_num %% 5 == 0) {
          progress_data$completed_results <- all_results
          progress_data$last_batch <- batch_num
          saveRDS(progress_data, progress_file)
          cat("  → Progress saved\n")
        }
        
        Sys.sleep(0.5)
        
      }, error = function(e) {
        cat("  → Error:", e$message, "\n")
      })
    }
  }
  
  final_result <- bind_rows(all_results)
  
  return(list(
    lookup_table = final_result,
    unique_origins = unique_origins,
    unique_destinations = unique_destinations
  ))
}

# Execute processing
cat("Starting OSRM processing...\n")
cat("Number of data rows:", nrow(distance_revised2), "\n")

# Check number of unique locations
num_unique_origins <- distance_revised2 %>% 
  distinct(origin_lon, origin_lat) %>% 
  nrow()
num_unique_destinations <- distance_revised2 %>% 
  distinct(dest_lon, dest_lat) %>% 
  nrow()

cat("Unique origins:", num_unique_origins, "\n")
cat("Unique destinations:", num_unique_destinations, "\n")
cat("Total combinations:", num_unique_origins * num_unique_destinations, "\n")

# Run processing
# have to comment out

# osrm_results <- process_osrm_batch(distance_revised2, batch_size = 50)
# some room to increase batch size from 50...?

# Save final results
# saveRDS(osrm_results, "osrm_results_final.rds")
cat("Processing complete! Final results saved.\n")

# summary(osrm_results$lookup_table)

# for after-comment-out
osrm_results <- readRDS("osrm_results_final.rds")

### 5.1A.2 Connect to the distance file -----------------------------------

distance_revised_osrm <- distance_revised2 %>%
  # giving source id
  left_join(
    osrm_results$unique_origins %>% 
      dplyr::select(origin_lon, origin_lat, origin_id),
    by = c("origin_lon", "origin_lat")
  ) %>%
  # destination id  
  left_join(
    osrm_results$unique_destinations %>% 
      dplyr::select(dest_lon, dest_lat, dest_id),
    by = c("dest_lon", "dest_lat")
  ) %>%
  left_join(
    osrm_results$lookup_table,
    by = c("origin_id" = "origin_idx", "dest_id" = "dest_idx")
  ) %>%
  dplyr::select(-origin_id, -dest_id) 

# summary(distance_revised_osrm)

distance_revised_osrm <- distance_revised_osrm %>% 
  mutate(osrm_distance_km = osrm_distance/1000,
         # osrmRoute returns km, but API batch-calculation returns meter. 
         osrm_distance_miles = osrm_distance_km * 0.621371) %>% 
  dplyr::select(-osrm_distance)

summary(distance_revised_osrm)

# write.csv(distance_revised_osrm, "distance_revised_osrm.csv", row.names = FALSE)



## 5.2 combine to the original SS dataset -----------------------------------------------------------

# glimpse(distance_revised2)

glimpse(data_s)
# checked that we have within_state_district var.
# table(data_s$within_state_district)
str(data_s$within_state_district) # character

# Step 1: prep of distance_revised2 or osrm version
distance_clean <- 
  # distance_revised2 %>%
  distance_revised_osrm %>%
  # exclude ZZ
  filter(!str_detect(dest_GEOID2, "ZZ")) %>%
  # extract district number from dest_GEOID2 and make new line
  mutate(
    district = case_when(
      dest_GEOID2 == "0" | dest_GEOID2 == "0pw" ~ "AL",
      str_detect(dest_GEOID2, "pw$") ~ str_pad(str_remove(dest_GEOID2, "pw$"), 2, pad = "0"),
      TRUE ~ str_pad(dest_GEOID2, 2, pad = "0")
    ),
    # flag pw by making a new line
    is_pw = str_detect(dest_GEOID2, "pw$")
  ) %>%
  dplyr::select(year, month, dest_USPS, district, # new column
                distance_miles, is_pw, osrm_duration, osrm_distance_km,  osrm_distance_miles) # no need to pick old dest_GEOID2 column

# Step 2: calculate minimum distances for each group
min_distances_shooting <- distance_clean %>%
  group_by(year, month, dest_USPS, district, is_pw) %>%
  # the minimum would be its district itself, but it is ok, since we will eventually exclude
  # districts with incidents
  
  # I grouped by is=pw, so we have two patterns of distance
  
  # if you want to pick the second closest distant
  # arrange(distance_miles) %>%  
  # mutate(rank = row_number()) %>%  # specify rank
  # filter(rank == 2) %>%  
  # summarise(min_distance_shooting = first(distance_miles), .groups = "drop")
  
  summarise(min_distance_shooting = min(distance_miles, na.rm = TRUE), 
            min_osrmdistance_shooting = min(osrm_distance_miles, na.rm = TRUE),
            min_minutes_shooting = min(osrm_duration, na.rm = TRUE),
            .groups = "drop")

# Step 3: separate pw and non-pw distances
# Non-pw distances
nonpw_distances_shooting <- min_distances_shooting %>%
  filter(!is_pw) %>% # no pw
  dplyr::select(year, month, state = dest_USPS, within_state_district = district, 
                incident_spillover_distance_nonpw = min_distance_shooting,
                incident_spillover_osrmdistance_nonpw = min_osrmdistance_shooting, 
                incident_spillover_minute_nonpw =min_minutes_shooting)

# pw distances
pw_distances_shooting <- min_distances_shooting %>%
  filter(is_pw) %>%
  dplyr::select(year, month, state = dest_USPS, within_state_district = district, 
                incident_spillover_distance_pw = min_distance_shooting,
                incident_spillover_osrmdistance_pw = min_osrmdistance_shooting, 
                incident_spillover_minute_pw =min_minutes_shooting)

# Step 4: merge with original data
merged_spillover_data <- data_s %>%
  # merge non-pw distances
  left_join(nonpw_distances_shooting, 
            by = c("year", "month", "state", "within_state_district")) %>%
  # merge pw distances
  left_join(pw_distances_shooting, 
            by = c("year", "month", "state", "within_state_district")) %>% 
  
  # Step 5: exclude districts with incident to identify only distance between other districts' shooting.
  # converting them to NAs
  # and then, later, we have to exclude this. but also the exclusion should last for 24 months
  mutate(
    incident_spillover_distance_nonpw = ifelse(incident_count >= 1, NA_real_, incident_spillover_distance_nonpw),
    incident_spillover_distance_pw = ifelse(incident_count >= 1, NA_real_, incident_spillover_distance_pw)
  )

# if no shooting cases nationally, it will be also NA.



### 5.2.1 Check matching --------------------------------------------------

dim(data_s)
dim(merged_spillover_data)
# two new columns

summary(merged_spillover_data$incident_spillover_distance_nonpw)
summary(merged_spillover_data$incident_spillover_distance_pw)

# matching ratio
nonpw_match_rate <- mean(!is.na(merged_spillover_data$incident_spillover_distance_nonpw)) * 100
nonpw_match_rate
# 42%

table(data_s$incident)
(2192*435)/163950


### 5.2.2 new incident var. -----------------------------------------------


#### 5.2.2.1 Direct distance -----------------------------------------------


# STEP1
# first, 0-X miles

merged_spillover_data <- merged_spillover_data %>%
  mutate(
    # Non-pw distances
    # NAs should be transformed into 0.
  
    incident_dist_nonpw_10 = ifelse(!is.na(incident_spillover_distance_nonpw) & 
                                      incident_spillover_distance_nonpw <= 10, 1, 0),
    incident_dist_nonpw_30 = ifelse(!is.na(incident_spillover_distance_nonpw) & 
                                      incident_spillover_distance_nonpw <= 30, 1, 0),
    incident_dist_nonpw_50 = ifelse(!is.na(incident_spillover_distance_nonpw) & 
                                      incident_spillover_distance_nonpw <= 50, 1, 0),
    incident_dist_nonpw_75 = ifelse(!is.na(incident_spillover_distance_nonpw) & 
                                      incident_spillover_distance_nonpw <= 75, 1, 0),
    incident_dist_nonpw_100 = ifelse(!is.na(incident_spillover_distance_nonpw) & 
                                       incident_spillover_distance_nonpw <= 100, 1, 0),
    
    # PW distances
  
    incident_dist_pw_10 = ifelse(!is.na(incident_spillover_distance_pw) & 
                                   incident_spillover_distance_pw <= 10, 1, 0),
    incident_dist_pw_30 = ifelse(!is.na(incident_spillover_distance_pw) & 
                                   incident_spillover_distance_pw <= 30, 1, 0),
    incident_dist_pw_50 = ifelse(!is.na(incident_spillover_distance_pw) & 
                                   incident_spillover_distance_pw <= 50, 1, 0),
    incident_dist_pw_75 = ifelse(!is.na(incident_spillover_distance_pw) & 
                                   incident_spillover_distance_pw <= 75, 1, 0),
    incident_dist_pw_100 = ifelse(!is.na(incident_spillover_distance_pw) & 
                                    incident_spillover_distance_pw <= 100, 1, 0)
  )

# avoid doing 10 same steps
spillover_distance_thresholds <- c(10, 30, 50, 75, 100)
spillover_distance_types <- c("nonpw", "pw")

merged_spillover_data <- merged_spillover_data %>%
  arrange(id, time) %>%
  group_by(id)

for (dist_type in spillover_distance_types) {
  for (threshold in spillover_distance_thresholds) {
    
    incident_var <- paste0("incident_dist_", dist_type, "_", threshold)
    window_var <- paste0("incident_window_", dist_type, "_", threshold)
    last_incident_var <- paste0("last_incident_time_", dist_type, "_", threshold)
    
    merged_spillover_data <- merged_spillover_data %>%
      mutate(
        # use dynamic operator
        !!last_incident_var := ifelse(.data[[incident_var]] == 1, time, NA)
      ) %>%
      fill(all_of(last_incident_var), .direction = "down") %>%
      mutate(
        !!window_var := ifelse(
          !is.na(.data[[last_incident_var]]) & 
            (time - .data[[last_incident_var]] <= 24) & 
            (time >= .data[[last_incident_var]]), 1, 0
        ),
        !!window_var := as.integer(.data[[window_var]])
      )
  }
}

merged_spillover_data <- merged_spillover_data %>%
  ungroup()


# STEP2
# next, donut shape; X-Y miles

merged_spillover_data <- merged_spillover_data %>%
  mutate(
    ncident_window_nonpw_1030 = ifelse(incident_window_nonpw_30 == 1 & incident_window_nonpw_10 == 0, 1, 0),
    incident_window_nonpw_3050 = ifelse(incident_window_nonpw_50 == 1 & incident_window_nonpw_30 == 0, 1, 0),
    incident_window_nonpw_5075 = ifelse(incident_window_nonpw_75 == 1 & incident_window_nonpw_50 == 0, 1, 0),
    incident_window_nonpw_50100 = ifelse(incident_window_nonpw_100 == 1 & incident_window_nonpw_50 == 0, 1, 0),
    incident_window_nonpw_75100 = ifelse(incident_window_nonpw_100 == 1 & incident_window_nonpw_75 == 0, 1, 0),
      
    # pw
    incident_window_pw_1030 = ifelse(incident_window_pw_30 == 1 & incident_window_pw_10 == 0, 1, 0),
    incident_window_pw_3050 = ifelse(incident_window_pw_50 == 1 & incident_window_pw_30 == 0, 1, 0),
    incident_window_pw_5075 = ifelse(incident_window_pw_75 == 1 & incident_window_pw_50 == 0, 1, 0),
    incident_window_pw_50100 = ifelse(incident_window_pw_100 == 1 & incident_window_pw_50 == 0, 1, 0),
    incident_window_pw_75100 = ifelse(incident_window_pw_100 == 1 & incident_window_pw_75 == 0, 1, 0),
    
    # STEP 3 
    # categorical var. for all-in pattern
    # non-pw
    distance_band_nonpw_original = case_when(
      incident_window_nonpw_10   == 1 ~ "0_10",
     incident_window_nonpw_1030 == 1 ~ "10_30",
      incident_window_nonpw_30   == 1 ~ "0_30",
      incident_window_nonpw_3050 == 1 ~ "30_50",
      incident_window_nonpw_50 == 1 ~ "0_50",
      incident_window_nonpw_5075 == 1 ~ "50_75",
      incident_window_nonpw_75 == 1 ~ "0_75",
      incident_window_nonpw_75100== 1 ~ "75_100",
      incident_window_nonpw_50100== 1 ~ "50_100",
      incident_window_nonpw_100== 1 ~ "0_100",
      TRUE                            ~ "far"      
    ),
    distance_band_nonpw_granular = factor(distance_band_nonpw_original, levels = c("far","0_10","10_30", "30_50",
                                                                                  "50_75","75_100"
                                                                                   
    )),
    distance_band_nonpw_broad = factor(distance_band_nonpw_original, levels = c("far","0_50","50_100"
                                                                                
    )),
    
    # population weighted
    distance_band_pw_original = case_when(
      incident_window_pw_10   == 1 ~ "0_10",
      ncident_window_pw_30   == 1 ~ "0_30",
      incident_window_pw_3050 == 1 ~ "30_50",
      incident_window_pw_50 == 1 ~ "0_50",
      incident_window_pw_5075 == 1 ~ "50_75",
      incident_window_pw_75 == 1 ~ "0_75",
      incident_window_pw_75100== 1 ~ "75_100",
      incident_window_pw_50100== 1 ~ "50_100",
      incident_window_pw_100== 1 ~ "0_100",
      TRUE                            ~ "far"      
    ),
    distance_band_pw_granular = factor(distance_band_pw_original, levels = c("far","0_10","10-30","30_50",
                                                                            "50_75","75_100"
                                                                             
    )),
    distance_band_pw_broad = factor(distance_band_pw_original, levels = c("far","0_50","50_100"
                                                                          
    )),
    
  )


# glimpse(merged_spillover_data)

# STEP 4
# continuous distance, but not working out??? since this is for heterogeneity of treatment effect
# based on continuous var. However, in this context, we don't assume continuous var., which is distance,
# as a control group. (every thing is treated)

# but, although this cannot be used for continuous analysis, it might be used for other purposes,
# such as setting the farther pattern. 

# past threshold was 200, as a placeholder, but since this should be renewed once a new relevant
# incident happens, 200 is not appropriate.

merged_spillover_data <- merged_spillover_data %>%
  arrange(id, time) %>%
  group_by(id) %>%
  # record both time AND distance when a shooting happens close to a district
  
  # since this should be renewed only when a new good incident happens, we have multiple var.
  
  # be careful with two thresholds; threshold here and threshold to exclude
  
 
  
  mutate(last_shoot_time100_nonpw = ifelse(incident_spillover_distance_nonpw <= 100,
                                           time, NA_real_),
         last_shoot_time100_pw = ifelse(incident_spillover_distance_pw <= 100,
                                        time, NA_real_),
         last_shoot_dist100_nonpw = ifelse(incident_spillover_distance_nonpw <= 100,
                                           incident_spillover_distance_nonpw, NA_real_),
         last_shoot_dist100_pw = ifelse(incident_spillover_distance_pw <= 100,
                                        incident_spillover_distance_pw, NA_real_)
  ) %>%
  # copy and paste those info
  
  fill(last_shoot_time100_nonpw, last_shoot_dist100_nonpw,
       last_shoot_time100_pw, last_shoot_dist100_pw,
       .direction = "down") %>% 
  
  # only lasting for 24 months
  mutate(
    dist_follow_100_nonpw = ifelse(
      !is.na(last_shoot_time100_nonpw) &
        (time - last_shoot_time100_nonpw <= 24),
      last_shoot_dist100_nonpw,
      NA_real_
    ),
    dist_follow_100_pw = ifelse(
      !is.na(last_shoot_time100_pw) &
        (time - last_shoot_time100_pw <= 24),
      last_shoot_dist100_pw,
      NA_real_
    )
  ) %>%
  
  mutate(last_shoot_time75_nonpw = ifelse(incident_spillover_distance_nonpw <= 75,
                                          time, NA_real_),
         last_shoot_time75_pw = ifelse(incident_spillover_distance_pw <= 75,
                                       time, NA_real_),
         last_shoot_dist75_nonpw = ifelse(incident_spillover_distance_nonpw <= 75,
                                          incident_spillover_distance_nonpw, NA_real_),
         last_shoot_dist75_pw = ifelse(incident_spillover_distance_pw <= 75,
                                       incident_spillover_distance_pw, NA_real_)
  ) %>%
  # copy and paste those info
  
  fill(last_shoot_time75_nonpw, last_shoot_dist75_nonpw,
       last_shoot_time75_pw, last_shoot_dist75_pw,
       .direction = "down") %>% 
  
  # only lasting for 24 months
  mutate(
    dist_follow_75_nonpw = ifelse(
      !is.na(last_shoot_time75_nonpw) &
        (time - last_shoot_time75_nonpw <= 24),
      last_shoot_dist75_nonpw,
      NA_real_
    ),
    dist_follow_75_pw = ifelse(
      !is.na(last_shoot_time75_pw) &
        (time - last_shoot_time75_pw <= 24),
      last_shoot_dist75_pw,
      NA_real_
    )
  ) %>%
  
  mutate(last_shoot_time50_nonpw = ifelse(incident_spillover_distance_nonpw <= 50,
                                          time, NA_real_),
         last_shoot_time50_pw = ifelse(incident_spillover_distance_pw <= 50,
                                       time, NA_real_),
         last_shoot_dist50_nonpw = ifelse(incident_spillover_distance_nonpw <= 50,
                                          incident_spillover_distance_nonpw, NA_real_),
         last_shoot_dist50_pw = ifelse(incident_spillover_distance_pw <= 50,
                                       incident_spillover_distance_pw, NA_real_)
  ) %>%
  # copy and paste those info
  
  fill(last_shoot_time50_nonpw, last_shoot_dist50_nonpw,
       last_shoot_time50_pw, last_shoot_dist50_pw,
       .direction = "down") %>% 
  
  # only lasting for 24 months
  mutate(
    dist_follow_50_nonpw = ifelse(
      !is.na(last_shoot_time50_nonpw) &
        (time - last_shoot_time50_nonpw <= 24),
      last_shoot_dist50_nonpw,
      NA_real_
    ),
    dist_follow_50_pw = ifelse(
      !is.na(last_shoot_time50_pw) &
        (time - last_shoot_time50_pw <= 24),
      last_shoot_dist50_pw,
      NA_real_
    )
  ) %>%
  
  mutate(last_shoot_time30_nonpw = ifelse(incident_spillover_distance_nonpw <= 30,
                                          time, NA_real_),
         last_shoot_time30_pw = ifelse(incident_spillover_distance_pw <= 30,
                                       time, NA_real_),
         last_shoot_dist30_nonpw = ifelse(incident_spillover_distance_nonpw <= 30,
                                          incident_spillover_distance_nonpw, NA_real_),
         last_shoot_dist30_pw = ifelse(incident_spillover_distance_pw <= 30,
                                       incident_spillover_distance_pw, NA_real_)
  ) %>%
  # copy and paste those info
  
  fill(last_shoot_time30_nonpw, last_shoot_dist30_nonpw,
       last_shoot_time30_pw, last_shoot_dist30_pw,
       .direction = "down") %>% 
  
  # only lasting for 24 months
  mutate(
    dist_follow_30_nonpw = ifelse(
      !is.na(last_shoot_time30_nonpw) &
        (time - last_shoot_time30_nonpw <= 24),
      last_shoot_dist30_nonpw,
      NA_real_
    ),
    dist_follow_30_pw = ifelse(
      !is.na(last_shoot_time30_pw) &
        (time - last_shoot_time30_pw <= 24),
      last_shoot_dist30_pw,
      NA_real_
    )
  ) %>%
  
  mutate(last_shoot_time10_nonpw = ifelse(incident_spillover_distance_nonpw <= 10,
                                          time, NA_real_),
         last_shoot_time10_pw = ifelse(incident_spillover_distance_pw <= 10,
                                       time, NA_real_),
         last_shoot_dist10_nonpw = ifelse(incident_spillover_distance_nonpw <= 10,
                                          incident_spillover_distance_nonpw, NA_real_),
         last_shoot_dist10_pw = ifelse(incident_spillover_distance_pw <= 10,
                                       incident_spillover_distance_pw, NA_real_)
  ) %>%
  # copy and paste those info
  
  fill(last_shoot_time10_nonpw, last_shoot_dist10_nonpw,
       last_shoot_time10_pw, last_shoot_dist10_pw,
       .direction = "down") %>% 
  
  # only lasting for 24 months
  mutate(
    dist_follow_10_nonpw = ifelse(
      !is.na(last_shoot_time10_nonpw) &
        (time - last_shoot_time10_nonpw <= 24),
      last_shoot_dist10_nonpw,
      NA_real_
    ),
    dist_follow_10_pw = ifelse(
      !is.na(last_shoot_time10_pw) &
        (time - last_shoot_time10_pw <= 24),
      last_shoot_dist10_pw,
      NA_real_
    )
  ) %>% 
  
  ungroup()


# be careful and distinguish incident_window_pw_30, incident_spillover_distance_pw, dist_follow_200_pw
# the first one is a binary and "window" lasting for 24 months after a shooting in neighborhood
# the second one is a monthly-basis minimum distance for each district
# the third one is a continuous version of the first binary var.


#### 5.2.2.2 OSRM distance --------------------------------------------------

# first, check
summary(merged_spillover_data$incident_spillover_osrmdistance_nonpw)
# have to check km or meter, or mile

hist(merged_spillover_data$incident_spillover_osrmdistance_nonpw)

# STEP1
# first, 0-X miles

merged_spillover_data <- merged_spillover_data %>%
  mutate(
    incident_dist_nonpw_osrm10 = ifelse(!is.na(incident_spillover_osrmdistance_nonpw) & 
                                          incident_spillover_osrmdistance_nonpw <= 10, 1, 0),
    
   incident_dist_nonpw_osrm30 = ifelse(!is.na(incident_spillover_osrmdistance_nonpw) & 
                                          incident_spillover_osrmdistance_nonpw <= 30, 1, 0),
    
    incident_dist_nonpw_osrm50 = ifelse(!is.na(incident_spillover_osrmdistance_nonpw) & 
                                          incident_spillover_osrmdistance_nonpw <= 50, 1, 0),
    incident_dist_nonpw_osrm75 = ifelse(!is.na(incident_spillover_osrmdistance_nonpw) & 
                                          incident_spillover_osrmdistance_nonpw <= 75, 1, 0),
    incident_dist_nonpw_osrm100 = ifelse(!is.na(incident_spillover_osrmdistance_nonpw) & 
                                           incident_spillover_osrmdistance_nonpw <= 100, 1, 0),
    
    
    # PW distances
    incident_dist_pw_osrm10 = ifelse(!is.na(incident_spillover_osrmdistance_pw) & 
                                       incident_spillover_osrmdistance_pw <= 10, 1, 0),
    incident_dist_pw_osrm30 = ifelse(!is.na(incident_spillover_osrmdistance_pw) & 
                                       incident_spillover_osrmdistance_pw <= 30, 1, 0),
    incident_dist_pw_osrm50 = ifelse(!is.na(incident_spillover_osrmdistance_pw) & 
                                       incident_spillover_osrmdistance_pw <= 50, 1, 0),
    incident_dist_pw_osrm75 = ifelse(!is.na(incident_spillover_osrmdistance_pw) & 
                                       incident_spillover_osrmdistance_pw <= 75, 1, 0),
    incident_dist_pw_osrm100 = ifelse(!is.na(incident_spillover_osrmdistance_pw) & 
                                        incident_spillover_osrmdistance_pw <= 100, 1, 0),
   
  )

# avoid doing 10 same steps
spillover_distance_thresholds <- c(10, 30, 50, 75, 100)
spillover_distance_types <- c("nonpw", "pw")

merged_spillover_data <- merged_spillover_data %>%
  arrange(id, time) %>%
  group_by(id)

for (dist_type in spillover_distance_types) {
  for (threshold in spillover_distance_thresholds) {
    
    incident_var <- paste0("incident_dist_", dist_type, "_osrm", threshold)
    window_var <- paste0("incident_window_", dist_type, "_osrm", threshold)
    last_incident_var <- paste0("last_incident_time_", dist_type, "_osrm", threshold)
    
    merged_spillover_data <- merged_spillover_data %>%
      mutate(
        # use dynamic operator
        !!last_incident_var := ifelse(.data[[incident_var]] == 1, time, NA)
      ) %>%
      fill(all_of(last_incident_var), .direction = "down") %>%
      mutate(
        !!window_var := ifelse(
          !is.na(.data[[last_incident_var]]) & 
            (time - .data[[last_incident_var]] <= 24) & 
            (time >= .data[[last_incident_var]]), 1, 0
        ),
        !!window_var := as.integer(.data[[window_var]])
      )
  }
}

merged_spillover_data <- merged_spillover_data %>%
  ungroup()


# STEP2
# next, donut shape; X-Y miles

merged_spillover_data <- merged_spillover_data %>%
  mutate(
    incident_window_nonpw_osrm1030 = ifelse(incident_window_nonpw_osrm30 == 1 & incident_window_nonpw_osrm10 == 0, 1, 0),
    incident_window_nonpw_osrm3050 = ifelse(incident_window_nonpw_osrm50 == 1 & incident_window_nonpw_osrm30 == 0, 1, 0),
    incident_window_nonpw_osrm5075 = ifelse(incident_window_nonpw_osrm75 == 1 & incident_window_nonpw_osrm50 == 0, 1, 0),
    incident_window_nonpw_osrm50100 = ifelse(incident_window_nonpw_osrm100 == 1 & incident_window_nonpw_osrm50 == 0, 1, 0),
    incident_window_nonpw_osrm75100 = ifelse(incident_window_nonpw_osrm100 == 1 & incident_window_nonpw_osrm75 == 0, 1, 0),
    
    # pw
    incident_window_pw_osrm1030 = ifelse(incident_window_pw_osrm30 == 1 & incident_window_pw_osrm10 == 0, 1, 0),
    incident_window_pw_osrm3050 = ifelse(incident_window_pw_osrm50 == 1 & incident_window_pw_osrm30 == 0, 1, 0),
    incident_window_pw_osrm5075 = ifelse(incident_window_pw_osrm75 == 1 & incident_window_pw_osrm50 == 0, 1, 0),
    incident_window_pw_osrm50100 = ifelse(incident_window_pw_osrm100 == 1 & incident_window_pw_osrm50 == 0, 1, 0),
    incident_window_pw_osrm75100 = ifelse(incident_window_pw_osrm100 == 1 & incident_window_pw_osrm75 == 0, 1, 0),
    
    # STEP 3 
    # categorical var. for all-in pattern
    # non-pw
    osrmdistance_band_nonpw_original = case_when(
    incident_window_nonpw_osrm10   == 1 ~ "0_10",
      incident_window_nonpw_osrm1030 == 1 ~ "10_30",
      incident_window_nonpw_osrm3050 == 1 ~ "30_50",
      incident_window_nonpw_osrm50 == 1 ~ "0_50",
      incident_window_nonpw_osrm5075 == 1 ~ "50_75",
      incident_window_nonpw_osrm75 == 1 ~ "0_75",
      incident_window_nonpw_osrm75100== 1 ~ "75_100",
      incident_window_nonpw_osrm50100== 1 ~ "50_100",
      incident_window_nonpw_osrm100== 1 ~ "0_100",
      TRUE                            ~ "far"      
    ),

    osrmdistance_band_nonpw_granular = factor(osrmdistance_band_nonpw_original, levels = c("far","0_10","10_30","30_50",
                                                                                          
                                                                                           "50_75","75_100"
                                                                                          
    )),
    osrmdistance_band_nonpw_broad = factor(osrmdistance_band_nonpw_original, levels = c("far","0_50","50_100"
                                                                                       
    )),
    
    # population weighted
    osrmdistance_band_pw_original = case_when(
      incident_window_pw_osrm10   == 1 ~ "0_10",
      incident_window_pw_osrm1030 == 1 ~ "10_30",
      incident_window_pw_osrm30   == 1 ~ "0_30",
      incident_window_pw_osrm3050 == 1 ~ "30_50",
      incident_window_pw_osrm50 == 1 ~ "0_50",
      incident_window_pw_osrm5075 == 1 ~ "50_75",
      incident_window_pw_osrm75 == 1 ~ "0_75",
      incident_window_pw_osrm75100== 1 ~ "75_100",
      incident_window_pw_osrm50100== 1 ~ "50_100",
      incident_window_pw_osrm100== 1 ~ "0_100",
     
      TRUE                            ~ "far"      
    ),
    osrmdistance_band_pw_granular = factor(osrmdistance_band_pw_original, levels = c("far","0_10","10_30","30_50",
                                                                                    
                                                                                     "50_75","75_100"
                                                                                     
    )),
    osrmdistance_band_pw_broad = factor(osrmdistance_band_pw_original, levels = c("far","0_50","50_100"
                                                                                 
    )),
    
  )


# STEP 4
# continuous distance
# threshold is now 200, as a placeholder

merged_spillover_data <- merged_spillover_data %>%
  arrange(id, time) %>%
  group_by(id) %>%
  # record both time AND distance when a shooting happens close to a district
  
 
  
  
  mutate(last_shoot_time100_osrmnonpw = ifelse(incident_spillover_osrmdistance_nonpw <= 100,
                                               time, NA_real_),
         last_shoot_time100_osrmpw = ifelse(incident_spillover_osrmdistance_pw <= 100,
                                            time, NA_real_),
         last_shoot_dist100_osrmnonpw = ifelse(incident_spillover_osrmdistance_nonpw <= 100,
                                               incident_spillover_osrmdistance_nonpw, NA_real_),
         last_shoot_dist100_osrmpw = ifelse(incident_spillover_osrmdistance_pw <= 100,
                                            incident_spillover_osrmdistance_pw, NA_real_)
  ) %>%
  # copy and paste those info
  
  fill(last_shoot_time100_osrmnonpw, last_shoot_dist100_osrmnonpw,
       last_shoot_time100_osrmpw, last_shoot_dist100_osrmpw,
       .direction = "down") %>%
  
  # only lasting for 24 months
  mutate(
    dist_follow_100_osrmnonpw = ifelse(
      !is.na(last_shoot_time100_osrmnonpw) &
        (time - last_shoot_time100_osrmnonpw <= 24),
      last_shoot_dist100_osrmnonpw,
      NA_real_
    ),
    dist_follow_100_osrmpw = ifelse(
      !is.na(last_shoot_time100_osrmpw) &
        (time - last_shoot_time100_osrmpw <= 24),
      last_shoot_dist100_osrmpw,
      NA_real_
    )
  ) %>%
  
  mutate(last_shoot_time75_osrmnonpw = ifelse(incident_spillover_osrmdistance_nonpw <= 75,
                                              time, NA_real_),
         last_shoot_time75_osrmpw = ifelse(incident_spillover_osrmdistance_pw <= 75,
                                           time, NA_real_),
         last_shoot_dist75_osrmnonpw = ifelse(incident_spillover_osrmdistance_nonpw <= 75,
                                              incident_spillover_osrmdistance_nonpw, NA_real_),
         last_shoot_dist75_osrmpw = ifelse(incident_spillover_osrmdistance_pw <= 75,
                                           incident_spillover_osrmdistance_pw, NA_real_)
  ) %>%
  # copy and paste those info
  
  fill(last_shoot_time75_osrmnonpw, last_shoot_dist75_osrmnonpw,
       last_shoot_time75_osrmpw, last_shoot_dist75_osrmpw,
       .direction = "down") %>%
  
  # only lasting for 24 months
  mutate(
    dist_follow_75_osrmnonpw = ifelse(
      !is.na(last_shoot_time75_osrmnonpw) &
        (time - last_shoot_time75_osrmnonpw <= 24),
      last_shoot_dist75_osrmnonpw,
      NA_real_
    ),
    dist_follow_75_osrmpw = ifelse(
      !is.na(last_shoot_time75_osrmpw) &
        (time - last_shoot_time75_osrmpw <= 24),
      last_shoot_dist75_osrmpw,
      NA_real_
    )
  ) %>%
  
  mutate(last_shoot_time50_osrmnonpw = ifelse(incident_spillover_osrmdistance_nonpw <= 50,
                                              time, NA_real_),
         last_shoot_time50_osrmpw = ifelse(incident_spillover_osrmdistance_pw <= 50,
                                           time, NA_real_),
         last_shoot_dist50_osrmnonpw = ifelse(incident_spillover_osrmdistance_nonpw <= 50,
                                              incident_spillover_osrmdistance_nonpw, NA_real_),
         last_shoot_dist50_osrmpw = ifelse(incident_spillover_osrmdistance_pw <= 50,
                                           incident_spillover_osrmdistance_pw, NA_real_)
  ) %>%
  # copy and paste those info
  
  fill(last_shoot_time50_osrmnonpw, last_shoot_dist50_osrmnonpw,
       last_shoot_time50_osrmpw, last_shoot_dist50_osrmpw,
       .direction = "down") %>%
  
  # only lasting for 24 months
  mutate(
    dist_follow_50_osrmnonpw = ifelse(
      !is.na(last_shoot_time50_osrmnonpw) &
        (time - last_shoot_time50_osrmnonpw <= 24),
      last_shoot_dist50_osrmnonpw,
      NA_real_
    ),
    dist_follow_50_osrmpw = ifelse(
      !is.na(last_shoot_time50_osrmpw) &
        (time - last_shoot_time50_osrmpw <= 24),
      last_shoot_dist50_osrmpw,
      NA_real_
    )
  ) %>%
  
  mutate(last_shoot_time30_osrmnonpw = ifelse(incident_spillover_osrmdistance_nonpw <= 30,
                                              time, NA_real_),
         last_shoot_time30_osrmpw = ifelse(incident_spillover_osrmdistance_pw <= 30,
                                           time, NA_real_),
         last_shoot_dist30_osrmnonpw = ifelse(incident_spillover_osrmdistance_nonpw <= 30,
                                              incident_spillover_osrmdistance_nonpw, NA_real_),
         last_shoot_dist30_osrmpw = ifelse(incident_spillover_osrmdistance_pw <= 30,
                                           incident_spillover_osrmdistance_pw, NA_real_)
  ) %>%
  # copy and paste those info
  
  fill(last_shoot_time30_osrmnonpw, last_shoot_dist30_osrmnonpw,
       last_shoot_time30_osrmpw, last_shoot_dist30_osrmpw,
       .direction = "down") %>%
  
  # only lasting for 24 months
  mutate(
    dist_follow_30_osrmnonpw = ifelse(
      !is.na(last_shoot_time30_osrmnonpw) &
        (time - last_shoot_time30_osrmnonpw <= 24),
      last_shoot_dist30_osrmnonpw,
      NA_real_
    ),
    dist_follow_30_osrmpw = ifelse(
      !is.na(last_shoot_time30_osrmpw) &
        (time - last_shoot_time30_osrmpw <= 24),
      last_shoot_dist30_osrmpw,
      NA_real_
    )
  ) %>%
  
  
 
  
  
  mutate(last_shoot_time10_osrmnonpw = ifelse(incident_spillover_osrmdistance_nonpw <= 10,
                                              time, NA_real_),
         last_shoot_time10_osrmpw = ifelse(incident_spillover_osrmdistance_pw <= 10,
                                           time, NA_real_),
         last_shoot_dist10_osrmnonpw = ifelse(incident_spillover_osrmdistance_nonpw <= 10,
                                              incident_spillover_osrmdistance_nonpw, NA_real_),
         last_shoot_dist10_osrmpw = ifelse(incident_spillover_osrmdistance_pw <= 10,
                                           incident_spillover_osrmdistance_pw, NA_real_)
  ) %>%
  # copy and paste those info
  
  fill(last_shoot_time10_osrmnonpw, last_shoot_dist10_osrmnonpw,
       last_shoot_time10_osrmpw, last_shoot_dist10_osrmpw,
       .direction = "down") %>%
  
  # only lasting for 24 months
  mutate(
    dist_follow_10_osrmnonpw = ifelse(
      !is.na(last_shoot_time10_osrmnonpw) &
        (time - last_shoot_time10_osrmnonpw <= 24),
      last_shoot_dist10_osrmnonpw,
      NA_real_
    ),
    dist_follow_10_osrmpw = ifelse(
      !is.na(last_shoot_time10_osrmpw) &
        (time - last_shoot_time10_osrmpw <= 24),
      last_shoot_dist10_osrmpw,
      NA_real_
    )
  ) %>%
  
 
  ungroup()

summary(merged_spillover_data$dist_follow_100_osrmpw)

#### 5.2.2.3 OSRM minute --------------------------------------------------

# summary(merged_spillover_data$incident_spillover_minute_nonpw)

# STEP1
# first, 0-X minutes

merged_spillover_data <- merged_spillover_data %>%
  mutate(
    incident_time_nonpw_osrm10 = ifelse(!is.na(incident_spillover_minute_nonpw) & 
                                          incident_spillover_minute_nonpw <= 10, 1, 0),
    
    incident_time_nonpw_osrm30 = ifelse(!is.na(incident_spillover_minute_nonpw) & 
                                          incident_spillover_minute_nonpw <= 30, 1, 0),
    
    incident_time_nonpw_osrm50 = ifelse(!is.na(incident_spillover_minute_nonpw) & 
                                          incident_spillover_minute_nonpw <= 50, 1, 0),
    incident_time_nonpw_osrm60 = ifelse(!is.na(incident_spillover_minute_nonpw) & 
                                          incident_spillover_minute_nonpw <=60, 1, 0),
    incident_time_nonpw_osrm75 = ifelse(!is.na(incident_spillover_minute_nonpw) & 
                                          incident_spillover_minute_nonpw <= 75, 1, 0),
    incident_time_nonpw_osrm90 = ifelse(!is.na(incident_spillover_minute_nonpw) & 
                                          incident_spillover_minute_nonpw <=90, 1, 0),
    incident_time_nonpw_osrm100 = ifelse(!is.na(incident_spillover_minute_nonpw) & 
                                           incident_spillover_minute_nonpw <= 100, 1, 0),
    incident_time_nonpw_osrm120 = ifelse(!is.na(incident_spillover_minute_nonpw) & 
                                           incident_spillover_minute_nonpw <=120, 1, 0),
    incident_time_nonpw_osrm180 = ifelse(!is.na(incident_spillover_minute_nonpw) & 
                                           incident_spillover_minute_nonpw <=180, 1, 0),
    
    
    # PW distances
    incident_time_pw_osrm10 = ifelse(!is.na(incident_spillover_minute_pw) & 
                                       incident_spillover_minute_pw <= 10, 1, 0),
   incident_time_pw_osrm30 = ifelse(!is.na(incident_spillover_minute_pw) & 
                                       incident_spillover_minute_pw <= 30, 1, 0),
    incident_time_pw_osrm50 = ifelse(!is.na(incident_spillover_minute_pw) & 
                                       incident_spillover_minute_pw <= 50, 1, 0),
    incident_time_pw_osrm60 = ifelse(!is.na(incident_spillover_minute_pw) & 
                                       incident_spillover_minute_pw <= 60, 1, 0),
    incident_time_pw_osrm75 = ifelse(!is.na(incident_spillover_minute_pw) & 
                                       incident_spillover_minute_pw <= 75, 1, 0),
    incident_time_pw_osrm90 = ifelse(!is.na(incident_spillover_minute_pw) & 
                                       incident_spillover_minute_pw <= 90, 1, 0),
    incident_time_pw_osrm100 = ifelse(!is.na(incident_spillover_minute_pw) & 
                                        incident_spillover_minute_pw <= 100, 1, 0),
    incident_time_pw_osrm120 = ifelse(!is.na(incident_spillover_minute_pw) & 
                                        incident_spillover_minute_pw <= 120, 1, 0),
    incident_time_pw_osrm180 = ifelse(!is.na(incident_spillover_minute_pw) & 
                                        incident_spillover_minute_pw <= 180, 1, 0),
  )

# avoid doing 10 same steps
spillover_distance_thresholds <- c(10, 30, 50, 60, 75, 90, 100, 120, 180)
spillover_distance_types <- c("nonpw", "pw")

merged_spillover_data <- merged_spillover_data %>%
  arrange(id, time) %>%
  group_by(id)

for (dist_type in spillover_distance_types) {
  for (threshold in spillover_distance_thresholds) {
    
    incident_var <- paste0("incident_time_", dist_type, "_osrm", threshold)
    window_var <- paste0("incident_window_", dist_type, "_min", threshold)
    last_incident_var <- paste0("last_incident_time_", dist_type, "_min", threshold)
    
    merged_spillover_data <- merged_spillover_data %>%
      mutate(
        # use dynamic operator
        !!last_incident_var := ifelse(.data[[incident_var]] == 1, time, NA)
      ) %>%
      fill(all_of(last_incident_var), .direction = "down") %>%
      mutate(
        !!window_var := ifelse(
          !is.na(.data[[last_incident_var]]) & 
            (time - .data[[last_incident_var]] <= 24) & 
            (time >= .data[[last_incident_var]]), 1, 0
        ),
        !!window_var := as.integer(.data[[window_var]])
      )
  }
}

merged_spillover_data <- merged_spillover_data %>%
  ungroup()


# STEP2
# next, donut shape; X-Y miles

merged_spillover_data <- merged_spillover_data %>%
  mutate(
    incident_window_nonpw_min1030 = ifelse(incident_window_nonpw_min30 == 1 & incident_window_nonpw_min10 == 0, 1, 0),
    incident_window_nonpw_min3050 = ifelse(incident_window_nonpw_min50 == 1 & incident_window_nonpw_min30 == 0, 1, 0),
    incident_window_nonpw_min3060 = ifelse(incident_window_nonpw_min60 == 1 & incident_window_nonpw_min30 == 0, 1, 0),
    incident_window_nonpw_min5075 = ifelse(incident_window_nonpw_min75 == 1 & incident_window_nonpw_min50 == 0, 1, 0),
    incident_window_nonpw_min50100 = ifelse(incident_window_nonpw_min100 == 1 & incident_window_nonpw_min50 == 0, 1, 0),
    incident_window_nonpw_min6090 = ifelse(incident_window_nonpw_min90 == 1 & incident_window_nonpw_min60 == 0, 1, 0),
    incident_window_nonpw_min60120 = ifelse(incident_window_nonpw_min120 == 1 & incident_window_nonpw_min60 == 0, 1, 0),
    incident_window_nonpw_min75100 = ifelse(incident_window_nonpw_min100 == 1 & incident_window_nonpw_min75 == 0, 1, 0),
    incident_window_nonpw_min120180 = ifelse(incident_window_nonpw_min180 == 1 & incident_window_nonpw_min120 == 0, 1, 0),
    
    # pw
    incident_window_pw_min1030 = ifelse(incident_window_pw_min30 == 1 & incident_window_pw_min10 == 0, 1, 0),
    incident_window_pw_min3050 = ifelse(incident_window_pw_min50 == 1 & incident_window_pw_min30 == 0, 1, 0),
    incident_window_pw_min3060 = ifelse(incident_window_pw_min60 == 1 & incident_window_pw_min30 == 0, 1, 0),
    incident_window_pw_min5075 = ifelse(incident_window_pw_min75 == 1 & incident_window_pw_min50 == 0, 1, 0),
    incident_window_pw_min50100 = ifelse(incident_window_pw_min100 == 1 & incident_window_pw_min50 == 0, 1, 0),
    incident_window_pw_min6090 = ifelse(incident_window_pw_min90 == 1 & incident_window_pw_min60 == 0, 1, 0),
    incident_window_pw_min60120 = ifelse(incident_window_pw_min120 == 1 & incident_window_pw_min60 == 0, 1, 0),
    incident_window_pw_min75100 = ifelse(incident_window_pw_min100 == 1 & incident_window_pw_min75 == 0, 1, 0),
    incident_window_pw_min120180 = ifelse(incident_window_pw_min180 == 1 & incident_window_pw_min120 == 0, 1, 0),
    
    # STEP 3 
    # categorical var. for all-in pattern
    # non-pw
    mindistance_band_nonpw_original = case_when(
      incident_window_nonpw_min10   == 1 ~ "0_10",
      incident_window_nonpw_min1030 == 1 ~ "10_30",
      incident_window_nonpw_min30   == 1 ~ "0_30",
      incident_window_nonpw_min3050 == 1 ~ "30_50",
      incident_window_nonpw_min3060 == 1 ~ "30_60",
      incident_window_nonpw_min50 == 1 ~ "0_50",
      incident_window_nonpw_min5075 == 1 ~ "50_75",
      incident_window_nonpw_min6090 == 1 ~ "60_90",
      incident_window_nonpw_min60120 == 1 ~ "60_120",
      incident_window_nonpw_min75 == 1 ~ "0_75",
      incident_window_nonpw_min75100== 1 ~ "75_100",
      incident_window_nonpw_min50100== 1 ~ "50_100",
      incident_window_nonpw_min100== 1 ~ "0_100",
      incident_window_nonpw_min120180==1 ~ "120_180",
      TRUE                            ~ "far"      
    ),
    mindistance_band_nonpw_granular = factor(mindistance_band_nonpw_original, levels = c("far","0_30","30_60","60_120",
                                                                                         "120_180"
    )),
    mindistance_band_nonpw_broad = factor(mindistance_band_nonpw_original, levels = c("far","0_30","30_60","60_120",
                                                                                      "120_180"
    )),
    
    # population weighted
    mindistance_band_pw_original = case_when(
      incident_window_pw_min10   == 1 ~ "0_10",
      incident_window_pw_min1030 == 1 ~ "10_30",
      ncident_window_pw_min30   == 1 ~ "0_30",
      incident_window_pw_min3050 == 1 ~ "30_50",
      incident_window_pw_min3060 == 1 ~ "30_60",
      incident_window_pw_min50 == 1 ~ "0_50",
      incident_window_pw_min5075 == 1 ~ "50_75",
      incident_window_pw_min6090== 1 ~ "60_90",
      incident_window_pw_min60120== 1 ~ "60_120",
      incident_window_pw_min75 == 1 ~ "0_75",
      incident_window_pw_min75100== 1 ~ "75_100",
      incident_window_pw_min50100== 1 ~ "50_100",
      incident_window_pw_min100== 1 ~ "0_100",
      incident_window_pw_min120180==1 ~ "120_180",
      TRUE                            ~ "far"      
    ),
    mindistance_band_pw_granular = factor(mindistance_band_pw_original, levels = c("far","0_30","30_60","60_120",
                                                                                   "120_180"
    )),
    mindistance_band_pw_broad = factor(mindistance_band_pw_original, levels = c("far","0_30","30_60","60_120",
                                                                                "120_180"
    )),
    
  )





# STEP 4
# continuous distance
# threshold is now 200, as a placeholder

merged_spillover_data <- merged_spillover_data %>%
  arrange(id, time) %>%
  group_by(id) %>%
  # record both time AND distance when a shooting happens close to a district
  
 
  
  mutate(last_shoot_time100_minnonpw = ifelse(incident_spillover_minute_nonpw <= 100,
                                              time, NA_real_),
         last_shoot_time100_minpw = ifelse(incident_spillover_minute_pw <= 100,
                                           time, NA_real_),
         last_shoot_dist100_minnonpw = ifelse(incident_spillover_minute_nonpw <= 100,
                                              incident_spillover_minute_nonpw, NA_real_),
         last_shoot_dist100_minpw = ifelse(incident_spillover_minute_pw <= 100,
                                           incident_spillover_minute_pw, NA_real_)
  ) %>%
  # copy and paste those info
  
  fill(last_shoot_time100_minnonpw, last_shoot_dist100_minnonpw,
       last_shoot_time100_minpw, last_shoot_dist100_minpw,
       .direction = "down") %>%
  
  # only lasting for 24 months
  mutate(
    dist_follow_100_minnonpw = ifelse(
      !is.na(last_shoot_time100_minnonpw) &
        (time - last_shoot_time100_minnonpw <= 24),
      last_shoot_dist100_minnonpw,
      NA_real_
    ),
    dist_follow_100_minpw = ifelse(
      !is.na(last_shoot_time100_minpw) &
        (time - last_shoot_time100_minpw <= 24),
      last_shoot_dist100_minpw,
      NA_real_
    )
  ) %>%
  
  mutate(last_shoot_time75_minnonpw = ifelse(incident_spillover_minute_nonpw <= 75,
                                             time, NA_real_),
         last_shoot_time75_minpw = ifelse(incident_spillover_minute_pw <= 75,
                                          time, NA_real_),
         last_shoot_dist75_minnonpw = ifelse(incident_spillover_minute_nonpw <= 75,
                                             incident_spillover_minute_nonpw, NA_real_),
         last_shoot_dist75_minpw = ifelse(incident_spillover_minute_pw <= 75,
                                          incident_spillover_minute_pw, NA_real_)
  ) %>%
  # copy and paste those info
  
  fill(last_shoot_time75_minnonpw, last_shoot_dist75_minnonpw,
       last_shoot_time75_minpw, last_shoot_dist75_minpw,
       .direction = "down") %>%
  
  # only lasting for 24 months
  mutate(
    dist_follow_75_minnonpw = ifelse(
      !is.na(last_shoot_time75_minnonpw) &
        (time - last_shoot_time75_minnonpw <= 24),
      last_shoot_dist75_minnonpw,
      NA_real_
    ),
    dist_follow_75_minpw = ifelse(
      !is.na(last_shoot_time75_minpw) &
        (time - last_shoot_time75_minpw <= 24),
      last_shoot_dist75_minpw,
      NA_real_
    )
  ) %>%
  
  mutate(last_shoot_time50_minnonpw = ifelse(incident_spillover_minute_nonpw <= 50,
                                             time, NA_real_),
         last_shoot_time50_minpw = ifelse(incident_spillover_minute_pw <= 50,
                                          time, NA_real_),
         last_shoot_dist50_minnonpw = ifelse(incident_spillover_minute_nonpw <= 50,
                                             incident_spillover_minute_nonpw, NA_real_),
         last_shoot_dist50_minpw = ifelse(incident_spillover_minute_pw <= 50,
                                          incident_spillover_minute_pw, NA_real_)
  ) %>%
  # copy and paste those info
  
  fill(last_shoot_time50_minnonpw, last_shoot_dist50_minnonpw,
       last_shoot_time50_minpw, last_shoot_dist50_minpw,
       .direction = "down") %>%
  
  # only lasting for 24 months
  mutate(
    dist_follow_50_minnonpw = ifelse(
      !is.na(last_shoot_time50_minnonpw) &
        (time - last_shoot_time50_minnonpw <= 24),
      last_shoot_dist50_minnonpw,
      NA_real_
    ),
    dist_follow_50_minpw = ifelse(
      !is.na(last_shoot_time50_minpw) &
        (time - last_shoot_time50_minpw <= 24),
      last_shoot_dist50_minpw,
      NA_real_
    )
  ) %>%
  
  
  mutate(last_shoot_time30_minnonpw = ifelse(incident_spillover_minute_nonpw <= 30,
                                             time, NA_real_),
         last_shoot_time30_minpw = ifelse(incident_spillover_minute_pw <= 30,
                                          time, NA_real_),
         last_shoot_dist30_minnonpw = ifelse(incident_spillover_minute_nonpw <= 30,
                                             incident_spillover_minute_nonpw, NA_real_),
         last_shoot_dist30_minpw = ifelse(incident_spillover_minute_pw <= 30,
                                          incident_spillover_minute_pw, NA_real_)
  ) %>%
  # copy and paste those info
  
  fill(last_shoot_time30_minnonpw, last_shoot_dist30_minnonpw,
       last_shoot_time30_minpw, last_shoot_dist30_minpw,
       .direction = "down") %>%
  
  # only lasting for 24 months
  mutate(
    dist_follow_30_minnonpw = ifelse(
      !is.na(last_shoot_time30_minnonpw) &
        (time - last_shoot_time30_minnonpw <= 24),
      last_shoot_dist30_minnonpw,
      NA_real_
    ),
    dist_follow_30_minpw = ifelse(
      !is.na(last_shoot_time30_minpw) &
        (time - last_shoot_time30_minpw <= 24),
      last_shoot_dist30_minpw,
      NA_real_
    )
  ) %>%
  
  
  
  
  mutate(last_shoot_time10_minnonpw = ifelse(incident_spillover_minute_nonpw <= 10,
                                             time, NA_real_),
         last_shoot_time10_minpw = ifelse(incident_spillover_minute_pw <= 10,
                                          time, NA_real_),
         last_shoot_dist10_minnonpw = ifelse(incident_spillover_minute_nonpw <= 10,
                                             incident_spillover_minute_nonpw, NA_real_),
         last_shoot_dist10_minpw = ifelse(incident_spillover_minute_pw <= 10,
                                          incident_spillover_minute_pw, NA_real_)
  ) %>%
  # copy and paste those info
  
  fill(last_shoot_time10_minnonpw, last_shoot_dist10_minnonpw,
       last_shoot_time10_minpw, last_shoot_dist10_minpw,
       .direction = "down") %>%
  
  # only lasting for 24 months
  mutate(
    dist_follow_10_minnonpw = ifelse(
      !is.na(last_shoot_time10_minnonpw) &
        (time - last_shoot_time10_minnonpw <= 24),
      last_shoot_dist10_minnonpw,
      NA_real_
    ),
    dist_follow_10_minpw = ifelse(
      !is.na(last_shoot_time10_minpw) &
        (time - last_shoot_time10_minpw <= 24),
      last_shoot_dist10_minpw,
      NA_real_
    )
  ) %>%
  
  mutate(last_shoot_time5_minnonpw = ifelse(incident_spillover_minute_nonpw <= 5,
                                            time, NA_real_),
         last_shoot_time5_minpw = ifelse(incident_spillover_minute_pw <= 5,
                                         time, NA_real_),
         last_shoot_dist5_minnonpw = ifelse(incident_spillover_minute_nonpw <= 5,
                                            incident_spillover_minute_nonpw, NA_real_),
         last_shoot_dist5_minpw = ifelse(incident_spillover_minute_pw <= 5,
                                         incident_spillover_minute_pw, NA_real_)
  ) %>%
  # copy and paste those info
  
  fill(last_shoot_time5_minnonpw, last_shoot_dist5_minnonpw,
       last_shoot_time5_minpw, last_shoot_dist5_minpw,
       .direction = "down") %>%
  
  # only lasting for 24 months
  mutate(
    dist_follow_5_minnonpw = ifelse(
      !is.na(last_shoot_time5_minnonpw) &
        (time - last_shoot_time5_minnonpw <= 24),
      last_shoot_dist5_minnonpw,
      NA_real_
    ),
    dist_follow_5_minpw = ifelse(
      !is.na(last_shoot_time5_minpw) &
        (time - last_shoot_time5_minpw <= 24),
      last_shoot_dist5_minpw,
      NA_real_
    )
  ) %>%
  
  # minutes specific
  
  mutate(last_shoot_time180_minnonpw = ifelse(incident_spillover_minute_nonpw <= 180,
                                              time, NA_real_),
         last_shoot_time180_minpw = ifelse(incident_spillover_minute_pw <= 180,
                                           time, NA_real_),
         last_shoot_dist180_minnonpw = ifelse(incident_spillover_minute_nonpw <= 180,
                                              incident_spillover_minute_nonpw, NA_real_),
         last_shoot_dist180_minpw = ifelse(incident_spillover_minute_pw <= 180,
                                           incident_spillover_minute_pw, NA_real_)
  ) %>%
  # copy and paste those info
  
  fill(last_shoot_time180_minnonpw, last_shoot_dist180_minnonpw,
       last_shoot_time180_minpw, last_shoot_dist180_minpw,
       .direction = "down") %>%
  
  # only lasting for 24 months
  mutate(
    dist_follow_180_minnonpw = ifelse(
      !is.na(last_shoot_time180_minnonpw) &
        (time - last_shoot_time180_minnonpw <= 24),
      last_shoot_dist180_minnonpw,
      NA_real_
    ),
    dist_follow_180_minpw = ifelse(
      !is.na(last_shoot_time180_minpw) &
        (time - last_shoot_time180_minpw <= 24),
      last_shoot_dist180_minpw,
      NA_real_
    )
  ) %>%
  
  
  mutate(last_shoot_time120_minnonpw = ifelse(incident_spillover_minute_nonpw <= 120,
                                              time, NA_real_),
         last_shoot_time120_minpw = ifelse(incident_spillover_minute_pw <= 120,
                                           time, NA_real_),
         last_shoot_dist120_minnonpw = ifelse(incident_spillover_minute_nonpw <= 120,
                                              incident_spillover_minute_nonpw, NA_real_),
         last_shoot_dist120_minpw = ifelse(incident_spillover_minute_pw <= 120,
                                           incident_spillover_minute_pw, NA_real_)
  ) %>%
  # copy and paste those info
  
  fill(last_shoot_time120_minnonpw, last_shoot_dist120_minnonpw,
       last_shoot_time120_minpw, last_shoot_dist120_minpw,
       .direction = "down") %>%
  
  # only lasting for 24 months
  mutate(
    dist_follow_120_minnonpw = ifelse(
      !is.na(last_shoot_time120_minnonpw) &
        (time - last_shoot_time120_minnonpw <= 24),
      last_shoot_dist120_minnonpw,
      NA_real_
    ),
    dist_follow_120_minpw = ifelse(
      !is.na(last_shoot_time120_minpw) &
        (time - last_shoot_time120_minpw <= 24),
      last_shoot_dist120_minpw,
      NA_real_
    )
  ) %>%
  
  mutate(last_shoot_time90_minnonpw = ifelse(incident_spillover_minute_nonpw <= 90,
                                             time, NA_real_),
         last_shoot_time90_minpw = ifelse(incident_spillover_minute_pw <= 90,
                                          time, NA_real_),
         last_shoot_dist90_minnonpw = ifelse(incident_spillover_minute_nonpw <= 90,
                                             incident_spillover_minute_nonpw, NA_real_),
         last_shoot_dist90_minpw = ifelse(incident_spillover_minute_pw <= 90,
                                          incident_spillover_minute_pw, NA_real_)
  ) %>%
  # copy and paste those info
  
  fill(last_shoot_time90_minnonpw, last_shoot_dist90_minnonpw,
       last_shoot_time90_minpw, last_shoot_dist90_minpw,
       .direction = "down") %>%
  
  # only lasting for 24 months
  mutate(
    dist_follow_90_minnonpw = ifelse(
      !is.na(last_shoot_time90_minnonpw) &
        (time - last_shoot_time90_minnonpw <= 24),
      last_shoot_dist90_minnonpw,
      NA_real_
    ),
    dist_follow_90_minpw = ifelse(
      !is.na(last_shoot_time90_minpw) &
        (time - last_shoot_time90_minpw <= 24),
      last_shoot_dist90_minpw,
      NA_real_
    )
  ) %>%
  
  mutate(last_shoot_time60_minnonpw = ifelse(incident_spillover_minute_nonpw <= 60,
                                             time, NA_real_),
         last_shoot_time60_minpw = ifelse(incident_spillover_minute_pw <= 60,
                                          time, NA_real_),
         last_shoot_dist60_minnonpw = ifelse(incident_spillover_minute_nonpw <= 60,
                                             incident_spillover_minute_nonpw, NA_real_),
         last_shoot_dist60_minpw = ifelse(incident_spillover_minute_pw <= 60,
                                          incident_spillover_minute_pw, NA_real_)
  ) %>%
  # copy and paste those info
  
  fill(last_shoot_time60_minnonpw, last_shoot_dist60_minnonpw,
       last_shoot_time60_minpw, last_shoot_dist60_minpw,
       .direction = "down") %>%
  
  # only lasting for 24 months
  mutate(
    dist_follow_60_minnonpw = ifelse(
      !is.na(last_shoot_time60_minnonpw) &
        (time - last_shoot_time60_minnonpw <= 24),
      last_shoot_dist60_minnonpw,
      NA_real_
    ),
    dist_follow_60_minpw = ifelse(
      !is.na(last_shoot_time60_minpw) &
        (time - last_shoot_time60_minpw <= 24),
      last_shoot_dist60_minpw,
      NA_real_
    )
  ) %>%
  
  ungroup()


### 5.2.3 prep of datasets ------------------------------------------------

# filtering
# no need of making incident var. anymore


#### 5.2.3.1 usual filtering pattern ---------------------------------------

# first, usual pattern as usual
# but we have to exclude incident districts (24 months) here, because we will not use that in this section anymore

data_spillover_2000 <- function(df){
  df <- as.data.frame(df)  %>%
    
    

    
    arrange(id, time) %>%
    group_by(id) %>%
    mutate(last_incident_time_re = ifelse(incident == 1, time, NA)) %>% # specify the last incident time
    fill(last_incident_time_re, .direction = "down") %>%
    mutate(incident_window_re = if_else(!is.na(last_incident_time_re) & (time - last_incident_time_re <= 24), 1, 0),
           incident_window_re = as.integer(incident_window_re)) %>% 
    filter( incident_window_re==0) %>% 
    ungroup() %>% 
  filter(year>=2000) 
  return(df)
}

data_spillover_baseline <- function(df){
  df <- as.data.frame(df) %>%
    filter(abs(pct_RDmargin_before)<=5) %>% 
    mutate(
      # incident = ifelse(incident >= 1 &fatalities>0, 1, 0), # we have to re-define incident window and history!
      pro_don_number = ifelse(pro_pac_donations_dollar>=10000, pro_don_number, 0),
      pro_log_sum = ifelse(pro_pac_donations_dollar>=10000, pro_log_sum, 0),
      anti_don_number = ifelse(anti_pac_donations_dollar>=10000, anti_don_number, 0),
      anti_log_sum = ifelse(anti_pac_donations_dollar>=10000, anti_log_sum, 0)
    ) %>% 
 
    arrange(id, time) %>%
    group_by(id) %>%
    mutate(last_incident_time_re = ifelse(incident == 1, time, NA)) %>% # specify the last incident time
    fill(last_incident_time_re, .direction = "down") %>%
    mutate(incident_window_re = if_else(!is.na(last_incident_time_re) & (time - last_incident_time_re <= 24), 1, 0),
           incident_window_re = as.integer(incident_window_re)) %>% 
    filter( incident_window_re==0) %>% 
    ungroup() %>% 
    filter(year>=2000)
  return(df)
}

merged_spillover_data_2000 <- data_spillover_2000(merged_spillover_data)
merged_spillover_data_baseline <- data_spillover_baseline(merged_spillover_data)
# write.csv(merged_spillover_data_baseline, "merged_spillover_data_baseline.csv")


#### 5.2.3.2 Farther pattern -----------------------------------------------

# next, cutoff model (like, 0-30 for treated and >30 for control)
# also, we have to exclude distrcits with shooting

# use data_spillover_2000_farnonpw function

# we need two patterns depending on pw or not, since we have two patterns of distance
data_spillover_2000_farpw <- function(df, # have to be a whole unfiltered data
                                      min_threshold, max_threshold,
                                      min_year=2000, max_year=2024,
                                      type="distance", # osrm_distance or minute
                                      fin_threshold = TRUE
){
  df <- as.data.frame(df) 
  
  if (fin_threshold==TRUE){
  df <- df %>% 
    mutate(
      pro_don_number = ifelse(pro_pac_donations_dollar>=10000, pro_don_number, 0),
      pro_log_sum = ifelse(pro_pac_donations_dollar>=10000, pro_log_sum, 0),
      anti_don_number = ifelse(anti_pac_donations_dollar>=10000, anti_don_number, 0),
      anti_log_sum = ifelse(anti_pac_donations_dollar>=10000, anti_log_sum, 0)
    )
  }
   df <- df %>%  
    arrange(id, time) %>%
    group_by(id) %>%
    mutate(last_incident_time_re = ifelse(incident == 1, time, NA)) %>% # specify the last incident time
    fill(last_incident_time_re, .direction = "down") %>%
    mutate(incident_window_re = if_else(!is.na(last_incident_time_re) & (time - last_incident_time_re <= 24), 1, 0),
           incident_window_re = as.integer(incident_window_re)) %>% 
    filter( incident_window_re==0) %>%
    ungroup()
  
  
  if (type == "distance"){
    col_name <- paste0("dist_follow_", max_threshold, "_pw")
    df <- df %>% 
      filter(!!sym(col_name) >= min_threshold | is.na(!!sym(col_name)))
  } else if (type == "osrm_distance"){
    col_name <- paste0("dist_follow_", max_threshold, "_osrmpw")
    df <- df %>% 
      filter(!!sym(col_name) >= min_threshold | is.na(!!sym(col_name)))
  } else if (type == "minute"){
    col_name <- paste0("dist_follow_", max_threshold, "_minpw")
    df <- df %>% 
      filter(!!sym(col_name) >= min_threshold | is.na(!!sym(col_name)))
  }
  
  df <- df %>% 
    filter(year>=min_year) %>%
    filter(year<=max_year) 
  
  return(df)
}

data_spillover_baseline_farnonpw <- function(df, min_threshold,max_threshold,
                                             min_year=2000, max_year=2024, # both includes equal
                                             type="distance"){
  df <- as.data.frame(df) %>%
    filter(abs(pct_RDmargin_before)<=5) %>% 
    mutate(
      # incident = ifelse(incident >= 1 &fatalities>0, 1, 0), # we have to re-define incident window and history!
      pro_don_number = ifelse(pro_pac_donations_dollar>=10000, pro_don_number, 0),
      pro_log_sum = ifelse(pro_pac_donations_dollar>=10000, pro_log_sum, 0),
      anti_don_number = ifelse(anti_pac_donations_dollar>=10000, anti_don_number, 0),
      anti_log_sum = ifelse(anti_pac_donations_dollar>=10000, anti_log_sum, 0)
    ) %>% 

    arrange(id, time) %>%
    group_by(id) %>%
    mutate(last_incident_time_re = ifelse(incident == 1, time, NA)) %>% # specify the last incident time
    fill(last_incident_time_re, .direction = "down") %>%
    mutate(incident_window_re = if_else(!is.na(last_incident_time_re) & (time - last_incident_time_re <= 24), 1, 0),
           incident_window_re = as.integer(incident_window_re)) %>% 
    filter( incident_window_re==0)  %>%
    ungroup()
  
  if (type == "distance"){
    col_name <- paste0("dist_follow_", max_threshold, "_nonpw")
    df <- df %>% 
      filter(!!sym(col_name) >= min_threshold | is.na(!!sym(col_name)))
  } else if (type == "osrm_distance"){
    col_name <- paste0("dist_follow_", max_threshold, "_osrmnonpw")
    df <- df %>% 
      filter(!!sym(col_name) >= min_threshold | is.na(!!sym(col_name)))
  } else if (type == "minute"){
    col_name <- paste0("dist_follow_", max_threshold, "_minnonpw")
    df <- df %>% 
      filter(!!sym(col_name) >= min_threshold | is.na(!!sym(col_name)))
  }
  
  df <- df %>% 
    filter(year>=min_year)%>%
    filter(year<=max_year)
  return(df)
}

# Use data_spillover_baseline_farpw function

### 5.2.4 TWFE analysis ------------------------------------------------------------

#### 5.2.4.0A Basic Function ------------------------------------------------

# for one-by-one analysis
# not consistent with the all-in pattern due to different treatment var.

# Use anti_pro_spillover_DID function


#### 5.2.4.0B All-in pattern ------------------------------------------------

# no distance threshold, but have to exclude incident district
# already excluded

# before analysis, we have to tune in on how granular our categorical treatment var. should be

# turned out that broad pattern has multicollin issue!!
# but only the case for all-in version. seemingly, not at one-by-one pattern

# one of the disadvantages of this model is a strong linear assumption among all distance categories.
# multiple "farther-control" models are more granular???
# on the other hand, multiple "farther-control" model might have more severe multiple testing issue compared with the all-in-one model
# that said, even at the all-in-one model, there is some multiple testing, since we have to estimate coefficients of multiple categorical variables.


GVP_allin_baseline_pwgran <- fixest::feols(anti_log_sum ~ i(distance_band_pw_granular, ref = "far") + bachelor + black + white + unemployment +
                                             log_income + rep_incumbent_before + gun_ratio_pct
                                           | id + time,  data = merged_spillover_data_baseline)

GVP_allin_baseline_pwgran
# certain multi-collin, which is a disadvantage of this method??

GVP_allin_baseline_pwgran_osrm <- fixest::feols(anti_log_sum ~ i(osrmdistance_band_pw_granular, ref = "far") + bachelor + black + white + unemployment +
                                                  log_income + rep_incumbent_before + gun_ratio_pct
                                                | id + time,  data = merged_spillover_data_baseline)

GVP_allin_baseline_pwgran_osrm
# huge multicollin

GVP_allin_baseline_pwgran_min <- fixest::feols(anti_log_sum ~ i(mindistance_band_pw_granular, ref = "far") + bachelor + black + white + unemployment +
                                                 log_income + rep_incumbent_before + gun_ratio_pct
                                               | id + time,  data = merged_spillover_data_baseline)

GVP_allin_baseline_pwgran_min



GVP_allin_baseline_pwbroad <- fixest::feols(anti_log_sum ~ i(distance_band_pw_broad, ref = "far") + bachelor + black + white + unemployment +
                                              log_income + rep_incumbent_before + gun_ratio_pct
                                            | id + time,  data = merged_spillover_data_baseline)

GVP_allin_baseline_pwbroad
# multicollin

# next, gun lobby side
gun_allin_baseline_pwgran <- fixest::feols(pro_log_sum ~ i(distance_band_pw_granular, ref = "far") + bachelor + black + white + unemployment +
                                             log_income + rep_incumbent_before + gun_ratio_pct
                                           | id + time,  data = merged_spillover_data_baseline)

gun_allin_baseline_pwgran

# multicollin again
gun_allin_baseline_pwbroad <- fixest::feols(pro_log_sum ~ i(distance_band_pw_broad, ref = "far") + bachelor + black + white + unemployment +
                                              log_income + rep_incumbent_before + gun_ratio_pct
                                            | id + time,  data = merged_spillover_data_baseline)

gun_allin_baseline_pwbroad




###### 5.2.4.0B.1 Non-pw patterns ------------------------------------------------

GVP_allin_baseline_nonpw <- fixest::feols(anti_log_sum ~ i(distance_band_nonpw_granular, ref = "far") + bachelor + black + white + unemployment +
                                            log_income + rep_incumbent_before + gun_ratio_pct
                                          | id + time,  data = merged_spillover_data_baseline)

GVP_allin_baseline_nonpw

# next, gun side
gun_allin_baseline_nonpw <- fixest::feols(pro_log_sum ~ i(distance_band_nonpw_granular, ref = "far") + bachelor + black + white + unemployment +
                                            log_income + rep_incumbent_before + gun_ratio_pct
                                          | id + time,  data = merged_spillover_data_baseline)

gun_allin_baseline_nonpw


#### 5.2.4.1A Baseline (and pw) ------------------------------------------------------------

# the most basic pattern


##### 5.2.4.1A.2 10 ------------------------------------------------------------------------

# anti
# 10 mile pw
spillover_anti_baseline_10_pw <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 0, 10),
                                                        treatment = "incident_window_pw_10")
spillover_anti_baseline_10_pw

# non-pw
spillover_anti_baseline_10_nonpw <- anti_pro_spillover_DID(data_spillover_baseline_farnonpw(merged_spillover_data, 0,10),
                                                           treatment = "incident_window_nonpw_10")
spillover_anti_baseline_10_nonpw
# slightly positive???

# actual distance
spillover_anti_baseline_osrm10_pw <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 0, 10, type = "osrm_distance"),
                                                            treatment = "incident_window_pw_osrm10")
spillover_anti_baseline_osrm10_pw
# multi-collin

# minutes
spillover_anti_baseline_min10_pw <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 0, 10, type = "minute"),
                                                           treatment = "incident_window_pw_min10")
spillover_anti_baseline_min10_pw
# multicollin

# minutes -non pw
spillover_anti_baseline_min10_nonpw <- anti_pro_spillover_DID(data_spillover_baseline_farnonpw(merged_spillover_data, 0, 10, type = "minute"),
                                                              treatment = "incident_window_nonpw_min10")
spillover_anti_baseline_min10_nonpw
# multicollin


spillover_pro_baseline_osrm10_pw <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 0, 10, type = "osrm_distance"),
                                                           outcome = "pro_log_sum",
                                                           treatment = "incident_window_pw_osrm10")
spillover_pro_baseline_osrm10_pw
# multicollin

spillover_pro_baseline_min10_pw <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 0, 10, type="minute"),
                                                          outcome = "pro_log_sum",
                                                          treatment = "incident_window_pw_min10")
spillover_pro_baseline_min10_pw
# multi-collin


##### 5.2.4.1A.3 30 ---------------------------------------------------------


spillover_anti_baseline_1030_pw <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 10, 30),
                                                          treatment = "incident_window_pw_1030")
spillover_anti_baseline_1030_pw




spillover_anti_baseline_30_pw <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 0, 30),
                                                        treatment = "incident_window_pw_30")
spillover_anti_baseline_30_pw

spillover_pro_baseline_1030_pw <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 10, 30),
                                                         outcome = "pro_log_sum",
                                                         treatment = "incident_window_pw_1030")
spillover_pro_baseline_1030_pw

spillover_pro_baseline_30_pw <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 0, 30),
                                                       outcome = "pro_log_sum",
                                                       treatment = "incident_window_pw_30")
spillover_pro_baseline_30_pw

spillover_anti_baseline_30_osrmpw <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 0, 30, type = "osrm_distance"),
                                                            # outcome = "pro_log_sum",
                                                            treatment = "incident_window_pw_osrm30")
spillover_anti_baseline_30_osrmpw
# multi-collin

spillover_pro_baseline_30_osrmpw <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 0, 30, type = "osrm_distance"),
                                                           outcome = "pro_log_sum",
                                                           treatment = "incident_window_pw_osrm30")
spillover_pro_baseline_30_osrmpw
# multi-collin

spillover_anti_baseline_30_minpw <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 0, 30, type = "minute"),
                                                           # outcome = "pro_log_sum",
                                                           treatment = "incident_window_pw_min30")
spillover_anti_baseline_30_minpw
# sig


spillover_pro_baseline_30_minpw <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 0, 30, type = "minute"),
                                                          outcome = "pro_log_sum",
                                                          treatment = "incident_window_pw_min30")
spillover_pro_baseline_30_minpw

# non-pw

spillover_anti_baseline_30_nonpw <- anti_pro_spillover_DID(data_spillover_baseline_farnonpw(merged_spillover_data, 0, 30),
                                                           treatment = "incident_window_nonpw_30")
spillover_anti_baseline_30_nonpw

spillover_pro_baseline_30_nonpw <- anti_pro_spillover_DID(data_spillover_baseline_farnonpw(merged_spillover_data, 0, 30),
                                                          outcome = "pro_log_sum",
                                                          treatment = "incident_window_nonpw_30")
spillover_pro_baseline_30_nonpw

spillover_anti_baseline_1030_nonpw <- anti_pro_spillover_DID(data_spillover_baseline_farnonpw(merged_spillover_data, 10, 30),
                                                             treatment = "incident_window_nonpw_1030")
spillover_anti_baseline_1030_nonpw

spillover_pro_baseline_1030_nonpw <- anti_pro_spillover_DID(data_spillover_baseline_farnonpw(merged_spillover_data, 10, 30),
                                                            outcome = "pro_log_sum",
                                                            treatment = "incident_window_nonpw_1030")
spillover_pro_baseline_1030_nonpw


##### 5.2.4.1A.4. 50 --------------------------------------------------------


# time-separate
# first, data-separate

spillover_anti_baseline_3050_pw0017 <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 30, 50, 
                                                                                            min_year = 2000, max_year = 2017), # both includes equal
                                                              treatment = "incident_window_pw_3050")
spillover_anti_baseline_3050_pw0017

spillover_anti_baseline_3050_pw1824 <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 30, 50, 
                                                                                            min_year = 2018, max_year = 2024),
                                                              treatment = "incident_window_pw_3050")
spillover_anti_baseline_3050_pw1824

# next, interaction


spillover_anti_baseline_3050_pw17int <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 30, 50),
                                                               treatment = "incident_window_pw_3050", year_separate = TRUE, year_before = 2017)
spillover_anti_baseline_3050_pw17int

spillover_anti_baseline_3050_pw <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 30, 50),
                                                          treatment = "incident_window_pw_3050")
spillover_anti_baseline_3050_pw



spillover_pro_baseline_3050_pw <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 30, 50),
                                                         outcome = "pro_log_sum",
                                                         treatment = "incident_window_pw_3050")
spillover_pro_baseline_3050_pw


# nonpw





##### 5.2.4.1A.4 75 ---------------------------------------------------------


# 50-75 miles
spillover_anti_baseline_5075_pw <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 50, 75),
                                                          treatment = "incident_window_pw_5075")
spillover_anti_baseline_5075_pw

spillover_pro_baseline_5075_pw <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 50,75),
                                                         outcome = "pro_log_sum",
                                                         treatment = "incident_window_pw_5075")
spillover_pro_baseline_5075_pw

# non-pw
spillover_anti_baseline_5075_nonpw <- anti_pro_spillover_DID(data_spillover_baseline_farnonpw(merged_spillover_data, 50,75),
                                                             treatment = "incident_window_nonpw_5075")
spillover_anti_baseline_5075_nonpw

spillover_pro_baseline_5075_nonpw <- anti_pro_spillover_DID(data_spillover_baseline_farnonpw(merged_spillover_data, 50,75),
                                                            outcome = "pro_log_sum",
                                                            treatment = "incident_window_nonpw_5075")
spillover_pro_baseline_5075_nonpw


##### 5.2.4.1A.5 100------------------------------------------------------------


spillover_anti_baseline_75100_pw <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 75, 100),
                                                           treatment = "incident_window_pw_75100")
spillover_anti_baseline_75100_pw

spillover_pro_baseline_75100_pw <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 75, 100),
                                                          outcome = "pro_log_sum",
                                                          treatment = "incident_window_pw_75100")
spillover_pro_baseline_75100_pw


spillover_anti_baseline_75100_nonpw <- anti_pro_spillover_DID(data_spillover_baseline_farnonpw(merged_spillover_data, 75, 100),
                                                              # outcome = "pro_log_sum",
                                                              treatment = "incident_window_nonpw_75100")
spillover_anti_baseline_75100_nonpw
# 
spillover_pro_baseline_75100_nonpw <- anti_pro_spillover_DID(data_spillover_baseline_farnonpw(merged_spillover_data, 75, 100),
                                                             outcome = "pro_log_sum",
                                                             treatment = "incident_window_nonpw_75100")
spillover_pro_baseline_75100_nonpw



#### 5.2.4.1D number outcome -----------------------------------------------

# 50-75 miles
spillover_antinum_baseline_5075_pw <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 50, 75),
                                                             outcome = "anti_don_number",
                                                             treatment = "incident_window_pw_5075")
spillover_antinum_baseline_5075_pw

spillover_pronum_baseline_5075_pw <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 50,75),
                                                            outcome = "pro_don_number",
                                                            treatment = "incident_window_pw_5075")
spillover_pronum_baseline_5075_pw

# 75-100 miles
spillover_antinum_baseline_75100_pw <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 75, 100),
                                                              outcome = "anti_don_number",
                                                              treatment = "incident_window_pw_75100")
spillover_antinum_baseline_75100_pw

spillover_pronum_baseline_75100_pw <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 75, 100),
                                                             outcome = "pro_don_number",
                                                             treatment = "incident_window_pw_75100")
spillover_pronum_baseline_75100_pw



#### 5.2.5.1A Whole and Geo centroid (no-pw) -----------------------------------------


spillover_anti_whole_10_nonpw <- anti_pro_spillover_DID(data_spillover_2000_farnonpw(merged_spillover_data, 0, 10), # not 30!!!
                                                        treatment = "incident_window_nonpw_10")
spillover_anti_whole_10_nonpw

spillover_pro_whole_10_nonpw <- anti_pro_spillover_DID(data_spillover_2000_farnonpw(merged_spillover_data, 0, 10),
                                                       outcome = "pro_log_sum",
                                                       treatment = "incident_window_nonpw_10")
spillover_pro_whole_10_nonpw



# 30 miles
spillover_anti_whole_30_nonpw <- anti_pro_spillover_DID(data_spillover_2000_farnonpw(merged_spillover_data, 0, 30), 
                                                        treatment = "incident_window_nonpw_30")
spillover_anti_whole_30_nonpw

spillover_pro_whole_30_nonpw <- anti_pro_spillover_DID(data_spillover_2000_farnonpw(merged_spillover_data, 0, 30),
                                                       outcome = "pro_log_sum",
                                                       treatment = "incident_window_nonpw_30")
spillover_pro_whole_30_nonpw



# 50-75 miles
spillover_anti_whole_5075_nonpw <- anti_pro_spillover_DID(data_spillover_2000_farnonpw(merged_spillover_data, 50, 75),
                                                          treatment = "incident_window_nonpw_5075")
spillover_anti_whole_5075_nonpw

spillover_pro_whole_5075_nonpw <- anti_pro_spillover_DID(data_spillover_2000_farnonpw(merged_spillover_data, 50, 75),
                                                         outcome = "pro_log_sum",
                                                         treatment = "incident_window_nonpw_5075")
spillover_pro_whole_5075_nonpw

# 75-100 miles
spillover_anti_whole_75100_nonpw <- anti_pro_spillover_DID(data_spillover_2000_farnonpw(merged_spillover_data, 75, 100),
                                                           treatment = "incident_window_nonpw_75100")
spillover_anti_whole_75100_nonpw

spillover_pro_whole_75100_nonpw <- anti_pro_spillover_DID(data_spillover_2000_farnonpw(merged_spillover_data, 75, 100),
                                                          outcome = "pro_log_sum",
                                                          treatment = "incident_window_nonpw_75100")
spillover_pro_whole_75100_nonpw



#### 5.2.5.1B Whole and pw -------------------------------------------------

# 0-50 miles

spillover_anti_whole_50_pw <- anti_pro_spillover_DID(data_spillover_2000_farpw(merged_spillover_data, 0, 50),
                                                     treatment = "incident_window_pw_50")
spillover_anti_whole_50_pw

spillover_pro_whole_50_pw <- anti_pro_spillover_DID(data_spillover_2000_farpw(merged_spillover_data, 0, 50),
                                                    outcome = "pro_log_sum",
                                                    treatment = "incident_window_pw_50")
spillover_pro_whole_50_pw


# 50-75 miles
spillover_anti_whole_5075_pw <- anti_pro_spillover_DID(data_spillover_2000_farpw(merged_spillover_data, 50, 75),
                                                       treatment = "incident_window_pw_5075")
spillover_anti_whole_5075_pw

spillover_pro_whole_5075_pw <- anti_pro_spillover_DID(data_spillover_2000_farpw(merged_spillover_data, 50, 75),
                                                      outcome = "pro_log_sum",
                                                      treatment = "incident_window_pw_5075")
spillover_pro_whole_5075_pw

# 75-100 miles
spillover_anti_whole_75100_pw <- anti_pro_spillover_DID(data_spillover_2000_farpw(merged_spillover_data, 75, 100),
                                                        treatment = "incident_window_pw_75100")
spillover_anti_whole_75100_pw

spillover_pro_whole_75100_pw <- anti_pro_spillover_DID(data_spillover_2000_farpw(merged_spillover_data, 75, 100),
                                                       outcome = "pro_log_sum",
                                                       treatment = "incident_window_pw_75100")
spillover_pro_whole_75100_pw


# 50-100 miles
spillover_anti_whole_50100_pw <- anti_pro_spillover_DID(data_spillover_2000_farpw(merged_spillover_data, 50, 100),
                                                        treatment = "incident_window_pw_50100")
spillover_anti_whole_50100_pw

spillover_pro_whole_50100_pw <- anti_pro_spillover_DID(data_spillover_2000_farpw(merged_spillover_data, 50, 100),
                                                       outcome = "pro_log_sum",
                                                       treatment = "incident_window_pw_50100")
spillover_pro_whole_50100_pw




#### 5.2.5.1C Baseline and Geo centroid ------------------------------------


# 
# # non-donut version
# # 0-50 mile
# # not good
# spillover_anti_baseline_50_nonpw <- anti_pro_spillover_DID(data_spillover_baseline_farnonpw(merged_spillover_data, 0),
#                                                              treatment = "incident_window_nonpw_50")
# spillover_anti_baseline_50_nonpw
# 
# spillover_pro_baseline_50_nonpw <- anti_pro_spillover_DID(data_spillover_baseline_farnonpw(merged_spillover_data, 0),
#                                                             outcome = "pro_log_sum",
#                                                             treatment = "incident_window_nonpw_50")
# spillover_pro_baseline_50_nonpw
# 



## 5.3 Visualization of Results -------------------------------------------------


#### 5.3.1 Basic -----------------------------------------------------------

spillover_anti_baseline_10_pw <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 0, 10),
                                                        treatment = "incident_window_pw_10")
spillover_anti_baseline_1030_pw <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 10, 30),
                                                          treatment = "incident_window_pw_1030")
spillover_anti_baseline_30_pw <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 0, 30),
                                                        treatment = "incident_window_pw_30")

spillover_anti_baseline_3050_pw <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 30, 50),
                                                          treatment = "incident_window_pw_3050")
spillover_anti_baseline_5075_pw <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 50, 75),
                                                          treatment = "incident_window_pw_5075")
spillover_anti_baseline_75100_pw <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 75, 100),
                                                           treatment = "incident_window_pw_75100")

# gvp
GVP_spillover_list <- list(
  "0-10 miles"=spillover_anti_baseline_10_pw, 
  "10-30 miles"=spillover_anti_baseline_1030_pw, 
  "0-30 miles"=spillover_anti_baseline_30_pw, 
  "30-50 miles"= spillover_anti_baseline_3050_pw, 
  "50-75 miles"=spillover_anti_baseline_5075_pw,
  "75-100 miles"=spillover_anti_baseline_75100_pw
 
)

coef_map_spillover <- c(
  "incident_window_pw_10" = "Treatment",
  "incident_window_pw_1030" = "Treatment",
  "incident_window_pw_30" = "Treatment",
  "incident_window_pw_3050" = "Treatment",
  "incident_window_pw_5075" = "Treatment",
  "incident_window_pw_75100" = "Treatment"
)

# how many "treated" districts??
# nrow(subset(data_spillover_baseline_farpw(merged_spillover_data, 0),
#     incident_window_pw_30 == 1))
# 
# nrow(subset(merged_spillover_data_baseline,incident_window_pw_30 == 1))
# nrow(subset(merged_spillover_data_baseline,incident_window_pw_3050 == 1))

gof_add_pw_spillover <- data.frame(
  raw = c("# of treated obs.","Distance from shooting places", "Control group"),
  mod1 = c(nrow(subset(merged_spillover_data_baseline,incident_window_pw_10 == 1)),
           "0-10 miles", "> 10 miles"), 
  mod2 = c(nrow(subset(merged_spillover_data_baseline,incident_window_pw_1030 == 1)),
           "10-30 miles", "> 30 miles"), 
  mod3 = c(nrow(subset(merged_spillover_data_baseline,incident_window_pw_30 == 1)),
           "0-30 miles", "> 30 miles"), 
  mod4 = c(nrow(subset(merged_spillover_data_baseline,incident_window_pw_3050 == 1)),
           "30-50 miles", "> 50 miles"), 
  mod5 = c(nrow(subset(merged_spillover_data_baseline,incident_window_pw_5075 == 1)),
           "50-75 miles",  "> 75 miles"), 
  mod6 = c(nrow(subset(merged_spillover_data_baseline,incident_window_pw_75100 == 1)),
           "75-100 miles", "> 100 miles"), 
  stringsAsFactors = FALSE
)

GVP_spillover_pw <- gt_table(GVP_spillover_list, coef_map_spillover,  c("nobs"),
                             gof_add_pw_spillover,"Spill-over Effect; GVP Contributions",
                             "Log Dollar Outcome; Population-weighted Centroid; Competitive Districts"
)
GVP_spillover_pw

# gunlobby

spillover_pro_baseline_10_pw <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 0, 10),
                                                       outcome = "pro_log_sum",
                                                       treatment = "incident_window_pw_10")
spillover_pro_baseline_1030_pw <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 10, 30),
                                                         outcome = "pro_log_sum",
                                                         treatment = "incident_window_pw_1030")
spillover_pro_baseline_30_pw <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 0, 30),
                                                       outcome = "pro_log_sum",
                                                       treatment = "incident_window_pw_30")

spillover_pro_baseline_3050_pw <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 30, 50),
                                                         outcome = "pro_log_sum",
                                                         treatment = "incident_window_pw_3050")
spillover_pro_baseline_5075_pw <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 50, 75),
                                                         outcome = "pro_log_sum",
                                                         treatment = "incident_window_pw_5075")
spillover_pro_baseline_75100_pw <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 75, 100),
                                                          outcome = "pro_log_sum",
                                                          treatment = "incident_window_pw_75100")

GLobby_spillover_list <- list(
  "0-10 miles"=spillover_pro_baseline_10_pw, 
  "10-30 miles"=spillover_pro_baseline_1030_pw, 
  "0-30 miles"=spillover_pro_baseline_30_pw, 
  "30-50 miles"= spillover_pro_baseline_3050_pw, 
  "50-75 miles"=spillover_pro_baseline_5075_pw,
  "75-100 miles"=spillover_pro_baseline_75100_pw
)


GLobby_spillover_pw <- gt_table(GLobby_spillover_list, coef_map_spillover,  c("nobs"),
                                gof_add_pw_spillover,"Spill-over Effect; Gun Lobby Contributions",
                                "Log Dollar Outcome; Population-weighted Centroid; Competitive Districts"
)
GLobby_spillover_pw


### 5.3.1A grouping before and after 2018 ---------------------------------



spillover_anti_baseline_10_pw0017 <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 0, 10, 2000, 2017),
                                                            treatment = "incident_window_pw_10")
spillover_anti_baseline_1030_pw0017 <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 10, 30, 2000, 2017),
                                                              treatment = "incident_window_pw_1030")
spillover_anti_baseline_30_pw0017 <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 0, 30, 2000, 2017),
                                                            treatment = "incident_window_pw_30")

spillover_anti_baseline_3050_pw0017 <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 30, 50, 2000, 2017),
                                                              treatment = "incident_window_pw_3050")
spillover_anti_baseline_5075_pw0017 <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 50, 75, 2000, 2017),
                                                              treatment = "incident_window_pw_5075")
spillover_anti_baseline_75100_pw0017 <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 75, 100, 2000, 2017),
                                                               treatment = "incident_window_pw_75100")

# gvp
GVP_spillover_list_0017 <- list(
  "0-10 miles"=spillover_anti_baseline_10_pw0017, 
  "10-30 miles"=spillover_anti_baseline_1030_pw0017, 
  "0-30 miles"=spillover_anti_baseline_30_pw0017, 
  "30-50 miles"= spillover_anti_baseline_3050_pw0017, 
  "50-75 miles"=spillover_anti_baseline_5075_pw0017,
  "75-100 miles"=spillover_anti_baseline_75100_pw0017
)

merged_spillover_data_baseline0017 <- merged_spillover_data_baseline %>% 
  filter(year<=2017)

gof_add_pw_spillover_0017 <- data.frame(
  raw = c("# of treated obs.","Distance from shooting places", "Control group"),
  mod1 = c(nrow(subset(merged_spillover_data_baseline0017,incident_window_pw_10 == 1)),
           "0-10 miles", "> 10 miles"), 
  mod2 = c(nrow(subset(merged_spillover_data_baseline0017,incident_window_pw_1030 == 1)),
           "10-30 miles", "> 30 miles"), 
  mod3 = c(nrow(subset(merged_spillover_data_baseline0017,incident_window_pw_30 == 1)),
           "0-30 miles", "> 30 miles"), 
  mod4 = c(nrow(subset(merged_spillover_data_baseline0017,incident_window_pw_3050 == 1)),
           "30-50 miles", "> 50 miles"), 
  mod5 = c(nrow(subset(merged_spillover_data_baseline0017,incident_window_pw_5075 == 1)),
           "50-75 miles",  "> 75 miles"), 
  mod6 = c(nrow(subset(merged_spillover_data_baseline0017,incident_window_pw_75100 == 1)),
           "75-100 miles", "> 100 miles"), 
  stringsAsFactors = FALSE
)

GVP_spillover_pw0017 <- gt_table(GVP_spillover_list_0017, coef_map_spillover,  c("nobs"),
                                 gof_add_pw_spillover_0017,"Spill-over Effect; GVP Contributions; 2000-2017",
                                 "Log Dollar Outcome; Population-weighted Centroid; Competitive Districts"
)
GVP_spillover_pw0017

# next, after 2018


spillover_anti_baseline_10_pw1824 <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 0, 10,2018, 2024),
                                                            treatment = "incident_window_pw_10")
spillover_anti_baseline_1030_pw1824 <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 10, 30,2018, 2024),
                                                              treatment = "incident_window_pw_1030")
spillover_anti_baseline_30_pw1824 <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 0, 30,2018, 2024),
                                                            treatment = "incident_window_pw_30")

spillover_anti_baseline_3050_pw1824 <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 30, 50,2018, 2024),
                                                              treatment = "incident_window_pw_3050")
spillover_anti_baseline_5075_pw1824 <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 50, 75,2018, 2024),
                                                              treatment = "incident_window_pw_5075")
spillover_anti_baseline_75100_pw1824 <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 75, 100,2018, 2024),
                                                               treatment = "incident_window_pw_75100")

# gvp
GVP_spillover_list_1824 <- list(
  "0-10 miles"=spillover_anti_baseline_10_pw1824, 
  "10-30 miles"=spillover_anti_baseline_1030_pw1824, 
  "0-30 miles"=spillover_anti_baseline_30_pw1824, 
  "30-50 miles"= spillover_anti_baseline_3050_pw1824, 
  "50-75 miles"=spillover_anti_baseline_5075_pw1824,
  "75-100 miles"=spillover_anti_baseline_75100_pw1824
)

merged_spillover_data_baseline1824 <- merged_spillover_data_baseline %>% 
  filter(year>=2018)

gof_add_pw_spillover_1824 <- data.frame(
  raw = c("# of treated obs.","Distance from shooting places", "Control group"),
  mod1 = c(nrow(subset(merged_spillover_data_baseline1824,incident_window_pw_10 == 1)),
           "0-10 miles", "> 10 miles"), 
  mod2 = c(nrow(subset(merged_spillover_data_baseline1824,incident_window_pw_1030 == 1)),
           "10-30 miles", "> 30 miles"), 
  mod3 = c(nrow(subset(merged_spillover_data_baseline1824,incident_window_pw_30 == 1)),
           "0-30 miles", "> 30 miles"), 
  mod4 = c(nrow(subset(merged_spillover_data_baseline1824,incident_window_pw_3050 == 1)),
           "30-50 miles", "> 50 miles"), 
  mod5 = c(nrow(subset(merged_spillover_data_baseline1824,incident_window_pw_5075 == 1)),
           "50-75 miles",  "> 75 miles"), 
  mod6 = c(nrow(subset(merged_spillover_data_baseline1824,incident_window_pw_75100 == 1)),
           "75-100 miles", "> 100 miles"), 
  stringsAsFactors = FALSE
)

GVP_spillover_pw1824 <- gt_table(GVP_spillover_list_1824, coef_map_spillover,  c("nobs"),
                                 gof_add_pw_spillover_1824,"Spill-over Effect; GVP Contributions; 2018-2024",
                                 "Log Dollar Outcome; Population-weighted Centroid; Competitive Districts"
)
GVP_spillover_pw1824




### 5.3.1B gun lobby and parkland -----------------------------------------



spillover_pro_baseline_10_pw0017 <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 0, 10, 2000, 2017),
                                                           outcome = "pro_log_sum",
                                                           treatment = "incident_window_pw_10")
spillover_pro_baseline_1030_pw0017 <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 10, 30, 2000, 2017),
                                                             outcome = "pro_log_sum",
                                                             treatment = "incident_window_pw_1030")
spillover_pro_baseline_30_pw0017 <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 0, 30, 2000, 2017),
                                                           outcome = "pro_log_sum",
                                                           treatment = "incident_window_pw_30")

spillover_pro_baseline_3050_pw0017 <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 30, 50, 2000, 2017),
                                                             outcome = "pro_log_sum",
                                                             treatment = "incident_window_pw_3050")
spillover_pro_baseline_5075_pw0017 <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 50, 75, 2000, 2017),
                                                             outcome = "pro_log_sum",
                                                             treatment = "incident_window_pw_5075")
spillover_pro_baseline_75100_pw0017 <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 75, 100, 2000, 2017),
                                                              outcome = "pro_log_sum",
                                                              treatment = "incident_window_pw_75100")

# gvp
GVP_spillover_list_0017pro <- list(
  "0-10 miles"=spillover_pro_baseline_10_pw0017, 
  "10-30 miles"=spillover_pro_baseline_1030_pw0017, 
  "0-30 miles"=spillover_pro_baseline_30_pw0017, 
  "30-50 miles"= spillover_pro_baseline_3050_pw0017, 
  "50-75 miles"=spillover_pro_baseline_5075_pw0017,
  "75-100 miles"=spillover_pro_baseline_75100_pw0017
)



GVP_spillover_pw0017pro <- gt_table(GVP_spillover_list_0017pro, coef_map_spillover,  c("nobs"),
                                    gof_add_pw_spillover_0017,"Spill-over Effect; Gun Lobby Contributions; 2000-2017",
                                    "Log Dollar Outcome; Population-weighted Centroid; Competitive Districts"
)
GVP_spillover_pw0017pro

# next, after 2018


spillover_pro_baseline_10_pw1824 <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 0, 10,2018, 2024),
                                                           outcome = "pro_log_sum",
                                                           treatment = "incident_window_pw_10")
spillover_pro_baseline_1030_pw1824 <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 10, 30,2018, 2024),
                                                             outcome = "pro_log_sum",
                                                             treatment = "incident_window_pw_1030")
spillover_pro_baseline_30_pw1824 <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 0, 30,2018, 2024),
                                                           outcome = "pro_log_sum",
                                                           treatment = "incident_window_pw_30")

spillover_pro_baseline_3050_pw1824 <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 30, 50,2018, 2024),
                                                             outcome = "pro_log_sum",
                                                             treatment = "incident_window_pw_3050")
spillover_pro_baseline_5075_pw1824 <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 50, 75,2018, 2024),
                                                             outcome = "pro_log_sum",
                                                             treatment = "incident_window_pw_5075")
spillover_pro_baseline_75100_pw1824 <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 75, 100,2018, 2024),
                                                              outcome = "pro_log_sum",
                                                              treatment = "incident_window_pw_75100")

# gvp
GVP_spillover_list_1824pro <- list(
  "0-10 miles"=spillover_pro_baseline_10_pw1824, 
  "10-30 miles"=spillover_pro_baseline_1030_pw1824, 
  "0-30 miles"=spillover_pro_baseline_30_pw1824, 
  "30-50 miles"= spillover_pro_baseline_3050_pw1824, 
  "50-75 miles"=spillover_pro_baseline_5075_pw1824,
  "75-100 miles"=spillover_pro_baseline_75100_pw1824
)



GVP_spillover_pw1824pro <- gt_table(GVP_spillover_list_1824pro, coef_map_spillover,  c("nobs"),
                                    gof_add_pw_spillover_1824,"Spill-over Effect; Gun Lobby Contributions; 2018-2024",
                                    "Log Dollar Outcome; Population-weighted Centroid; Competitive Districts"
)
GVP_spillover_pw1824pro




#### 5.3.2. non-pw -------------------------------------------------------


# non-pw and gunlobby

spillover_pro_baseline_10_nonpw <- anti_pro_spillover_DID(data_spillover_baseline_farnonpw(merged_spillover_data, 0, 10),
                                                          outcome = "pro_log_sum",
                                                          treatment = "incident_window_nonpw_10")
spillover_pro_baseline_1030_nonpw <- anti_pro_spillover_DID(data_spillover_baseline_farnonpw(merged_spillover_data, 10, 30),
                                                            outcome = "pro_log_sum",
                                                            treatment = "incident_window_nonpw_1030")
spillover_pro_baseline_30_nonpw <- anti_pro_spillover_DID(data_spillover_baseline_farnonpw(merged_spillover_data, 0, 30),
                                                          outcome = "pro_log_sum",
                                                          treatment = "incident_window_nonpw_30")

spillover_pro_baseline_3050_nonpw <- anti_pro_spillover_DID(data_spillover_baseline_farnonpw(merged_spillover_data, 30, 50),
                                                            outcome = "pro_log_sum",
                                                            treatment = "incident_window_nonpw_3050")
spillover_pro_baseline_5075_nonpw <- anti_pro_spillover_DID(data_spillover_baseline_farnonpw(merged_spillover_data, 50, 75),
                                                            outcome = "pro_log_sum",
                                                            treatment = "incident_window_nonpw_5075")
spillover_pro_baseline_75100_nonpw <- anti_pro_spillover_DID(data_spillover_baseline_farnonpw(merged_spillover_data, 75, 100),
                                                             outcome = "pro_log_sum",
                                                             treatment = "incident_window_nonpw_75100")


GLobby_spillover_nonpw_list <- list(
  "0-10 miles"=spillover_pro_baseline_10_nonpw, 
  "10-30 miles"=spillover_pro_baseline_1030_nonpw, 
  "0-30 miles"=spillover_pro_baseline_30_nonpw, 
  "30-50 miles"= spillover_pro_baseline_3050_nonpw, 
  "50-75 miles"=spillover_pro_baseline_5075_nonpw,
  "75-100 miles"=spillover_pro_baseline_75100_nonpw
)

coef_map_nonpw_spillover <- c(
  "incident_window_nonpw_10" = "Treatment",
  "incident_window_nonpw_1030" = "Treatment",
  "incident_window_nonpw_30" = "Treatment",
  "incident_window_nonpw_3050" = "Treatment",
  "incident_window_nonpw_5075" = "Treatment",
  "incident_window_nonpw_75100" = "Treatment")

gof_add_nonpw_spillover <- data.frame(
  raw = c("# of treated obs.","Distance from shooting places", "Control group"),
  mod1 = c(nrow(subset(merged_spillover_data_baseline,incident_window_nonpw_10 == 1)),
           "0-10 miles", "> 10 miles"), 
  mod2 = c(nrow(subset(merged_spillover_data_baseline,incident_window_nonpw_1030 == 1)),
           "10-30 miles", "> 30 miles"), 
  mod3 = c(nrow(subset(merged_spillover_data_baseline,incident_window_nonpw_30 == 1)),
           "0-30 miles", "> 30 miles"), 
  mod4 = c(nrow(subset(merged_spillover_data_baseline,incident_window_nonpw_3050 == 1)),
           "30-50 miles", "> 50 miles"), 
  mod5 = c(nrow(subset(merged_spillover_data_baseline,incident_window_nonpw_5075 == 1)),
           "50-75 miles",  "> 75 miles"), 
  mod6 = c(nrow(subset(merged_spillover_data_baseline,incident_window_nonpw_75100 == 1)),
           "75-100 miles", "> 100 miles"), 
  stringsAsFactors = FALSE
)


GLobby_spillover_nonpw <- gt_table(GLobby_spillover_nonpw_list, coef_map_nonpw_spillover,  c("nobs"),
                                   gof_add_nonpw_spillover,"Spill-over Effect; Gun Lobby Contributions",
                                   "Log Dollar Outcome; Geographical Centroid; Competitive Districts"
)
GLobby_spillover_nonpw

# non-pw and GVP

spillover_anti_baseline_10_nonpw <- anti_pro_spillover_DID(data_spillover_baseline_farnonpw(merged_spillover_data, 0, 10),
                                                           treatment = "incident_window_nonpw_10")
spillover_anti_baseline_1030_nonpw <- anti_pro_spillover_DID(data_spillover_baseline_farnonpw(merged_spillover_data, 10, 30),
                                                             treatment = "incident_window_nonpw_1030")
spillover_anti_baseline_30_nonpw <- anti_pro_spillover_DID(data_spillover_baseline_farnonpw(merged_spillover_data, 0, 30),
                                                           treatment = "incident_window_nonpw_30")

spillover_anti_baseline_3050_nonpw <- anti_pro_spillover_DID(data_spillover_baseline_farnonpw(merged_spillover_data, 30, 50),
                                                             treatment = "incident_window_nonpw_3050")
spillover_anti_baseline_5075_nonpw <- anti_pro_spillover_DID(data_spillover_baseline_farnonpw(merged_spillover_data, 50, 75),
                                                             treatment = "incident_window_nonpw_5075")
spillover_anti_baseline_75100_nonpw <- anti_pro_spillover_DID(data_spillover_baseline_farnonpw(merged_spillover_data, 75, 100),
                                                              treatment = "incident_window_nonpw_75100")


GVP_spillover_nonpw_list <- list(
  "0-10 miles"=spillover_anti_baseline_10_nonpw, 
  "10-30 miles"=spillover_anti_baseline_1030_nonpw, 
  "0-30 miles"=spillover_anti_baseline_30_nonpw, 
  "30-50 miles"= spillover_anti_baseline_3050_nonpw, 
  "50-75 miles"=spillover_anti_baseline_5075_nonpw,
  "75-100 miles"=spillover_anti_baseline_75100_nonpw
)


GVP_spillover_nonpw <- gt_table(GVP_spillover_nonpw_list, coef_map_nonpw_spillover,  c("nobs"),
                                gof_add_nonpw_spillover,"Spill-over Effect; GVP Contributions",
                                "Log Dollar Outcome; Geographical Centroid; Competitive Districts"
)
GVP_spillover_nonpw


### 5.3.2A before and after 2018 ------------------------------------------


spillover_anti_baseline_10_nonpw0017 <- anti_pro_spillover_DID(data_spillover_baseline_farnonpw(merged_spillover_data, 0, 10, 2000, 2017),
                                                               treatment = "incident_window_nonpw_10")
spillover_anti_baseline_1030_nonpw0017 <- anti_pro_spillover_DID(data_spillover_baseline_farnonpw(merged_spillover_data, 10, 30, 2000, 2017),
                                                                 treatment = "incident_window_nonpw_1030")
spillover_anti_baseline_30_nonpw0017 <- anti_pro_spillover_DID(data_spillover_baseline_farnonpw(merged_spillover_data, 0, 30, 2000, 2017),
                                                               treatment = "incident_window_nonpw_30")

spillover_anti_baseline_3050_nonpw0017 <- anti_pro_spillover_DID(data_spillover_baseline_farnonpw(merged_spillover_data, 30, 50, 2000, 2017),
                                                                 treatment = "incident_window_nonpw_3050")
spillover_anti_baseline_5075_nonpw0017 <- anti_pro_spillover_DID(data_spillover_baseline_farnonpw(merged_spillover_data, 50, 75, 2000, 2017),
                                                                 treatment = "incident_window_nonpw_5075")
spillover_anti_baseline_75100_nonpw0017 <- anti_pro_spillover_DID(data_spillover_baseline_farnonpw(merged_spillover_data, 75, 100, 2000, 2017),
                                                                  treatment = "incident_window_nonpw_75100")

# gvp
GVP_spillover_list_nonpw0017 <- list(
  # "0-10 miles"=spillover_anti_baseline_10_nonpw0017, 
  # multicollin
  "10-30 miles"=spillover_anti_baseline_1030_nonpw0017, 
  "0-30 miles"=spillover_anti_baseline_30_nonpw0017, 
  "30-50 miles"= spillover_anti_baseline_3050_nonpw0017, 
  "50-75 miles"=spillover_anti_baseline_5075_nonpw0017,
  "75-100 miles"=spillover_anti_baseline_75100_nonpw0017
)



gof_add_nonpw_spillover_0017 <- data.frame(
  raw = c("# of treated obs.","Distance from shooting places", "Control group"),
  # mod1 = c(nrow(subset(merged_spillover_data_baseline0017,incident_window_nonpw_10 == 1)),
  #          "0-10 miles", "> 10 miles"), 
  mod1 = c(nrow(subset(merged_spillover_data_baseline0017,incident_window_nonpw_1030 == 1)),
           "10-30 miles", "> 30 miles"), 
  mod2 = c(nrow(subset(merged_spillover_data_baseline0017,incident_window_nonpw_30 == 1)),
           "0-30 miles", "> 30 miles"), 
  mod3 = c(nrow(subset(merged_spillover_data_baseline0017,incident_window_nonpw_3050 == 1)),
           "30-50 miles", "> 50 miles"), 
  mod4 = c(nrow(subset(merged_spillover_data_baseline0017,incident_window_nonpw_5075 == 1)),
           "50-75 miles",  "> 75 miles"), 
  mod5 = c(nrow(subset(merged_spillover_data_baseline0017,incident_window_nonpw_75100 == 1)),
           "75-100 miles", "> 100 miles"),
  stringsAsFactors = FALSE
)

GVP_spillover_nonpw0017 <- gt_table(GVP_spillover_list_nonpw0017, coef_map_nonpw_spillover,  c("nobs"),
                                    gof_add_nonpw_spillover_0017,"Spill-over Effect; GVP Contributions; 2000-2017",
                                    "Log Dollar Outcome; Geographical Centroid; Competitive Districts"
)
GVP_spillover_nonpw0017

# next, after 2018


spillover_anti_baseline_10_nonpw1824 <- anti_pro_spillover_DID(data_spillover_baseline_farnonpw(merged_spillover_data, 0, 10,2018, 2024),
                                                               treatment = "incident_window_nonpw_10")
spillover_anti_baseline_1030_nonpw1824 <- anti_pro_spillover_DID(data_spillover_baseline_farnonpw(merged_spillover_data, 10, 30,2018, 2024),
                                                                 treatment = "incident_window_nonpw_1030")
spillover_anti_baseline_30_nonpw1824 <- anti_pro_spillover_DID(data_spillover_baseline_farnonpw(merged_spillover_data, 0, 30,2018, 2024),
                                                               treatment = "incident_window_nonpw_30")

spillover_anti_baseline_3050_nonpw1824 <- anti_pro_spillover_DID(data_spillover_baseline_farnonpw(merged_spillover_data, 30, 50,2018, 2024),
                                                                 treatment = "incident_window_nonpw_3050")
spillover_anti_baseline_5075_nonpw1824 <- anti_pro_spillover_DID(data_spillover_baseline_farnonpw(merged_spillover_data, 50, 75,2018, 2024),
                                                                 treatment = "incident_window_nonpw_5075")
spillover_anti_baseline_75100_nonpw1824 <- anti_pro_spillover_DID(data_spillover_baseline_farnonpw(merged_spillover_data, 75, 100,2018, 2024),
                                                                  treatment = "incident_window_nonpw_75100")

# gvp
GVP_spillover_list_nonpw1824 <- list(
  "0-10 miles"=spillover_anti_baseline_10_nonpw1824, 
  "10-30 miles"=spillover_anti_baseline_1030_nonpw1824, 
  "0-30 miles"=spillover_anti_baseline_30_nonpw1824, 
  "30-50 miles"= spillover_anti_baseline_3050_nonpw1824, 
  "50-75 miles"=spillover_anti_baseline_5075_nonpw1824,
  "75-100 miles"=spillover_anti_baseline_75100_nonpw1824
)



gof_add_nonpw_spillover_1824 <- data.frame(
  raw = c("# of treated obs.","Distance from shooting places", "Control group"),
  mod1 = c(nrow(subset(merged_spillover_data_baseline1824,incident_window_nonpw_10 == 1)),
           "0-10 miles", "> 10 miles"), 
  mod2 = c(nrow(subset(merged_spillover_data_baseline1824,incident_window_nonpw_1030 == 1)),
           "10-30 miles", "> 30 miles"), 
  mod3 = c(nrow(subset(merged_spillover_data_baseline1824,incident_window_nonpw_30 == 1)),
           "0-30 miles", "> 30 miles"), 
  mod4 = c(nrow(subset(merged_spillover_data_baseline1824,incident_window_nonpw_3050 == 1)),
           "30-50 miles", "> 50 miles"), 
  mod5 = c(nrow(subset(merged_spillover_data_baseline1824,incident_window_nonpw_5075 == 1)),
           "50-75 miles",  "> 75 miles"), 
  mod6 = c(nrow(subset(merged_spillover_data_baseline1824,incident_window_nonpw_75100 == 1)),
           "75-100 miles", "> 100 miles"), 
  stringsAsFactors = FALSE
)

GVP_spillover_nonpw1824 <- gt_table(GVP_spillover_list_nonpw1824, coef_map_nonpw_spillover,  c("nobs"),
                                    gof_add_nonpw_spillover_1824,"Spill-over Effect; GVP Contributions; 2018-2024",
                                    "Log Dollar Outcome; Geographical Centroid; Competitive Districts"
)
GVP_spillover_nonpw1824



#### 5.3.2AA pro gun -------------------------------------------------------


spillover_pro_baseline_10_nonpw0017 <- anti_pro_spillover_DID(data_spillover_baseline_farnonpw(merged_spillover_data, 0, 10, 2000, 2017),
                                                              outcome = "pro_log_sum" ,
                                                              treatment = "incident_window_nonpw_10")
spillover_pro_baseline_1030_nonpw0017 <- anti_pro_spillover_DID(data_spillover_baseline_farnonpw(merged_spillover_data, 10, 30, 2000, 2017),
                                                                outcome = "pro_log_sum" ,
                                                                treatment = "incident_window_nonpw_1030")
spillover_pro_baseline_30_nonpw0017 <- anti_pro_spillover_DID(data_spillover_baseline_farnonpw(merged_spillover_data, 0, 30, 2000, 2017),
                                                              outcome = "pro_log_sum" ,
                                                              treatment = "incident_window_nonpw_30")

spillover_pro_baseline_3050_nonpw0017 <- anti_pro_spillover_DID(data_spillover_baseline_farnonpw(merged_spillover_data, 30, 50, 2000, 2017),
                                                                outcome = "pro_log_sum" ,
                                                                treatment = "incident_window_nonpw_3050")
spillover_pro_baseline_5075_nonpw0017 <- anti_pro_spillover_DID(data_spillover_baseline_farnonpw(merged_spillover_data, 50, 75, 2000, 2017),
                                                                outcome = "pro_log_sum" ,
                                                                treatment = "incident_window_nonpw_5075")
spillover_pro_baseline_75100_nonpw0017 <- anti_pro_spillover_DID(data_spillover_baseline_farnonpw(merged_spillover_data, 75, 100, 2000, 2017),
                                                                 outcome = "pro_log_sum" ,
                                                                 treatment = "incident_window_nonpw_75100")

# gvp
Gun_spillover_list_nonpw0017 <- list(
  # "0-10 miles"=spillover_pro_baseline_10_nonpw0017, 
  # multicollin
  "10-30 miles"=spillover_pro_baseline_1030_nonpw0017, 
  "0-30 miles"=spillover_pro_baseline_30_nonpw0017, 
  "30-50 miles"= spillover_pro_baseline_3050_nonpw0017, 
  "50-75 miles"=spillover_pro_baseline_5075_nonpw0017,
  "75-100 miles"=spillover_pro_baseline_75100_nonpw0017
)



Gun_spillover_nonpw0017 <- gt_table(Gun_spillover_list_nonpw0017, coef_map_nonpw_spillover,  c("nobs"),
                                    gof_add_nonpw_spillover_0017,"Spill-over Effect; Pro-gun Contributions; 2000-2017",
                                    "Log Dollar Outcome; Geographical Centroid; Competitive Districts"
) %>% print()

# next, after 2018


spillover_pro_baseline_10_nonpw1824 <- anti_pro_spillover_DID(data_spillover_baseline_farnonpw(merged_spillover_data, 0, 10,2018, 2024),
                                                               outcome="pro_log_sum",
                                                              treatment = "incident_window_nonpw_10")
spillover_pro_baseline_1030_nonpw1824 <- anti_pro_spillover_DID(data_spillover_baseline_farnonpw(merged_spillover_data, 10, 30,2018, 2024),
                                                                outcome="pro_log_sum",
                                                                treatment = "incident_window_nonpw_1030")
spillover_pro_baseline_30_nonpw1824 <- anti_pro_spillover_DID(data_spillover_baseline_farnonpw(merged_spillover_data, 0, 30,2018, 2024),
                                                              outcome="pro_log_sum",
                                                              treatment = "incident_window_nonpw_30")

spillover_pro_baseline_3050_nonpw1824 <- anti_pro_spillover_DID(data_spillover_baseline_farnonpw(merged_spillover_data, 30, 50,2018, 2024),
                                                                outcome="pro_log_sum",
                                                                treatment = "incident_window_nonpw_3050")
spillover_pro_baseline_5075_nonpw1824 <- anti_pro_spillover_DID(data_spillover_baseline_farnonpw(merged_spillover_data, 50, 75,2018, 2024),
                                                                outcome="pro_log_sum",
                                                                treatment = "incident_window_nonpw_5075")
spillover_pro_baseline_75100_nonpw1824 <- anti_pro_spillover_DID(data_spillover_baseline_farnonpw(merged_spillover_data, 75, 100,2018, 2024),
                                                                 outcome="pro_log_sum",
                                                                 treatment = "incident_window_nonpw_75100")

# gvp
Gun_spillover_list_nonpw1824 <- list(
  "0-10 miles"=spillover_pro_baseline_10_nonpw1824, 
  "10-30 miles"=spillover_pro_baseline_1030_nonpw1824, 
  "0-30 miles"=spillover_pro_baseline_30_nonpw1824, 
  "30-50 miles"= spillover_pro_baseline_3050_nonpw1824, 
  "50-75 miles"=spillover_pro_baseline_5075_nonpw1824,
  "75-100 miles"=spillover_pro_baseline_75100_nonpw1824
)



Gun_spillover_nonpw1824 <- gt_table(Gun_spillover_list_nonpw1824, coef_map_nonpw_spillover,  c("nobs"),
                                    gof_add_nonpw_spillover_1824,"Spill-over Effect; Pro-gun PAC Contributions; 2018-2024",
                                    "Log Dollar Outcome; Geographical Centroid; Competitive Districts"
) %>% print()




#### 5.3.3 number outcome -----------------------------------------------

# gunlobby


spillover_pronum_baseline_10_pw <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 0, 10),
                                                          outcome = "pro_don_number",
                                                          treatment = "incident_window_pw_10")
spillover_pronum_baseline_1030_pw <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 10, 30),
                                                            outcome = "pro_don_number",
                                                            treatment = "incident_window_pw_1030")
spillover_pronum_baseline_30_pw <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 0, 30),
                                                          outcome = "pro_don_number",
                                                          treatment = "incident_window_pw_30")

spillover_pronum_baseline_3050_pw <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 30, 50),
                                                            outcome = "pro_don_number",
                                                            treatment = "incident_window_pw_3050")
spillover_pronum_baseline_5075_pw <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 50, 75),
                                                            outcome = "pro_don_number",
                                                            treatment = "incident_window_pw_5075")
spillover_pronum_baseline_75100_pw <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 75, 100),
                                                             outcome = "pro_don_number",
                                                             treatment = "incident_window_pw_75100")


GLobbynum_spillover_list <- list(
  "0-10 miles"=spillover_pronum_baseline_10_pw, 
  "10-30 miles"=spillover_pronum_baseline_1030_pw, 
  "0-30 miles"= spillover_pronum_baseline_30_pw, 
  "30-50 miles"= spillover_pronum_baseline_3050_pw, 
  "50-75 miles"=spillover_pronum_baseline_5075_pw,
  "75-100 miles"=spillover_pronum_baseline_75100_pw
)


GLobbynum_spillover_pw <- gt_table(GLobbynum_spillover_list, coef_map_spillover,  c("nobs"),
                                   gof_add_pw_spillover,"Spill-over Effect; Gun Lobby Contributions",
                                   "Number Outcome; Population-weighted Centroid; Competitive Districts"
)
GLobbynum_spillover_pw

# GVP

spillover_antinum_baseline_10_pw <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 0, 10),
                                                           outcome = "anti_don_number",
                                                           treatment = "incident_window_pw_10")
spillover_antinum_baseline_1030_pw <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 10, 30),
                                                             outcome = "anti_don_number",
                                                             treatment = "incident_window_pw_1030")
spillover_antinum_baseline_30_pw <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 0, 30),
                                                           outcome = "anti_don_number",
                                                           treatment = "incident_window_pw_30")

spillover_antinum_baseline_3050_pw <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 30, 50),
                                                             outcome = "anti_don_number",
                                                             treatment = "incident_window_pw_3050")
spillover_antinum_baseline_5075_pw <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 50, 75),
                                                             outcome = "anti_don_number",
                                                             treatment = "incident_window_pw_5075")
spillover_antinum_baseline_75100_pw <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 75, 100),
                                                              outcome = "anti_don_number",
                                                              treatment = "incident_window_pw_75100")


GVPnum_spillover_list <- list(
  "0-10 miles"=spillover_antinum_baseline_10_pw, 
  "10-30 miles"=spillover_antinum_baseline_1030_pw, 
  "0-30 miles"= spillover_antinum_baseline_30_pw, 
  "30-50 miles"= spillover_antinum_baseline_3050_pw, 
  "50-75 miles"=spillover_antinum_baseline_5075_pw,
  "75-100 miles"=spillover_antinum_baseline_75100_pw
)

GVPnum_spillover_pw <- gt_table(GVPnum_spillover_list, coef_map_spillover,  c("nobs"),
                                gof_add_pw_spillover,"Spill-over Effect; GVP Contributions",
                                "Number Outcome; Population-weighted Centroid; Competitive Districts"
)
GVPnum_spillover_pw


#### 5.3.4 OSRM-distance ---------------------------------------------------


spillover_anti_baseline_10_osrmpw <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 0, 10, type = "osrm_distance"),
                                                            treatment = "incident_window_pw_osrm10")
spillover_anti_baseline_1030_osrmpw <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 10, 30, type = "osrm_distance"),
                                                              treatment = "incident_window_pw_osrm1030")
spillover_anti_baseline_30_osrmpw <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 0, 30, type = "osrm_distance"),
                                                            treatment = "incident_window_pw_osrm30")

spillover_anti_baseline_3050_osrmpw <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 30, 50, type = "osrm_distance"),
                                                              treatment = "incident_window_pw_osrm3050")

# check
spillover_anti_baseline_3050_osrmpw$collin.var
collinearity(spillover_anti_baseline_3050_osrmpw)

spillover_anti_baseline_5075_osrmpw <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 50, 75, type = "osrm_distance"),
                                                              treatment = "incident_window_pw_osrm5075")
spillover_anti_baseline_75100_osrmpw <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 75, 100, type = "osrm_distance"),
                                                               treatment = "incident_window_pw_osrm75100")

# gvp
GVP_spillover_osrm_list <- list(
  "0-10 miles"=spillover_anti_baseline_10_osrmpw, 
  "10-30 miles"=spillover_anti_baseline_1030_osrmpw, 
  "0-30 miles"=spillover_anti_baseline_30_osrmpw, 
  "30-50 miles"= spillover_anti_baseline_3050_osrmpw, 
  "50-75 miles"=spillover_anti_baseline_5075_osrmpw,
  "75-100 miles"=spillover_anti_baseline_75100_osrmpw
)

coef_map_spillover_osrm <- c(
  "incident_window_pw_osrm10" = "Treatment",
  "incident_window_pw_osrm1030" = "Treatment",
  "incident_window_pw_osrm30" = "Treatment",
  "incident_window_pw_osrm3050" = "Treatment",
  "incident_window_pw_osrm5075" = "Treatment",
  "incident_window_pw_osrm75100" = "Treatment"
)

gof_add_pw_spillover_osrm <- data.frame(
  raw = c("# of treated obs.","Distance from shooting places", "Control group"),
  mod1 = c(nrow(subset(merged_spillover_data_baseline,incident_window_pw_osrm10 == 1)),
           "0-10 miles", "> 10 miles"), 
  mod2 = c(nrow(subset(merged_spillover_data_baseline,incident_window_pw_osrm1030 == 1)),
           "10-30 miles", "> 30 miles"), 
  mod3 = c(nrow(subset(merged_spillover_data_baseline,incident_window_pw_osrm30 == 1)),
           "0-30 miles", "> 30 miles"), 
  mod4 = c(nrow(subset(merged_spillover_data_baseline,incident_window_pw_osrm3050 == 1)),
           "30-50 miles", "> 50 miles"), 
  mod5 = c(nrow(subset(merged_spillover_data_baseline,incident_window_pw_osrm5075 == 1)),
           "50-75 miles",  "> 75 miles"), 
  mod6 = c(nrow(subset(merged_spillover_data_baseline,incident_window_pw_osrm75100 == 1)),
           "75-100 miles", "> 100 miles"), 
  stringsAsFactors = FALSE
)

GVP_spillover_osrmpw <- gt_table(GVP_spillover_osrm_list, coef_map_spillover_osrm,  c("nobs"),
                                 gof_add_pw_spillover_osrm,"Spill-over Effect; GVP Contributions; Street-level Distance",
                                 "Log Dollar Outcome; Population-weighted Centroid; Competitive Districts"
)
GVP_spillover_osrmpw


# gunlobby

spillover_pro_baseline_10_osrmpw <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 0, 10, type = "osrm_distance"),
                                                           outcome = "pro_log_sum",
                                                           treatment = "incident_window_pw_osrm10")
spillover_pro_baseline_1030_osrmpw <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 10, 30, type = "osrm_distance"),
                                                             outcome = "pro_log_sum",
                                                             treatment = "incident_window_pw_osrm1030")
spillover_pro_baseline_30_osrmpw <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 0, 30, type = "osrm_distance"),
                                                           outcome = "pro_log_sum",
                                                           treatment = "incident_window_pw_osrm30")

spillover_pro_baseline_3050_osrmpw <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 30, 50, type = "osrm_distance"),
                                                             outcome = "pro_log_sum",
                                                             treatment = "incident_window_pw_osrm3050")
spillover_pro_baseline_5075_osrmpw <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 50, 75, type = "osrm_distance"),
                                                             outcome = "pro_log_sum",
                                                             treatment = "incident_window_pw_osrm5075")
spillover_pro_baseline_75100_osrmpw <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 75, 100, type = "osrm_distance"),
                                                              outcome = "pro_log_sum",
                                                              treatment = "incident_window_pw_osrm75100")

GLobby_spillover_osrm_list <- list(
  "0-10 miles"=spillover_pro_baseline_10_osrmpw, 
  "10-30 miles"=spillover_pro_baseline_1030_osrmpw, 
  "0-30 miles"=spillover_pro_baseline_30_osrmpw, 
  "30-50 miles"= spillover_pro_baseline_3050_osrmpw, 
  "50-75 miles"=spillover_pro_baseline_5075_osrmpw,
  "75-100 miles"=spillover_pro_baseline_75100_osrmpw
)


GLobby_spillover_osrmpw <- gt_table(GLobby_spillover_osrm_list, coef_map_spillover_osrm,  c("nobs"),
                                    gof_add_pw_spillover_osrm,"Spill-over Effect; Gun Lobby Contributions; Street-level Distance",
                                    "Log Dollar Outcome; Population-weighted Centroid; Competitive Districts"
)
GLobby_spillover_osrmpw


### 5.3.4A before and after -----------------------------------------------


spillover_anti_baseline_10_osrmpw0017 <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 0, 10, 2000, 2017,type = "osrm_distance"),
                                                                treatment = "incident_window_pw_osrm10")
spillover_anti_baseline_1030_osrmpw0017 <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 10, 30, 2000, 2017,type = "osrm_distance"),
                                                                  treatment = "incident_window_pw_osrm1030")
spillover_anti_baseline_30_osrmpw0017 <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 0, 30, 2000, 2017,type = "osrm_distance"),
                                                                treatment = "incident_window_pw_osrm30")

spillover_anti_baseline_3050_osrmpw0017 <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 30, 50, 2000, 2017,type = "osrm_distance"),
                                                                  treatment = "incident_window_pw_osrm3050")

spillover_anti_baseline_5075_osrmpw0017 <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 50, 75, 2000, 2017,type = "osrm_distance"),
                                                                  treatment = "incident_window_pw_osrm5075")
spillover_anti_baseline_75100_osrmpw0017 <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 75, 100, 2000, 2017,type = "osrm_distance"),
                                                                   treatment = "incident_window_pw_osrm75100")

# gvp
GVP_spillover_osrm_list0017 <- list(
  "0-10 miles"=spillover_anti_baseline_10_osrmpw0017, 
  "10-30 miles"=spillover_anti_baseline_1030_osrmpw0017, 
  "0-30 miles"=spillover_anti_baseline_30_osrmpw0017, 
  "30-50 miles"= spillover_anti_baseline_3050_osrmpw0017, 
  "50-75 miles"=spillover_anti_baseline_5075_osrmpw0017,
  "75-100 miles"=spillover_anti_baseline_75100_osrmpw0017
)


gof_add_pw_spillover_osrm0017 <- data.frame(
  raw = c("# of treated obs.","Distance from shooting places", "Control group"),
  mod1 = c(nrow(subset(merged_spillover_data_baseline0017,incident_window_pw_osrm10 == 1)),
           "0-10 miles", "> 10 miles"), 
  mod2 = c(nrow(subset(merged_spillover_data_baseline0017,incident_window_pw_osrm1030 == 1)),
           "10-30 miles", "> 30 miles"), 
  mod3 = c(nrow(subset(merged_spillover_data_baseline0017,incident_window_pw_osrm30 == 1)),
           "0-30 miles", "> 30 miles"), 
  mod4 = c(nrow(subset(merged_spillover_data_baseline0017,incident_window_pw_osrm3050 == 1)),
           "30-50 miles", "> 50 miles"), 
  mod5 = c(nrow(subset(merged_spillover_data_baseline0017,incident_window_pw_osrm5075 == 1)),
           "50-75 miles",  "> 75 miles"), 
  mod6 = c(nrow(subset(merged_spillover_data_baseline0017,incident_window_pw_osrm75100 == 1)),
           "75-100 miles", "> 100 miles"), 
 
  stringsAsFactors = FALSE
)

GVP_spillover_osrmpw0017 <- gt_table(GVP_spillover_osrm_list0017, coef_map_spillover_osrm,  c("nobs"),
                                     gof_add_pw_spillover_osrm0017,"Spill-over Effect; GVP Contributions; Street-level Distance; 2000-2017",
                                     "Log Dollar Outcome; Population-weighted Centroid; Competitive Districts"
)
GVP_spillover_osrmpw0017

# after 2018


spillover_anti_baseline_10_osrmpw1824 <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 0, 10, 2018, 2024,type = "osrm_distance"),
                                                                treatment = "incident_window_pw_osrm10")
spillover_anti_baseline_1030_osrmpw1824 <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 10, 30, 2018, 2024,type = "osrm_distance"),
                                                                  treatment = "incident_window_pw_osrm1030")
spillover_anti_baseline_30_osrmpw1824 <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 0, 30, 2018, 2024,type = "osrm_distance"),
                                                                treatment = "incident_window_pw_osrm30")

spillover_anti_baseline_3050_osrmpw1824 <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 30, 50, 2018, 2024,type = "osrm_distance"),
                                                                  treatment = "incident_window_pw_osrm3050")

spillover_anti_baseline_5075_osrmpw1824 <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 50, 75, 2018, 2024,type = "osrm_distance"),
                                                                  treatment = "incident_window_pw_osrm5075")
spillover_anti_baseline_75100_osrmpw1824 <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 75, 100, 2018, 2024,type = "osrm_distance"),
                                                                   treatment = "incident_window_pw_osrm75100")

# gvp
GVP_spillover_osrm_list1824 <- list(
  "0-10 miles"=spillover_anti_baseline_10_osrmpw1824, 
  "10-30 miles"=spillover_anti_baseline_1030_osrmpw1824, 
  "0-30 miles"=spillover_anti_baseline_30_osrmpw1824, 
  "30-50 miles"= spillover_anti_baseline_3050_osrmpw1824, 
  "50-75 miles"=spillover_anti_baseline_5075_osrmpw1824,
  "75-100 miles"=spillover_anti_baseline_75100_osrmpw1824
  
)


gof_add_pw_spillover_osrm1824 <- data.frame(
  raw = c("# of treated obs.","Distance from shooting places", "Control group"),
  mod1 = c(nrow(subset(merged_spillover_data_baseline1824,incident_window_pw_osrm10 == 1)),
           "0-10 miles", "> 10 miles"), 
  mod2 = c(nrow(subset(merged_spillover_data_baseline1824,incident_window_pw_osrm1030 == 1)),
           "10-30 miles", "> 30 miles"), 
  mod3 = c(nrow(subset(merged_spillover_data_baseline1824,incident_window_pw_osrm30 == 1)),
           "0-30 miles", "> 30 miles"), 
  mod4 = c(nrow(subset(merged_spillover_data_baseline1824,incident_window_pw_osrm3050 == 1)),
           "30-50 miles", "> 50 miles"), 
  mod5 = c(nrow(subset(merged_spillover_data_baseline1824,incident_window_pw_osrm5075 == 1)),
           "50-75 miles",  "> 75 miles"), 
  mod6 = c(nrow(subset(merged_spillover_data_baseline1824,incident_window_pw_osrm75100 == 1)),
           "75-100 miles", "> 100 miles"), 
  stringsAsFactors = FALSE
)

GVP_spillover_osrmpw1824 <- gt_table(GVP_spillover_osrm_list1824, coef_map_spillover_osrm,  c("nobs"),
                                     gof_add_pw_spillover_osrm1824,"Spill-over Effect; GVP Contributions; Street-level Distance; 2018-2024",
                                     "Log Dollar Outcome; Population-weighted Centroid; Competitive Districts"
)
GVP_spillover_osrmpw1824



### 5.3.4AA pro-gun ------------------------------------------------------



spillover_pro_baseline_10_osrmpw0017 <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 0, 10, 2000, 2017,type = "osrm_distance"),
                                                                outcome="pro_log_sum",
                                                               treatment = "incident_window_pw_osrm10")
spillover_pro_baseline_1030_osrmpw0017 <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 10, 30, 2000, 2017,type = "osrm_distance"),
                                                                 outcome="pro_log_sum",
                                                                 treatment = "incident_window_pw_osrm1030")
spillover_pro_baseline_30_osrmpw0017 <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 0, 30, 2000, 2017,type = "osrm_distance"),
                                                               outcome="pro_log_sum",
                                                               treatment = "incident_window_pw_osrm30")

spillover_pro_baseline_3050_osrmpw0017 <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 30, 50, 2000, 2017,type = "osrm_distance"),
                                                                 outcome="pro_log_sum",
                                                                 treatment = "incident_window_pw_osrm3050")

spillover_pro_baseline_5075_osrmpw0017 <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 50, 75, 2000, 2017,type = "osrm_distance"),
                                                                 outcome="pro_log_sum",
                                                                 treatment = "incident_window_pw_osrm5075")
spillover_pro_baseline_75100_osrmpw0017 <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 75, 100, 2000, 2017,type = "osrm_distance"),
                                                                  outcome="pro_log_sum",
                                                                  treatment = "incident_window_pw_osrm75100")

# gvp
Gun_spillover_osrm_list0017 <- list(
  "0-10 miles"=spillover_pro_baseline_10_osrmpw0017, 
  "10-30 miles"=spillover_pro_baseline_1030_osrmpw0017, 
  "0-30 miles"=spillover_pro_baseline_30_osrmpw0017, 
  "30-50 miles"= spillover_pro_baseline_3050_osrmpw0017, 
  "50-75 miles"=spillover_pro_baseline_5075_osrmpw0017,
  "75-100 miles"=spillover_pro_baseline_75100_osrmpw0017
)



Gun_spillover_osrmpw0017 <- gt_table(Gun_spillover_osrm_list0017, coef_map_spillover_osrm,  c("nobs"),
                                     gof_add_pw_spillover_osrm0017,"Spill-over Effect; Pro-Gun Contributions; Street-level Distance; 2000-2017",
                                     "Log Dollar Outcome; Population-weighted Centroid; Competitive Districts"
) %>% print()

# after 2018


spillover_pro_baseline_10_osrmpw1824 <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 0, 10, 2018, 2024,type = "osrm_distance"),
                                                               outcome="pro_log_sum",
                                                               treatment = "incident_window_pw_osrm10")
spillover_pro_baseline_1030_osrmpw1824 <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 10, 30, 2018, 2024,type = "osrm_distance"),
                                                                 outcome="pro_log_sum",
                                                                 treatment = "incident_window_pw_osrm1030")
spillover_pro_baseline_30_osrmpw1824 <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 0, 30, 2018, 2024,type = "osrm_distance"),
                                                               outcome="pro_log_sum",
                                                               treatment = "incident_window_pw_osrm30")

spillover_pro_baseline_3050_osrmpw1824 <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 30, 50, 2018, 2024,type = "osrm_distance"),
                                                                 outcome="pro_log_sum",
                                                                 treatment = "incident_window_pw_osrm3050")

spillover_pro_baseline_5075_osrmpw1824 <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 50, 75, 2018, 2024,type = "osrm_distance"),
                                                                 outcome="pro_log_sum",
                                                                 treatment = "incident_window_pw_osrm5075")
spillover_pro_baseline_75100_osrmpw1824 <- anti_pro_spillover_DID(data_spillover_baseline_farpw(merged_spillover_data, 75, 100, 2018, 2024,type = "osrm_distance"),
                                                                  outcome="pro_log_sum",
                                                                  treatment = "incident_window_pw_osrm75100")

Gun_spillover_osrm_list1824 <- list(
  "0-10 miles"=spillover_pro_baseline_10_osrmpw1824, 
  "10-30 miles"=spillover_pro_baseline_1030_osrmpw1824, 
  "0-30 miles"=spillover_pro_baseline_30_osrmpw1824, 
  "30-50 miles"= spillover_pro_baseline_3050_osrmpw1824, 
  "50-75 miles"=spillover_pro_baseline_5075_osrmpw1824,
  "75-100 miles"=spillover_pro_baseline_75100_osrmpw1824
)

Gun_spillover_osrmpw1824 <- gt_table(Gun_spillover_osrm_list1824, coef_map_spillover_osrm,  c("nobs"),
                                     gof_add_pw_spillover_osrm1824,"Spill-over Effect; Pro-Gun Contributions; Street-level Distance; 2018-2024",
                                     "Log Dollar Outcome; Population-weighted Centroid; Competitive Districts"
) %>% print()







#### 5.3.5 Unfiltered Whole Districts -----------------------------------------------


spillover_anti_whole_10_pw <- anti_pro_spillover_DID(data_spillover_2000_farpw(merged_spillover_data, 0, 10),
                                                     treatment = "incident_window_pw_10")
spillover_anti_whole_1030_pw <- anti_pro_spillover_DID(data_spillover_2000_farpw(merged_spillover_data, 10, 30),
                                                       treatment = "incident_window_pw_1030")

# check
spillover_anti_whole_1030_pw_2017int <- anti_pro_spillover_DID(data_spillover_2000_farpw(merged_spillover_data, 10, 30),
                                                               treatment = "incident_window_pw_1030", year_separate = TRUE, year_before = 2017)

spillover_anti_whole_1030_pw_2017int

spillover_anti_whole_1030_pw20182024 <- anti_pro_spillover_DID(data_spillover_2000_farpw(merged_spillover_data, 10, 30, 2018, 2024),
                                                               treatment = "incident_window_pw_1030")
spillover_anti_whole_1030_pw20182024

spillover_anti_whole_1030_pw200017 <- anti_pro_spillover_DID(data_spillover_2000_farpw(merged_spillover_data, 10, 30, 2000, 2017),
                                                             treatment = "incident_window_pw_1030")
spillover_anti_whole_1030_pw200017


spillover_anti_whole_30_pw <- anti_pro_spillover_DID(data_spillover_2000_farpw(merged_spillover_data, 0, 30),
                                                     treatment = "incident_window_pw_30")

spillover_anti_whole_3050_pw <- anti_pro_spillover_DID(data_spillover_2000_farpw(merged_spillover_data, 30, 50),
                                                       treatment = "incident_window_pw_3050")
spillover_anti_whole_5075_pw <- anti_pro_spillover_DID(data_spillover_2000_farpw(merged_spillover_data, 50, 75),
                                                       treatment = "incident_window_pw_5075")
spillover_anti_whole_75100_pw <- anti_pro_spillover_DID(data_spillover_2000_farpw(merged_spillover_data, 75, 100),
                                                        treatment = "incident_window_pw_75100")

# gvp
GVP_spillover_whole_list <- list(
  "0-10 miles"=spillover_anti_whole_10_pw, 
  "10-30 miles"=spillover_anti_whole_1030_pw, 
  "0-30 miles"=spillover_anti_whole_30_pw, 
  "30-50 miles"= spillover_anti_whole_3050_pw, 
  "50-75 miles"=spillover_anti_whole_5075_pw,
  "75-100 miles"=spillover_anti_whole_75100_pw
)


gof_add_pw_spillover_whole <- data.frame(
  raw = c("# of treated obs.","Distance from shooting places", "Control group"),
  mod1 = c(nrow(subset(merged_spillover_data_2000,incident_window_pw_10 == 1)),
           "0-10 miles", "> 10 miles"), 
  mod2 = c(nrow(subset(merged_spillover_data_2000,incident_window_pw_1030 == 1)),
           "10-30 miles", "> 30 miles"), 
  mod3 = c(nrow(subset(merged_spillover_data_2000,incident_window_pw_30 == 1)),
           "0-30 miles", "> 30 miles"), 
  mod4 = c(nrow(subset(merged_spillover_data_2000,incident_window_pw_3050 == 1)),
           "30-50 miles", "> 50 miles"), 
  mod5 = c(nrow(subset(merged_spillover_data_2000,incident_window_pw_5075 == 1)),
           "50-75 miles",  "> 75 miles"), 
  mod6 = c(nrow(subset(merged_spillover_data_2000,incident_window_pw_75100 == 1)),
           "75-100 miles", "> 100 miles"), 
  stringsAsFactors = FALSE
)

GVP_spillover_pw_whole <- gt_table(GVP_spillover_whole_list, coef_map_spillover,  c("nobs"),
                                   gof_add_pw_spillover_whole,"Spill-over Effect; GVP Contributions",
                                   "Log Dollar Outcome; Population-weighted Centroid; Whole (Unfiltered) Districts"
)
GVP_spillover_pw_whole

# gunlobby

spillover_pro_whole_10_pw <- anti_pro_spillover_DID(data_spillover_2000_farpw(merged_spillover_data, 0, 10),
                                                    outcome = "pro_log_sum",
                                                    treatment = "incident_window_pw_10")
spillover_pro_whole_1030_pw <- anti_pro_spillover_DID(data_spillover_2000_farpw(merged_spillover_data, 10, 30),
                                                      outcome = "pro_log_sum",
                                                      treatment = "incident_window_pw_1030")
spillover_pro_whole_30_pw <- anti_pro_spillover_DID(data_spillover_2000_farpw(merged_spillover_data, 0, 30),
                                                    outcome = "pro_log_sum",
                                                    treatment = "incident_window_pw_30")

spillover_pro_whole_3050_pw <- anti_pro_spillover_DID(data_spillover_2000_farpw(merged_spillover_data, 30, 50),
                                                      outcome = "pro_log_sum",
                                                      treatment = "incident_window_pw_3050")
spillover_pro_whole_5075_pw <- anti_pro_spillover_DID(data_spillover_2000_farpw(merged_spillover_data, 50, 75),
                                                      outcome = "pro_log_sum",
                                                      treatment = "incident_window_pw_5075")
spillover_pro_whole_75100_pw <- anti_pro_spillover_DID(data_spillover_2000_farpw(merged_spillover_data, 75, 100),
                                                       outcome = "pro_log_sum",
                                                       treatment = "incident_window_pw_75100")

GLobby_spillover_whole_list <- list(
  "0-10 miles"=spillover_pro_whole_10_pw, 
  "10-30 miles"=spillover_pro_whole_1030_pw, 
  "0-30 miles"=spillover_pro_whole_30_pw, 
  "30-50 miles"= spillover_pro_whole_3050_pw, 
  "50-75 miles"=spillover_pro_whole_5075_pw,
  "75-100 miles"=spillover_pro_whole_75100_pw
)


GLobby_spillover_pw_whole <- gt_table(GLobby_spillover_whole_list, coef_map_spillover,  c("nobs"),
                                      gof_add_pw_spillover_whole,"Spill-over Effect; Gun Lobby Contributions",
                                      "Log Dollar Outcome; Population-weighted Centroid; Whole (Unfiltered) Districts"
)
GLobby_spillover_pw_whole


### 5.3.5A before and after 2018 ------------------------------------------



spillover_anti_whole_10_pw_0017 <- anti_pro_spillover_DID(data_spillover_2000_farpw(merged_spillover_data, 0, 10, 2000, 2017),
                                                          treatment = "incident_window_pw_10")
spillover_anti_whole_1030_pw_0017 <- anti_pro_spillover_DID(data_spillover_2000_farpw(merged_spillover_data, 10, 30, 2000, 2017),
                                                            treatment = "incident_window_pw_1030")

spillover_anti_whole_30_pw_0017 <- anti_pro_spillover_DID(data_spillover_2000_farpw(merged_spillover_data, 0, 30, 2000, 2017),
                                                          treatment = "incident_window_pw_30")

spillover_anti_whole_3050_pw_0017 <- anti_pro_spillover_DID(data_spillover_2000_farpw(merged_spillover_data, 30, 50, 2000, 2017),
                                                            treatment = "incident_window_pw_3050")
spillover_anti_whole_5075_pw_0017 <- anti_pro_spillover_DID(data_spillover_2000_farpw(merged_spillover_data, 50, 75, 2000, 2017),
                                                            treatment = "incident_window_pw_5075")
spillover_anti_whole_75100_pw_0017 <- anti_pro_spillover_DID(data_spillover_2000_farpw(merged_spillover_data, 75, 100, 2000, 2017),
                                                             treatment = "incident_window_pw_75100")

# gvp
GVP_spillover_whole_list_0017 <- list(
  "0-10 miles"=spillover_anti_whole_10_pw_0017, 
  "10-30 miles"=spillover_anti_whole_1030_pw_0017, 
  "0-30 miles"=spillover_anti_whole_30_pw_0017, 
  "30-50 miles"= spillover_anti_whole_3050_pw_0017, 
  "50-75 miles"=spillover_anti_whole_5075_pw_0017,
  "75-100 miles"=spillover_anti_whole_75100_pw_0017
 
)

merged_spillover_data_2000_0017<- merged_spillover_data_2000 %>% 
  filter(year<=2017)

gof_add_pw_spillover_whole_0017 <- data.frame(
  raw = c("# of treated obs.","Distance from shooting places", "Control group"),
  mod1 = c(nrow(subset(merged_spillover_data_2000_0017,incident_window_pw_10 == 1)),
           "0-10 miles", "> 10 miles"), 
  mod2 = c(nrow(subset(merged_spillover_data_2000_0017,incident_window_pw_1030 == 1)),
           "10-30 miles", "> 30 miles"), 
  mod3 = c(nrow(subset(merged_spillover_data_2000_0017,incident_window_pw_30 == 1)),
           "0-30 miles", "> 30 miles"), 
  mod4 = c(nrow(subset(merged_spillover_data_2000_0017,incident_window_pw_3050 == 1)),
           "30-50 miles", "> 50 miles"), 
  mod5 = c(nrow(subset(merged_spillover_data_2000_0017,incident_window_pw_5075 == 1)),
           "50-75 miles",  "> 75 miles"), 
  mod6 = c(nrow(subset(merged_spillover_data_2000_0017,incident_window_pw_75100 == 1)),
           "75-100 miles", "> 100 miles"), 
  stringsAsFactors = FALSE
)

GVP_spillover_pw_whole_0017 <- gt_table(GVP_spillover_whole_list_0017, coef_map_spillover,  c("nobs"),
                                        gof_add_pw_spillover_whole_0017,"Spill-over Effect; GVP Contributions; 2000-2017",
                                        "Log Dollar Outcome; Population-weighted Centroid; Whole (Unfiltered) Districts"
)
GVP_spillover_pw_whole_0017


# after 2018



spillover_anti_whole_10_pw_1824 <- anti_pro_spillover_DID(data_spillover_2000_farpw(merged_spillover_data, 0, 10, 2018, 2024),
                                                          treatment = "incident_window_pw_10")
spillover_anti_whole_1030_pw_1824 <- anti_pro_spillover_DID(data_spillover_2000_farpw(merged_spillover_data, 10, 30, 2018, 2024),
                                                            treatment = "incident_window_pw_1030")

spillover_anti_whole_30_pw_1824 <- anti_pro_spillover_DID(data_spillover_2000_farpw(merged_spillover_data, 0, 30, 2018, 2024),
                                                          treatment = "incident_window_pw_30")

spillover_anti_whole_3050_pw_1824 <- anti_pro_spillover_DID(data_spillover_2000_farpw(merged_spillover_data, 30, 50, 2018, 2024),
                                                            treatment = "incident_window_pw_3050")
spillover_anti_whole_5075_pw_1824 <- anti_pro_spillover_DID(data_spillover_2000_farpw(merged_spillover_data, 50, 75, 2018, 2024),
                                                            treatment = "incident_window_pw_5075")
spillover_anti_whole_75100_pw_1824 <- anti_pro_spillover_DID(data_spillover_2000_farpw(merged_spillover_data, 75, 100, 2018, 2024),
                                                             treatment = "incident_window_pw_75100")

# gvp
GVP_spillover_whole_list_1824 <- list(
  "0-10 miles"=spillover_anti_whole_10_pw_1824, 
  "10-30 miles"=spillover_anti_whole_1030_pw_1824, 
  "0-30 miles"=spillover_anti_whole_30_pw_1824, 
  "30-50 miles"= spillover_anti_whole_3050_pw_1824, 
  "50-75 miles"=spillover_anti_whole_5075_pw_1824,
  "75-100 miles"=spillover_anti_whole_75100_pw_1824
)

merged_spillover_data_2000_1824<- merged_spillover_data_2000 %>% 
  filter(year>=2018)

gof_add_pw_spillover_whole_1824 <- data.frame(
  raw = c("# of treated obs.","Distance from shooting places", "Control group"),
  mod1 = c(nrow(subset(merged_spillover_data_2000_1824,incident_window_pw_10 == 1)),
           "0-10 miles", "> 10 miles"), 
  mod2 = c(nrow(subset(merged_spillover_data_2000_1824,incident_window_pw_1030 == 1)),
           "10-30 miles", "> 30 miles"), 
  mod3 = c(nrow(subset(merged_spillover_data_2000_1824,incident_window_pw_30 == 1)),
           "0-30 miles", "> 30 miles"), 
  mod4 = c(nrow(subset(merged_spillover_data_2000_1824,incident_window_pw_3050 == 1)),
           "30-50 miles", "> 50 miles"), 
  mod5 = c(nrow(subset(merged_spillover_data_2000_1824,incident_window_pw_5075 == 1)),
           "50-75 miles",  "> 75 miles"), 
  mod6 = c(nrow(subset(merged_spillover_data_2000_1824,incident_window_pw_75100 == 1)),
           "75-100 miles", "> 100 miles"), 
  stringsAsFactors = FALSE
)

GVP_spillover_pw_whole_1824 <- gt_table(GVP_spillover_whole_list_1824, coef_map_spillover,  c("nobs"),
                                        gof_add_pw_spillover_whole_1824,"Spill-over Effect; GVP Contributions; 2018-2024",
                                        "Log Dollar Outcome; Population-weighted Centroid; Whole (Unfiltered) Districts"
)
GVP_spillover_pw_whole_1824




ranked_distances_district_comp <- merged_spillover_data_baseline %>% 
  # already excluding incident districts and filtering out non-competitive dist.
  group_by(year, month, id) %>% 
  arrange(incident_spillover_distance_pw, .by_group = TRUE) %>% # using pw distance
  mutate(rank = row_number()) %>%
  ungroup()

top5_per_shooting_comp <- ranked_distances_district_comp %>%
  filter(rank <= 5)

rank_summary_distance_comp <- top5_per_shooting_comp %>%
  group_by(rank) %>%
  summarise(
    avg_distance_miles = mean(incident_spillover_distance_pw, na.rm = TRUE),
    sd  = sd(incident_spillover_distance_pw, na.rm = TRUE)
    # count_notes       = n_distinct(origin_note)
  ) %>%
  arrange(rank)

# jitter plot

ggplot() +
  geom_hline(
    yintercept = seq(25, 125, by = 25),
    linetype   = "dotted",    color      = "gray50"
  ) +
  # jitter dot
  geom_jitter(data = top5_per_shooting,
              aes(x = factor(rank), y = distance_miles),
              width = 0.2,      size = 1.5,
              alpha = 0.6,    color = "steelblue"
  ) +
  labs(
    title    = "Average Distance from School Shooting Venues",
    subtitle = "Top 5 Closest House Districts; Population Weighted; Competitive Districts",
    x        = "Rank in Distance",
    y        = "Distance to Poplation-weighted Centroid (miles)",
    caption = "Excluding districts where a shooting takes place."
  ) +
  scale_y_continuous(
    breaks = seq(0, 200, by = 25),
    limits = c(0, 200)
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(hjust = 0.5),
    plot.subtitle    = element_text(hjust = 0.5, size = 10),
    axis.title       = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )


