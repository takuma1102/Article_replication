# Spill over


haversine <- function(lat1, long1, lat2, long2) {
  
  # convert degrees to radians
  lat1_rad <- lat1 * pi / 180
  long1_rad <- long1 * pi / 180
  lat2_rad <- lat2 * pi / 180
  long2_rad <- long2 * pi / 180
  
  d_lat <- lat2_rad - lat1_rad
  d_long <- long2_rad - long1_rad
  
  a <- sin(d_lat/2)^2 + cos(lat1_rad) * cos(lat2_rad) * sin(d_long/2)^2
  c <- 2 * asin(sqrt(a))
  r <- 3958.8  # earth radius in mile
  return(c * r)
}



geo_distance_flat <- function(df) {
  
  ss_points <- df %>%
    filter(GEOID == 'SS')
  
  # error safe; return empty
  if (nrow(ss_points) == 0) {
    return(data.frame())
  }
  
  districts <- df %>%
    filter(GEOID != 'SS')
  
  # by expand_grid, calculating all distances
  all_combinations <- expand_grid(
    shooting_id = 1:nrow(ss_points),
    district_id = 1:nrow(districts)
  ) %>%
    mutate(
      # shooting place
      origin_USPS = ss_points$USPS[shooting_id],
      origin_GEOID = ss_points$GEOID[shooting_id],
      origin_lat = ss_points$INTPTLAT[shooting_id],
      origin_lon = ss_points$INTPTLONG[shooting_id],
      origin_note = ss_points$note[shooting_id],
      
      # district info
      dest_USPS = districts$USPS[district_id],
      dest_GEOID = districts$GEOID[district_id],
      dest_lat = districts$INTPTLAT[district_id],
      dest_lon = districts$INTPTLONG[district_id],
      
      distance_miles = mapply(haversine, 
                              origin_lat, origin_lon,
                              dest_lat, dest_lon)
    ) %>%
    dplyr::select(-shooting_id, -district_id) %>%
    arrange(dest_USPS, dest_GEOID, 
            # origin_lat, 
            distance_miles) # this portion and sequence matters a lot
  
  return(all_combinations)
}




process_osrm_batch <- function(data, batch_size = 50, progress_file = "osrm_progress.rds") {
  
  # Check if existing progress file exists
  if(file.exists(progress_file)) {
    cat("Existing progress file found. Do you want to resume? (y/n): ")
    response <- readline()
    if(tolower(response) == "y") {  # no need to set n case
      progress_data <- readRDS(progress_file)
      cat("Progress restored. Resuming from batch", length(progress_data$completed_results), "\n")
      return(resume_osrm_processing(data, progress_data, batch_size, progress_file))
      # resume functio is defined later
    }
  }
  
  # 1. Extract unique locations
  cat("Extracting unique locations...\n")
  unique_origins <- data %>%
    distinct(origin_lon, origin_lat) %>%
    filter(!is.na(origin_lon), !is.na(origin_lat)) %>%
    mutate(
      origin_lon = as.numeric(origin_lon),  # just in case. already checked
      origin_lat = as.numeric(origin_lat),
      origin_id = row_number()
    )
  
  unique_destinations <- data %>%
    distinct(dest_lon, dest_lat) %>%
    filter(!is.na(dest_lon), !is.na(dest_lat)) %>%
    mutate(
      dest_lon = as.numeric(dest_lon),
      dest_lat = as.numeric(dest_lat),
      dest_id = row_number()
    )
  
  cat("Number of unique origins:", nrow(unique_origins), "\n")
  cat("Number of unique destinations:", nrow(unique_destinations), "\n")
  
  # 2. Split into batches
  origin_batches <- split(unique_origins, ceiling(seq_len(nrow(unique_origins)) / batch_size))
  # ceiling (nrow(... is incorrect. this just calculates total batch numer, but we have to assign a batch number to each!
  # split func assigns a number to each
  dest_batches <- split(unique_destinations, ceiling(seq_len(nrow(unique_destinations)) / batch_size))
  
  cat("Number of origin batches:", length(origin_batches), "\n") # this is the total number
  cat("Number of destination batches:", length(dest_batches), "\n")
  
  # 3. Create batch combinations
  batch_combinations <- expand_grid(
    origin_batch = 1:length(origin_batches),
    dest_batch = 1:length(dest_batches)
  ) %>%
    mutate(batch_id = row_number())
  
  total_batches <- nrow(batch_combinations)
  cat("Total number of batches:", total_batches, "\n")
  
  # 4. Initialize results storage
  all_results <- list()
  
  # 5. Execute batch processing
  for(batch_num in 1:total_batches) {
    origin_batch_idx <- batch_combinations$origin_batch[batch_num]
    dest_batch_idx <- batch_combinations$dest_batch[batch_num]
    
    cat(sprintf("Processing batch %d/%d... (origin batch %d, destination batch %d)\n", 
                batch_num, total_batches, origin_batch_idx, dest_batch_idx))
    
    tryCatch({
      # Convert to SF objects. SF means simple features
      # osrm is only compatible with SF
      origins_sf <- st_as_sf(origin_batches[[origin_batch_idx]], 
                             coords = c("origin_lon", "origin_lat"), 
                             crs = 4326)  # 4326 = WGS84. more common than 2157
      destinations_sf <- st_as_sf(dest_batches[[dest_batch_idx]], 
                                  coords = c("dest_lon", "dest_lat"), 
                                  crs = 4326)
      
      # Calculate OSRM table
      osrm_result <- osrmTable(
        src = origins_sf,
        dst = destinations_sf,
        measure = c("distance", "duration") # km and min.
      )
      
      # Convert results to long format
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
      cat("  → Completed (", nrow(batch_result), "records)\n")
      
      # Save progress every 5 batches
      if(batch_num %% 5 == 0) {
        progress_data <- list(
          completed_results = all_results[1:batch_num],
          unique_origins = unique_origins,
          unique_destinations = unique_destinations,
          last_batch = batch_num,
          total_batches = total_batches,
          batch_combinations = batch_combinations
        )
        saveRDS(progress_data, progress_file)
        cat("  → Progress saved (", batch_num, "/", total_batches, ")\n")
      }
      
      # API rate limiting: small delay
      Sys.sleep(0.5) # have to wait a bit
      
    }, error = function(e) {
      cat("  → Error occurred:", e$message, "\n")
      all_results[[batch_num]] <- data.frame(
        origin_idx = numeric(0),
        dest_idx = numeric(0),
        osrm_distance = numeric(0),
        osrm_duration = numeric(0)
      )
    })
  }
  
  # 6. Final save
  progress_data <- list(
    completed_results = all_results,
    unique_origins = unique_origins,
    unique_destinations = unique_destinations,
    last_batch = total_batches,
    total_batches = total_batches,
    batch_combinations = batch_combinations
  )
  saveRDS(progress_data, progress_file)
  
  # 7. Combine results
  cat("Combining results...\n")
  final_result <- bind_rows(all_results)
  
  return(list(
    lookup_table = final_result,
    unique_origins = unique_origins,
    unique_destinations = unique_destinations
  ))
}




data_spillover_baseline_farpw <- function(df, min_threshold,max_threshold,min_year=2000, max_year=2024,
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
    filter(year>=min_year)%>%
    filter(year<= max_year) 
  
  return(df)
}



data_spillover_2000_farnonpw <- function(df, # have to be a whole unfiltered data
                                         min_threshold, # be careful on what district you want to exclude
                                         # typically, min_threshold+25=max_threshold
                                         # min_threshold is a threshold to exclude; max for incident_window's threshold is max_threshold
                                         # to demarcate treatment and control groups
                                         max_threshold,
                                         min_year=2000, max_year=2024, # should avoid separating data?
                                         type="distance" # osrm_distance or minute
                                         
){
  df <- as.data.frame(df)  %>%
    
    arrange(id, time) %>%
    group_by(id) %>%
    mutate(last_incident_time_re = ifelse(incident == 1, time, NA)) %>% # specify the last incident time
    fill(last_incident_time_re, .direction = "down") %>%
    mutate(incident_window_re = if_else(!is.na(last_incident_time_re) & (time - last_incident_time_re <= 24), 1, 0),
           incident_window_re = as.integer(incident_window_re)) %>% 
    filter( incident_window_re==0) %>% 
    ungroup()
  
  # not correct
  # threshold <- min_threshold+25
  
  if (type == "distance"){
    col_name <- paste0("dist_follow_", max_threshold, "_nonpw")
    df <- df %>% 
      filter(!!sym(col_name) >= min_threshold | is.na(!!sym(col_name)))  # NA for control group
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
    filter(year>=min_year) %>%
    filter(year<=max_year) 
  
  return(df)
}

anti_pro_spillover_DID <- function(df, # setting default might be tricky to code
                                   outcome="anti_log_sum", treatment = "incident_window_nonpw_25",
                                   year_separate=FALSE, year_before=2017 # using <=
) {
  
  if (year_separate == FALSE){
    formula_str <- paste0(outcome , " ~ ", treatment, # mainly incident. history is not reasonable
                          " + bachelor + black + white + unemployment + log_income + rep_incumbent_before + gun_ratio_pct|id + time") 
    
  } else if (year_separate == TRUE){
    df$before_year <- as.numeric(df$year <= year_before)
    formula_str <- paste0(outcome , " ~ ", treatment,"*before_year + bachelor + black + white + unemployment + log_income + rep_incumbent_before + gun_ratio_pct|id + time") 
    
  }
  
  formula_obj <- as.formula(formula_str)
  feols(formula_obj, data = df,cluster = "id")
}

