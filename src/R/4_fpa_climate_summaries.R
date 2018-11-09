if (!exists("fpa_ll")) {
  if (file.exists(file.path(processed_dir, "fpa_ll.gpkg"))) {

    fpa_ll <- st_read(file.path(processed_dir, "fpa_ll.gpkg")) %>%
      mutate(year_month_day = floor_date(ymd(DISCOVERY_DATE), "month"))

  } else if (!file.exists(file.path(processed_dir, "fpa_clean.gpkg"))) {

    fpa_ll <- fpa_clean %>%
      st_transform(crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") %>%
      mutate(year_month_day = floor_date(ymd(DISCOVERY_DATE), "month"))

    st_write(fpa_ll, file.path(processed_dir, "fpa_ll.gpkg"), overwrite = TRUE)

    system(paste0("aws s3 sync ",
                  processed_dir, " ",
                  s3_proc_prefix))

  } else if (exists("fpa_clean")) {

    fpa_ll <- st_read(file.path(processed_dir, "fpa_clean.gpkg")) %>%
      st_transform(crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0") %>%
      mutate(year_month_day = floor_date(ymd(DISCOVERY_DATE), "month"))

    st_write(fpa_ll, file.path(processed_dir, "fpa_ll.gpkg"))

    system(paste0("aws s3 sync ",
                  processed_dir, " ",
                  s3_proc_prefix))
  }
}

stat <- c('95th')
vars <- c('aet', 'def', 'ffwi', 'fm100', 'pdsi', 'pr', 'tmmx', 'vpd', 'vs')

unique_states <- unique(fpa_ll$STATE)

for (j in stat){
  for (i in vars) {
    fpa_summaries <- list()
    counter <- 1

    tifs <- check_tifs(j, i)

    if (is.null(tifs)) {

      print(paste0('Skipping ', j, ' ', i))

    } else if (!file.exists(file.path(summaries_dir, j, paste0('fpa_', i, '_', j, '_summaries.rds')))) {

      # subset the fpa-fod data based on state grouping variable
      # this increases the speed of this function and only needs ~40GB of memory max.
      for (k in 1:length(unique_states)) {

        # create a subdataframe based on state subset
        sub_df <- subset(fpa_ll, fpa_ll$STATE == unique_states[k])

        print(paste0('Working on ', counter, " of ", length(unique_states), " for the ", j, ' ', i))

        # setup parallel environment
        cl <- makeCluster(detectCores())
        registerDoParallel(cl)

        # extract climate time series data based on point or polygon locations.
        # this extract is pulling in point data, so fun = mean does not matter
        extractions <- foreach (i = tifs) %dopar% {

          res <- raster::extract(raster::stack(i), sub_df,
                                 na.rm = TRUE, fun = 'mean', df = TRUE)
        }
        system('aws s3 sync data/climate s3://earthlab-modeling-human-ignitions/climate')

        stopCluster(cl)

        # ensure that they all have the same length
        stopifnot(all(lapply(extractions, nrow) == nrow(sub_df)))

        if (!file.exists(file.path(summaries_dir, j, 'state', paste0('fpa_', k, '_', i, '_', j, '_summaries.rds')))) {

          # convert to a data frame
          extraction_df <- extractions %>%
            bind_cols %>%
            as_tibble %>%
            mutate(index = ID) %>%
            select(-starts_with("ID")) %>%
            rename(ID = index) %>%
            mutate(FPA_ID = data.frame(sub_df)$FPA_ID) %>%
            dplyr::select(-starts_with('X')) %>%
            gather(variable, value, -FPA_ID, -ID) %>%
            filter(!is.na(value)) %>%
            mutate(FPA_ID = as.factor(FPA_ID),
                   ID = as.factor(ID)) %>%
            # clean the final, long climate data frame with linked fpa ids
            separate(variable,
                     into = c("variable", 'year', "statistic", "month"),
                     sep = "_|\\.") %>%
            mutate(day = '01',
                   year_month_day = as.Date(paste(year, month, day, sep='-')))

          # reduce the size of the dataframe to be joined during the get_climate_lags
          sub_df <- sub_df %>%
            dplyr::select(FPA_ID, year_month_day)

          # run get_climate_lags for the prior 24 months given a fpa-fod fire event
          # this will iteratively populate a list given each state grouping variable
          fpa_summaries_out <- get_lags(sub_df, extraction_df, sub_df$year_month_day, time_lag = 24) %>%
            dplyr::select(-year_month_day)

          write_rds(fpa_summaries_out, file.path(summaries_dir, j, 'state', paste0('fpa_', k, '_', i, '_', j, '_summaries.rds')))
          system('aws s3 sync data/extractions s3://earthlab-modeling-human-ignitions/extractions')

          fpa_summaries[[k]] <- fpa_summaries_out
          counter <- counter + 1
        } else {
          fpa_summaries_out <- read_rds(file.path(summaries_dir, j, 'state', paste0('fpa_', k, '_', i, '_', j, '_summaries.rds')))

          fpa_summaries[[k]] <- fpa_summaries_out
          counter <- counter + 1
        }

      }
      if (length(fpa_summaries) > 0) {
        # rbinds the iteratively populated list and creates a cohesive dataframe
        fpa_summaries <- do.call(rbind, fpa_summaries) # Convert to data frame format

        # save the final cleaned climate extractions joined with the fpa-fod database
        summary_name <- file.path(summaries_dir, j, paste0('fpa_', i, '_', j, '_summaries.rds'))
        write_rds(fpa_summaries, summary_name)

        # push to S3
        system('aws s3 sync data/extractions s3://earthlab-modeling-human-ignitions/extractions')
      }
    }

    # creating the final dataframe takes about 150 GB of RAM, this clears the cache so it can be run again
    gc()
  }
}
