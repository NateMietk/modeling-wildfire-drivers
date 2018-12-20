
if(!file.exists(file.path(extraction_dir, 'fpa_all_vars.rds'))) {
  if(!file.exists(file.path(extraction_dir, 'fpa_anthro.rds'))) {
    # Load all external custom functions
    file_sources <- list.files(file.path('src', 'R'), pattern="prep", 
                               full.names=TRUE, ignore.case=TRUE)
    invisible(sapply(file_sources, source, .GlobalEnv))
    
    fpa_anthro <- as.data.frame(fpa_wui) %>%
      dplyr::select(fpa_id, fire_size_ha, fire_year, discovery_doy, discovery_doy, seasons, stat_cause_descr, 
                    ignition, state, us_l3name, na_l2name, na_l1name, region, class, class_coarse) %>%
      left_join(as.data.frame(fpa_clean) %>% dplyr::select(fpa_id, owner_descr, -geom), by = 'fpa_id') %>%
      left_join(fpa_terrain, by = 'fpa_id') %>%
      left_join(fpa_ztrax %>% dplyr::select(-fire_year), by = 'fpa_id') %>%
      left_join(as.data.frame(fpa_density_distance_to_transportation) %>% dplyr::select(-geom), by = 'fpa_id') %>%
      left_join(fpa_landcover, by = 'fpa_id') %>%
      left_join(fpa_ciesen, by = 'fpa_id') %>%
      mutate_if(is.numeric, replace_na, 0) %>%
      mutate_if(is.character, as.factor) %>%
      as_tibble()
    
    write_rds(fpa_anthro, file.path(extraction_dir, 'fpa_anthro.rds'))
    system(paste0("aws s3 sync ", extraction_dir, " ", s3_proc_extractions)) 
  } else {
    fpa_anthro <- read_rds(file.path(extraction_dir, 'fpa_anthro.rds'))
  }
  
  if(!file.exists(file.path(extraction_dir, 'fpa_climate.rds'))) {
    if(!file.exists(file.path(extraction_climate_dir, 'fpa_climate_mean.rds'))) {
      
      fpa_climate_mean <- lapply(list.files(extraction_mean_climate_dir, full.names = TRUE),
                                 function(x) {
                                   if(x != "data/extractions/climate_extractions/mean/fpa_pr_mean_summaries.rds"){
                                     out_df <- read_rds(x) %>%
                                       aggregate_climate(.)
                                   } else {
                                     out_df <- read_rds(x) %>%
                                       aggregate_climate(., do_sums = FALSE)
                                   }
                                   return(out_df)
                                 }) %>%
        bind_cols(.) %>%
        dplyr::select(-fpa_id1, -fpa_id2, -fpa_id3, -fpa_id4, -fpa_id5, -fpa_id6, -fpa_id7, -fpa_id8) %>%
        distinct(.keep_all = TRUE)
      
      write_rds(fpa_climate_mean, file.path(extraction_climate_dir, 'fpa_climate_mean.rds'))
    } else {
      fpa_climate_mean <- read_rds(file.path(extraction_climate_dir, 'fpa_climate_mean.rds'))
    }
    
    if(!file.exists(file.path(extraction_climate_dir, 'fpa_climate_numdays95th.rds'))) {
      
      fpa_climate_numdays95th <- lapply(list.files(extraction_numdays95th_climate_dir, full.names = TRUE),
                                        function(x) {
                                          out_df <- read_rds(x) %>%
                                            aggregate_climate(., do_sums = FALSE, do_sums_only = TRUE)
                                          return(out_df) 
                                        }) %>%
        bind_cols(.) %>%
        dplyr::select(-fpa_id1, -fpa_id2, -fpa_id3, -fpa_id4) %>%
        distinct(.keep_all = TRUE)
      
      write_rds(fpa_climate_numdays95th, file.path(extraction_climate_dir, 'fpa_climate_numdays95th.rds'))
    } else {
      fpa_climate_numdays95th <- read_rds(file.path(extraction_climate_dir, 'fpa_climate_numdays95th.rds'))
    }
    
    fpa_climate <- fpa_climate_mean %>%
      left_join(., fpa_climate_numdays95th, by = 'fpa_id')
    
    write_rds(fpa_climate, file.path(extraction_dir, 'fpa_climate.rds'))
    
  } else {
    fpa_climate <- read_rds(file.path(extraction_dir, 'fpa_climate.rds'))
  }
  
  fpa_all_vars <- fpa_anthro %>%
    left_join(., fpa_climate, by = 'fpa_id') %>%
    mutate_if(is.character, as.factor) %>%
    as_tibble() %>%
    distinct(.keep_all = TRUE)
  
  write_rds(fpa_all_vars, file.path(extraction_dir, 'fpa_all_vars.rds'))
  system(paste0("aws s3 sync ", extraction_dir, " ", s3_proc_extractions)) 
  
} else {
  fpa_all_vars <- read_rds(file.path(extraction_dir, 'fpa_all_vars.rds'))
}