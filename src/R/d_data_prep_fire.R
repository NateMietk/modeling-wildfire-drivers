# Load and process FPA-FOD wildfire iginition data
if (!exists("fpa_clean")) {
  if (file.exists(file.path(processed_dir, "fpa_clean.gpkg"))){
    
    fpa_clean <- st_read(file.path(processed_dir, "fpa_clean.gpkg"))
    
  } else {
    if (!exists("fpa")) {
      fpa <- sf::st_read(dsn = file.path(fpa_prefix, "Data", "FPA_FOD_20170508.gdb"),
                         layer = "Fires", quiet= FALSE) %>%
        sf::st_transform(st_crs(usa_shp)) %>%
        sf::st_intersection(., st_union(usa_shp))
    }
    
    fpa_clean <- fpa %>%
      dplyr::select(FPA_ID, LATITUDE, LONGITUDE, ICS_209_INCIDENT_NUMBER, ICS_209_NAME, MTBS_ID, MTBS_FIRE_NAME,
                    FIRE_YEAR, DISCOVERY_DATE, DISCOVERY_DOY, STAT_CAUSE_DESCR, OWNER_CODE, OWNER_DESCR, FIRE_SIZE, STATE)  %>%
      filter(STAT_CAUSE_DESCR != 'Missing/Undefined') %>%
      dplyr::mutate(STAT_CAUSE_DESCR = str_replace(STAT_CAUSE_DESCR, " ", "_"),
                    cause = ifelse(STAT_CAUSE_DESCR == "Lightning", "Lightning", "Human"),
                    FIRE_SIZE_km2 = (FIRE_SIZE*4046.86)/1000000,
                    doy = day(DISCOVERY_DATE),
                    day = day(DISCOVERY_DATE),
                    month = ifelse(month(DISCOVERY_DATE) >= 10, month(DISCOVERY_DATE), paste0('0', month(DISCOVERY_DATE))),
                    year = FIRE_YEAR,
                    year_month_day = floor_date(ymd(DISCOVERY_DATE), "month")) %>%
      rename_all(tolower)
    
    sf::st_write(fpa_clean, file.path(processed_dir, "fpa_clean.gpkg"), driver = "GPKG", delete_layer = TRUE)
    
    system(paste0("aws s3 sync ", processed_dir, " ", s3_proc_prefix))
  }
}

system(paste0("aws s3 sync ", processed_dir, " ", s3_proc_prefix))

