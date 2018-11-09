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


for(k in unique(fpa_clean$stat_cause_descr)) {
  
  fpa_subset_cause <- fpa_clean %>%
    filter(stat_cause_descr==k)

  cause_raster <- raster::stack()
  
  if(!file.exists(file.path('data', 'processed', 'fire', paste0(k, '_', i, '.tif')))) {
    for(i in unique(fpa_clean$year)) {

      fpa_subset_year <- fpa_subset_cause %>%
        filter(fire_year==i)

      for(j in unique(fpa_clean$month)) {
        fpa_subset_month<- fpa_subset_year %>%
          filter(month== as.character(j)) %>%
          mutate(unique_id= row_number())
        
        idx <- as.Date(paste0(i, '-', j, '-01'))
        varnames <- as.yearmon(idx)
        
        if(nrow(fpa_subset_month) == 0) {
          cause_raster_s <- raster_mask 
          cause_raster_s[] <- 0
          names(cause_raster_s) <- as.character(varnames)
          cause_raster <- stack(cause_raster, cause_raster_s)
          } else {
            fpa_subset_sp <- as(fpa_subset_month, 'Spatial')
            
            cause_raster_s <- raster::rasterize(fpa_subset_sp, raster_mask, fpa_subset_sp$unique_id, fun='count') 
            names(cause_raster_s) <- as.character(varnames)
            cause_raster <- stack(cause_raster, cause_raster_s)
          }
      }
      idx_full <- seq(as.Date(paste0(i, '-01-01')), as.Date(paste0(i,'-12-01')), by = 'month')
      yearmon_order <- data.frame(V1 = as.character(as.yearmon(idx_full))) %>%
        mutate(V1 = str_replace(V1, ' ', '.')) %>% as.matrix()
      
      cause_raster <- subset(cause_raster, yearmon_order)
      print(cause_raster)
      writeRaster(cause_raster, file.path('data', 'processed', 'fire', paste0(k, '_', i, '.tif')), format="GTiff")
      }
  }
}
system(paste0("aws s3 sync ", processed_dir, " ", s3_proc_prefix))

