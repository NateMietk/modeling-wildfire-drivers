
# Spatially join the fpa database to the WUI
if (!exists('fpa_wui')) {
  if(!file.exists(file.path(proc_fire_dir, "fpa_wui_conus.gpkg"))) {
    #Import the Wildland-Urban Interface and process
    if (!exists('wui')) {
      if (!file.exists(file.path(wui_dir, "wui_bounds.gpkg"))) {
        
        wui <- st_read(dsn = file.path('data', 'anthro', 'wui_9010', "CONUS_WUI_cp12_d.gdb"),
                       layer = "CONUS_WUI_cp12") %>%
          st_transform(st_crs(usa_shp)) %>%
          mutate(Class90 = classify_wui(WUICLASS90),
                 Class00 = classify_wui(WUICLASS00),
                 Class10 = classify_wui(WUICLASS10)) 
        
        st_write(wui, file.path(wui_dir, "wui_bounds.gpkg"),
                 driver = "GPKG",
                 delete_layer = TRUE)
        system(paste0("aws s3 sync ", prefix, " ", s3_base))
        
      } else {
        wui <- st_read(dsn = file.path(wui_dir, "wui_bounds.gpkg"))
      }
    }

    fpa_wui_step1 <- fpa_clean %>%
      st_intersection(., wui) %>%
      st_make_valid()
    fpa_wui <- fpa_wui_step1 %>%
      mutate(class = as.factor(ifelse(DISCOVERY_YEAR >= 1992 | DISCOVERY_YEAR < 2000, as.character(Class90),
                                      ifelse(DISCOVERY_YEAR >= 2000 | DISCOVERY_YEAR < 2009, as.character(Class00),
                                             ifelse(DISCOVERY_YEAR >= 2010 | DISCOVERY_YEAR < 2016, as.character(Class10),
                                                    NA)))),
             ten_year = as.factor(ifelse(DISCOVERY_YEAR >= 1994 & DISCOVERY_YEAR <= 2004, '1994-2004',
                                         ifelse(DISCOVERY_YEAR >= 2005 & DISCOVERY_YEAR <= 2015, '2005-2015', NA))),
             bidecadal = as.factor(ifelse(DISCOVERY_YEAR >= 1991 & DISCOVERY_YEAR <= 1995, 1995,
                                          ifelse(DISCOVERY_YEAR >= 1996 & DISCOVERY_YEAR <= 2000, 2000,
                                                 ifelse(DISCOVERY_YEAR >= 2001 & DISCOVERY_YEAR <= 2005, 2005,
                                                        ifelse(DISCOVERY_YEAR >= 2006 & DISCOVERY_YEAR <= 2010, 2010,
                                                               ifelse(DISCOVERY_YEAR >= 2011 & DISCOVERY_YEAR <= 2015, 2015,
                                                                      DISCOVERY_YEAR )))))),
             decadal = as.factor(ifelse(DISCOVERY_YEAR >= 1991 & DISCOVERY_YEAR <= 2000, 1990,
                                        ifelse(DISCOVERY_YEAR >= 2001 & DISCOVERY_YEAR <= 2010, 2000,
                                               ifelse(DISCOVERY_YEAR >= 2011 & DISCOVERY_YEAR <= 2015, 2010,
                                                      DISCOVERY_YEAR )))),
             number_of_persons = ifelse( DISCOVERY_YEAR >= 1992 | DISCOVERY_YEAR < 2000, POP1990,
                                         ifelse( DISCOVERY_YEAR >= 2000 | DISCOVERY_YEAR < 2009, POP2000,
                                                 ifelse( DISCOVERY_YEAR >= 2010 | DISCOVERY_YEAR < 2016, POP2010, NA))),
             pop_den = ifelse( DISCOVERY_YEAR >= 1992 | DISCOVERY_YEAR < 2000, POPDEN1990,
                               ifelse( DISCOVERY_YEAR >= 2000 | DISCOVERY_YEAR < 2009, POPDEN2000,
                                       ifelse( DISCOVERY_YEAR >= 2010 | DISCOVERY_YEAR < 2016, POPDEN2010, NA ))),
             house_den = ifelse( DISCOVERY_YEAR >= 1992 | DISCOVERY_YEAR < 2000, HUDEN1990,
                                 ifelse( DISCOVERY_YEAR >= 2000 | DISCOVERY_YEAR < 2009, HUDEN2000,
                                         ifelse( DISCOVERY_YEAR >= 2010 | DISCOVERY_YEAR < 2016, HUDEN2010, NA ))),
             house_units = ifelse( DISCOVERY_YEAR >= 1992 | DISCOVERY_YEAR < 2000, HHU1990,
                                   ifelse( DISCOVERY_YEAR >= 2000 | DISCOVERY_YEAR < 2009, HHU2000,
                                           ifelse( DISCOVERY_YEAR >= 2010 | DISCOVERY_YEAR < 2016, HHU2010, NA ))),
             class_coarse =  as.factor(ifelse( class == 'High Urban' | class == 'Med Urban' | class == 'Low Urban', 'Urban',
                                               ifelse( class == 'Intermix WUI' | class == 'Interface WUI', 'WUI', as.character(class)))),
             seasons = as.factor(classify_seasons(DISCOVERY_DOY)),
             size = as.factor(classify_fire_size_cl(FIRE_SIZE_km2)),
             regions = as.factor(ifelse(regions == 'East', 'North East', as.character(regions)))) %>%
      rename_all(tolower) %>%
      dplyr::select(-stusps.1) %>%
      dplyr::select(-matches('(1990|2000|2010|00|90|s10|flag|wuiclass|veg|water|shape)'))
    
    st_write(fpa_wui, file.path(proc_fire_dir, "fpa_wui_conus.gpkg"),
             driver = "GPKG", delete_layer = TRUE)
    
    system(paste0("aws s3 sync ", processed_dir, " ", s3_proc_prefix))

  } else {
    fpa_wui <- st_read(dsn = file.path(proc_fire_dir, "fpa_wui_conus.gpkg"))
  }
}
