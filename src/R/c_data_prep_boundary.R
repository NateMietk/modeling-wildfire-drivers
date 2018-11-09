# Download and import CONUS states
# Download will only happen once as long as the file exists
if (!exists("usa_shp")){
  usa_shp <- load_data(url = "https://www2.census.gov/geo/tiger/GENZ2016/shp/cb_2016_us_state_20m.zip",
                       dir = us_prefix,
                       layer = "cb_2016_us_state_20m",
                       outname = "usa") %>%
    sf::st_transform(p4string_ea) %>%
    dplyr::filter(!STUSPS %in% c("HI", "AK", "PR")) %>%
    dplyr::select(STATEFP, STUSPS)
  usa_shp$STUSPS <- droplevels(usa_shp$STUSPS)
}

# Create raster mask
if (!exists("raster_mask")) {
  raster_mask <- raster::raster()
  crs(raster_mask) <- crs(p4string_ea)
  extent(raster_mask) <- c(-2032092, 2515908, -2116850, 731150)
  nrow(raster_mask) <- 2848
  ncol(raster_mask) <- 4548
  res(raster_mask) <- 1000
  }

# Download and import the Level 3 Ecoregions data
# Download will only happen once as long as the file exists
if (!exists("ecoregions_l3")){
  if (!file.exists(file.path(bounds_dir, "us_eco_l3.gpkg"))) {
    
    ecoregions_l3 <- st_read(dsn = ecoregion_prefix, layer = "us_eco_l3", quiet= TRUE) %>%
      st_transform(st_crs(usa_shp)) %>%  # e.g. US National Atlas Equal Area
      dplyr::select(US_L3CODE, US_L3NAME, NA_L2CODE, NA_L2NAME, NA_L1CODE, NA_L1NAME) %>%
      st_make_valid() %>%
      st_intersection(., usa_shp)  %>%
      setNames(tolower(names(.))) %>%
      mutate(region = as.factor(if_else(na_l1name %in% c("EASTERN TEMPERATE FORESTS",
                                                         "TROPICAL WET FORESTS",
                                                         "NORTHERN FORESTS"), "East",
                                        if_else(na_l1name %in% c("NORTH AMERICAN DESERTS",
                                                                 "SOUTHERN SEMI-ARID HIGHLANDS",
                                                                 "TEMPERATE SIERRAS",
                                                                 "MEDITERRANEAN CALIFORNIA",
                                                                 "NORTHWESTERN FORESTED MOUNTAINS",
                                                                 "MARINE WEST COAST FOREST"), "West", "Central"))),
             regions = as.factor(if_else(region == "East" & stusps %in% c("FL", "GA", "AL", "MS", "LA", "AR", "TN", "NC", "SC", "TX", "OK"), "South East",
                                         if_else(region == "East" & stusps %in% c("ME", "NH", "VT", "NY", "PA", "DE", "NJ", "RI", "CT", "MI", "MD",
                                                                                  "MA", "WI", "IL", "IN", "OH", "WV", "VA", "KY", "MO", "IA", "MN"), "North East",
    
                                                                                              as.character(region))))) 
    st_write(ecoregions_l3, file.path(bounds_dir, 'us_eco_l3.gpkg'),
             driver = 'GPKG', delete_layer = TRUE)
    
    east <- ecoregions_l3 %>%
      filter(region == 'East') %>%
      st_union()
    st_write(east, file.path(bounds_dir, 'east.gpkg'),
             driver = 'GPKG', delete_layer = TRUE)  
    
    west <- ecoregions_l3 %>%
      filter(region != 'East') %>%
      st_union()
    st_write(west, file.path(bounds_dir, 'west.gpkg'),
             driver = 'GPKG', delete_layer = TRUE)
    
    system(paste0("aws s3 sync ", bounds_dir, " ", s3_proc_bounds))
    
  } else {
    ecoregions_l3 <- sf::st_read(file.path(bounds_dir, 'us_eco_l3.gpkg'))
    east <- sf::st_read(file.path(bounds_dir, 'east.gpkg'))
    west <- sf::st_read(file.path(bounds_dir, 'west.gpkg'))
  }
}

# Rasterize level 1 ecoregions
if(!file.exists(file.path(proc_bounds_dir, 'gridded_ecoregions_l1.tif'))) {
  gridded_ecoregions_l1 <- fasterize::fasterize(ecoregions_l3, raster_mask, field = "na_l1code")
  writeRaster(gridded_ecoregions_l1, file.path(proc_bounds_dir, 'gridded_ecoregions_l1.tif'))
  system(paste0("aws s3 sync ", proc_bounds_dir, " ", s3_proc_bounds))
  } else {
    gridded_ecoregions_l1 <- raster::raster(file.path(proc_bounds_dir, 'gridded_ecoregions_l1.tif'))
  }

# Rasterize level 2 ecoregions
if(!file.exists(file.path(proc_bounds_dir, 'gridded_ecoregions_l2.tif'))) {
  gridded_ecoregions_l2 <- fasterize::fasterize(ecoregions_l3, raster_mask, field = "na_l2code")
  writeRaster(gridded_ecoregions_l2, file.path(proc_bounds_dir, 'gridded_ecoregions_l2.tif'))
  system(paste0("aws s3 sync ", proc_bounds_dir, " ", s3_proc_bounds))
  } else {
    gridded_ecoregions_l2 <- raster::raster(file.path(proc_bounds_dir, 'gridded_ecoregions_l2.tif'))
  }

# Rasterize level 3 ecoregions
if(!file.exists(file.path(proc_bounds_dir, 'gridded_ecoregions_l3.tif'))) {
  gridded_ecoregions_l3 <- fasterize::fasterize(ecoregions_l3, raster_mask, field = "us_l3code")
  writeRaster(gridded_ecoregions_l3, file.path(proc_bounds_dir, 'gridded_ecoregions_l3.tif'))
  system(paste0("aws s3 sync ", proc_bounds_dir, " ", s3_proc_bounds))
  } else {
    gridded_ecoregions_l3 <- raster::raster(file.path(proc_bounds_dir, 'gridded_ecoregions_l3.tif'))
  }

# Rasterize states
if(!file.exists(file.path(proc_bounds_dir, 'gridded_states.tif'))) {
  gridded_states <- fasterize::fasterize(usa_shp, raster_mask, field = "STUSPS")
  writeRaster(gridded_states, file.path(proc_bounds_dir, 'gridded_states.tif'))
  system(paste0("aws s3 sync ", proc_bounds_dir, " ", s3_proc_bounds))
  } else {
    gridded_states <- raster::raster(file.path(proc_bounds_dir, 'gridded_states.tif'))
  }

# Create longitude and latitude rasters
if(!file.exists(file.path(proc_bounds_dir, 'gridded_lonitude.tif'))) {
  gridded_lonitude <- init(gridded_states, 'x')
  gridded_latitude <- init(gridded_states, 'y')
  
  writeRaster(gridded_lonitude, file.path(proc_bounds_dir, 'gridded_lonitude.tif'))
  writeRaster(gridded_latitude, file.path(proc_bounds_dir, 'gridded_latitude.tif'))
  system(paste0("aws s3 sync ", proc_bounds_dir, " ", s3_proc_bounds))
}
