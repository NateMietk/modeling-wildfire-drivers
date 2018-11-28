# Download and import CONUS states
# Download will only happen once as long as the file exists
if (!exists("states")){
  states <- load_data(url = "https://www2.census.gov/geo/tiger/GENZ2016/shp/cb_2016_us_state_20m.zip",
                       dir = raw_us,
                       layer = "cb_2016_us_state_20m",
                       outname = "usa") %>%
    sf::st_transform(p4string_ea) %>%
    dplyr::filter(!STUSPS %in% c("HI", "AK", "PR"))
  states$STUSPS <- droplevels(states$STUSPS)
}

# Download and import the Level 3 Ecoregions data
if (!exists("ecoregions_l3")) {
  if(!file.exists(file.path(bounds_dir, 'us_eco_l3.gpkg'))) {
    ecoregions_l3 <- load_data(url = "ftp://newftp.epa.gov/EPADataCommons/ORD/Ecoregions/us/us_eco_l3.zip",
                               dir = raw_ecoregionl3,
                               layer = "us_eco_l3",
                               outname = "ecoregions_l3") %>%
      sf::st_simplify(., preserveTopology = TRUE, dTolerance = 1000)  %>%
      sf::st_transform(st_crs(states)) %>%
      dplyr::mutate(NA_L3NAME = as.character(NA_L3NAME),
                    NA_L3NAME = ifelse(NA_L3NAME == 'Chihuahuan Desert',
                                       'Chihuahuan Deserts',
                                       NA_L3NAME))
    st_write(ecoregions_l3, file.path(bounds_dir, 'us_eco_l3.gpkg'))
  
} else {
  ecoregions_l3 <- st_read(file.path(bounds_dir, 'us_eco_l3.gpkg'))
  }
}

# Download and import the Level 4 Ecoregions data
if (!exists("ecoregions_l4")) {
  if(!file.exists(file.path(bounds_dir, 'us_eco_l4.gpkg'))) {
    ecoregions_l4 <- load_data(url = "ftp://newftp.epa.gov/EPADataCommons/ORD/Ecoregions/us/us_eco_l4.zip",
                               dir = raw_ecoregionl4,
                               layer = "us_eco_l4",
                               outname = "ecoregions_l4") %>%
      sf::st_simplify(., preserveTopology = TRUE, dTolerance = 1000)  %>%
      sf::st_transform(st_crs(states))
    st_write(ecoregions_l4, file.path(bounds_dir, 'us_eco_l4.gpkg'))
    
  } else {
    ecoregions_l4 <- st_read(file.path(bounds_dir, 'us_eco_l4.gpkg'))
  }
}

# Create raster mask
# 4k Fishnet
# if (!exists("fishnet_4k")) {
#   if (!file.exists(file.path(bounds_dir, "fishnet_4k.gpkg"))) {
#     fishnet_4k <- st_sf(geom=st_make_grid(states, cellsize = 4000, square = TRUE), crs=st_crs(states)) %>%
#       st_cast('MULTIPOLYGON') %>%
#       mutate(grid_4k = row_number())
#     
#     sf::st_write(fishnet_4k, file.path(bounds_dir, "fishnet_4k.gpkg"), driver = "GPKG")
#     
#     system(paste0("aws s3 sync ", bounds_dir, " ", s3_anc_prefix, "fishnet"))
#   } else {
#     fishnet_4k <- sf::st_read(file.path(bounds_dir, "fishnet_4k.gpkg"))
#   }
# }
