# Download and import CONUS states
# Download will only happen once as long as the file exists
if (!exists("usa_shp")){
  usa_shp <- load_data(url = "https://www2.census.gov/geo/tiger/GENZ2016/shp/cb_2016_us_state_20m.zip",
                       dir = us_prefix,
                       layer = "cb_2016_us_state_20m",
                       outname = "usa") %>%
    sf::st_transform(p4string_ea) %>%
    dplyr::filter(!STUSPS %in% c("HI", "AK", "PR"))
  usa_shp$STUSPS <- droplevels(usa_shp$STUSPS)
}

# Download and import the Level 4 Ecoregions data
# Download will only happen once as long as the file exists
if (!exists("ecoregions_l4")){
  ecoregions_l4 <- st_read(file.path(ecoregionl4_prefix, 'us_eco_l4_no_st.shp')) %>%
    sf::st_simplify(., preserveTopology = TRUE, dTolerance = 1000)  %>%
    sf::st_transform(st_crs(usa_shp))
}

# Download and import the Level 3 Ecoregions data
# Download will only happen once as long as the file exists
if (!exists("ecoregions_l3")){
  ecoregions_l3 <- st_read(file.path(ecoregion_prefix, 'us_eco_l3.shp')) %>%
    sf::st_simplify(., preserveTopology = TRUE, dTolerance = 1000)  %>%
    sf::st_transform(st_crs(usa_shp)) %>%
    dplyr::mutate(NA_L3NAME = as.character(NA_L3NAME),
                  NA_L3NAME = ifelse(NA_L3NAME == 'Chihuahuan Desert',
                                     'Chihuahuan Deserts',
                                     NA_L3NAME))
}

# Create raster mask
# 4k Fishnet
if (!exists("fishnet_4k")) {
  if (!file.exists(file.path(fishnet_path, "fishnet_4k.gpkg"))) {
    fishnet_4k <- sf::st_make_grid(usa_shp, cellsize = 4000, what = 'polygons') %>%
      sf::st_sf('geometry' = ., data.frame('fishid4k' = 1:length(.))) %>%
      sf::st_intersection(., st_union(usa_shp))
    
    sf::st_write(fishnet_4k,
                 file.path(fishnet_path, "fishnet_4k.gpkg"),
                 driver = "GPKG")
    
    system(paste0("aws s3 sync ",
                  fishnet_path, " ",
                  s3_anc_prefix, "fishnet"))
  } else {
    fishnet_4k <- sf::st_read(file.path(fishnet_path, "fishnet_4k.gpkg"))
  }
}
