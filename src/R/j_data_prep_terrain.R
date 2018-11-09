# Note this has to be manually downloaded from Earth Explorer unfortuantely
# Download elevation (https://lta.cr.usgs.gov/GTOPO30)

# pull the elevation data from s3 if not already in the working data directory
if (!file.exists(file.path(raw_prefix, 'gtopo30', 'gt30w100n40.tif'))) {
  system('aws s3 sync s3://earthlab-modeling-human-ignitions/raw/gtopo30 modeling-human-ignition/data/raw/gtopo30')
}

elev_files <- list.files(file.path(raw_prefix, 'gtopo30'),
                         pattern = '.tif',
                         full.names = TRUE)

if (!exists("elevation")) {
  if (!file.exists(file.path(processed_dir,'terrain', 'elevation.tif'))) {

    elevation <- mosaic_rasters(elev_files) %>%
      raster::projectRaster(., raster_mask, res = 1000, crs = p4string_ea, method = 'bilinear') %>%
      raster::crop(as(usa_shp, 'Spatial')) %>%
      raster::mask(as(usa_shp, 'Spatial'))

    raster::writeRaster(elevation, filename = file.path(processed_dir,'terrain', "elevation.tif"), format = "GTiff")

    system(paste0("aws s3 sync ",
                  processed_dir, " ",
                  s3_proc_prefix))

  } else {

    elevation <- raster::raster(file.path(processed_dir, 'terrain', "elevation.tif"))
  }
}

# Create slope raster
if (!exists("slope")) {
  if (!file.exists(file.path(processed_dir, 'terrain','slope.tif'))) {

    slope <- raster::raster(file.path(processed_dir, "elevation.tif")) %>%
      raster::terrain(., opt = 'slope', unit = 'degrees')

    raster::writeRaster(slope, filename = file.path(processed_dir, 'terrain',"slope.tif"), format = "GTiff")

    system(paste0("aws s3 sync ",
                  processed_dir, " ",
                  s3_proc_prefix))

  } else {

    slope <- raster::raster(file.path(processed_dir, 'terrain',"slope.tif"))
  }
}

# Create terrain ruggedness
if (!exists("ruggedness")) {
  if (!file.exists(file.path(processed_dir, 'terrain','ruggedness.tif'))) {

    ruggedness <- raster::raster(file.path(processed_dir, 'terrain',"elevation.tif")) %>%
      raster::terrain(., opt = 'TRI')

    raster::writeRaster(ruggedness, filename = file.path(processed_dir, 'terrain',"ruggedness.tif"), format = "GTiff")

    system(paste0("aws s3 sync ",
                  processed_dir, " ",
                  s3_proc_prefix))

  } else {

    ruggedness <- raster::raster(file.path(processed_dir,'terrain', "ruggedness.tif"))
  }
}

# Create terrain roughness
if (!exists("roughness")) {
  if (!file.exists(file.path(processed_dir, 'terrain','roughness.tif'))) {

    roughness <- raster::raster(file.path(processed_dir, 'terrain',"elevation.tif")) %>%
      raster::terrain(., opt = 'roughness')

    raster::writeRaster(roughness, filename = file.path(processed_dir, 'terrain',"roughness.tif"), format = "GTiff")

    system(paste0("aws s3 sync ",
                  processed_dir, " ",
                  s3_proc_prefix))

  } else {

    roughness <- raster::raster(file.path(processed_dir,'terrain', "roughness.tif"))
  }
}

# Create aspect
if (!exists("aspect")) {
  if (!file.exists(file.path(processed_dir,'terrain', 'aspect.tif'))) {

    aspect <- raster::raster(file.path(processed_dir,'terrain', "elevation.tif")) %>%
      raster::terrain(., opt = 'aspect', unit = 'degrees')

    raster::writeRaster(aspect, filename = file.path(processed_dir, 'terrain',"aspect.tif"), format = "GTiff")

    system(paste0("aws s3 sync ",
                  processed_dir, " ",
                  s3_proc_prefix))

  } else {

    aspect <- raster::raster(file.path(processed_dir, 'terrain',"aspect.tif"))
  }
}

# extract terrain variables by each fpa point and append to fpa dataframe
tifs <- list.files(processed_dir,
                         pattern = '.tif',
                         full.names = TRUE)

# extract terrain variables in parallel
sfInit(parallel = TRUE, cpus = parallel::detectCores())
sfExport(list = c("fpa_clean"))

extractions <- sfLapply(as.list(tifs),
                        fun = extract_one,
                        shapefile_extractor = fpa_clean)
sfStop()

# ensure that they all have the same length
stopifnot(all(lapply(extractions, nrow) == nrow(fpa_clean)))

# convert to a data frame
extraction_df <- extractions %>%
  bind_cols %>%
  as_tibble %>%
  mutate(FPA_ID = data.frame(fpa_clean)$FPA_ID) %>%
  dplyr::select(-starts_with('ID'))

# save processed/cleaned terrain extractions
write_rds(extraction_df, file.path(terrain_extract, 'terrain_extractions.rds'))

system(paste0("aws s3 sync ",
              summary_dir, " ",
              s3_proc_extractions))
