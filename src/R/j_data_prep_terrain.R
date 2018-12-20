  # Note this has to be manually downloaded from Earth Explorer unfortuantely
  # Download elevation (https://lta.cr.usgs.gov/GTOPO30)
  
  # pull the elevation data from s3 if not already in the working data directory
  if (!file.exists(file.path(raw_dir, 'gtopo30', 'gt30w100n40.tif'))) {
    system('aws s3 sync s3://earthlab-modeling-human-ignitions/raw/gtopo30 data/raw/gtopo30')
  }
  
  elev_files <- list.files(file.path(raw_dir, 'gtopo30'), pattern = '.tif', full.names = TRUE)
  
if(!file.exists(file.path(extraction_terrain, 'extraction_terrain.rds'))) {
  if (!exists("elevation")) {
    if (!file.exists(file.path(proc_terrain_dir, 'elevation.tif'))) {
      
      elevation <- mosaic_rasters(elev_files) %>%
        raster::projectRaster(., raster_mask, res = 1000, crs = p4string_ea, method = 'bilinear') %>%
        raster::crop(as(usa_shp, 'Spatial')) %>%
        raster::mask(as(usa_shp, 'Spatial'))
      
      raster::writeRaster(elevation, filename = file.path(proc_terrain_dir, "elevation.tif"), format = "GTiff")
      
      system(paste0("aws s3 sync ", processed_dir, " ", s3_proc_prefix))
      
    } else {
      
      elevation <- raster::raster(file.path(proc_terrain_dir, "elevation.tif"))
    }
  }
  
  # Create slope raster
  if (!exists("slope")) {
    if (!file.exists(file.path(proc_terrain_dir, 'slope.tif'))) {
      
      slope <- raster::raster(file.path(processed_dir, "elevation.tif")) %>%
        raster::terrain(., opt = 'slope', unit = 'degrees')
      
      raster::writeRaster(slope, filename = file.path(proc_terrain_dir, "slope.tif"), format = "GTiff")
      
      system(paste0("aws s3 sync ", processed_dir, " ", s3_proc_prefix))
      
    } else {
      
      slope <- raster::raster(file.path(proc_terrain_dir, "slope.tif"))
    }
  }
  
  # Create terrain ruggedness
  if (!exists("ruggedness")) {
    if (!file.exists(file.path(proc_terrain_dir, 'ruggedness.tif'))) {
      
      ruggedness <- raster::raster(file.path(proc_terrain_dir, "elevation.tif")) %>%
        raster::terrain(., opt = 'TRI')
      
      raster::writeRaster(ruggedness, filename = file.path(proc_terrain_dir, "ruggedness.tif"), format = "GTiff")
      
      system(paste0("aws s3 sync ", processed_dir, " ", s3_proc_prefix))
      
    } else {
      ruggedness <- raster::raster(file.path(proc_terrain_dir, "ruggedness.tif"))
    }
  }
  
  # Create terrain roughness
  if (!exists("roughness")) {
    if (!file.exists(file.path(proc_terrain_dir,'roughness.tif'))) {
      
      roughness <- raster::raster(file.path(proc_terrain_dir,"elevation.tif")) %>%
        raster::terrain(., opt = 'roughness')
      
      raster::writeRaster(roughness, filename = file.path(proc_terrain_dir, "roughness.tif"), format = "GTiff")
      
      system(paste0("aws s3 sync ",
                    processed_dir, " ",
                    s3_proc_prefix))
      
    } else {
      
      roughness <- raster::raster(file.path(proc_terrain_dir, "roughness.tif"))
    }
  }
  
  # Create aspect
  if (!exists("aspect")) {
    if (!file.exists(file.path(proc_terrain_dir, 'aspect.tif'))) {
      
      aspect <- raster::raster(file.path(proc_terrain_dir, "elevation.tif")) %>%
        raster::terrain(., opt = 'aspect', unit = 'degrees')
      
      raster::writeRaster(aspect, filename = file.path(proc_terrain_dir, "aspect.tif"), format = "GTiff")
      
      #Create folded aspect
      get_folded_aspect <- function(aspect, ...) {
        abs(180 - abs(aspect - 225))
      }
      folded_aspect <- calc(aspect, fun = get_folded_aspect, na.rm = TRUE)
      raster::writeRaster(folded_aspect, filename = file.path(proc_terrain_dir, "folded_aspect.tif"), format = "GTiff")
      
      system(paste0("aws s3 sync ", processed_dir, " ", s3_proc_prefix))
      
    } else {
      aspect <- raster::raster(file.path(proc_terrain_dir,"aspect.tif"))
      folded_aspect <- raster::raster(file.path(proc_terrain_dir,"folded_aspect.tif"))
    }
  }

  # extract terrain variables by each fpa point and append to fpa dataframe
  terrain_list <- list.files(proc_terrain_dir, pattern = '.tif', full.names = TRUE)
  
  fpa_slim <- fpa_clean %>%
    dplyr::select(fpa_id)
  
  extracted_terrain <- velox(raster::stack(terrain_list))$extract_points(sp = fpa_slim) %>%
    as_tibble() 
  # Rename to the raster layer names
  colnames(extracted_terrain) <- names(raster::stack(terrain_list))
  
  # convert to a data frame
  fpa_terrain <- as.data.frame(extracted_terrain) %>%
    mutate(fpa_id = as.data.frame(fpa_slim)$fpa_id) %>%
    left_join(., as.data.frame(fpa_slim), by = 'fpa_id') %>%
    dplyr::select(-geom) %>%
    rename_all(tolower) %>% as_tibble() 
  
  # save processed/cleaned terrain extractions
  write_rds(fpa_terrain, file.path(extraction_terrain, 'extraction_terrain.rds'))
  
  system(paste0("aws s3 sync ", extraction_dir, " ", s3_proc_extractions)) 

  } else {
    fpa_terrain <- read_rds(file.path(extraction_terrain, 'extraction_terrain.rds'))
  }