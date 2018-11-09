# List all decadal tiffs from the CIESIN gridded census products
anthro_list <- list.files(file.path(anthro_dir, 'gridded_census'),
                          pattern = '*.tif',
                          full.names = TRUE,
                          recursive = TRUE)
elevation <- raster::raster(file.path(processed_dir, 'terrain', "elevation.tif"))

pboptions(type = 'txt', use_lb = TRUE)
cl <- makeCluster(getOption("cl.cores", detectCores()/2)) # 8 cores on a m5d.4xlarge instance

pblapply(anthro_list, function(x, raster_mask, masks, dir) {
  require(tidyverse)
  require(raster)
  
  names <- x %>%
    basename()
  if(!file.exists(file.path(dir, names))) {
    rst <- raster::raster(x) %>%
      raster::projectRaster(., raster_mask, method = 'bilinear') %>%
      raster::mask(masks)
    
    writeRaster(rst, file.path(dir, names))
  }
}, 
raster_mask = elevation, 
masks = as(usa_shp, 'Spatial'),
dir = proc_gridded_census, 
cl = cl)

stopCluster(cl)