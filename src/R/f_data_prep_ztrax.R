final_list <- list.files('data/anthro/ztrax/ztrax_raw_cumsum_1980_2015_1k', full.names = TRUE)

if(length(final_list) != 37){
  rst_list <- list.files(raw_ztrax_dir, pattern = '.gpkg$', full.names = TRUE)
  
  for(i in rst_list) {
    
    shp <- sf::st_read(i) %>%
      filter(built_class == 'Residential') %>%
      filter(YearBuilt != 0) %>%
      mutate(YearBuilt = ifelse(YearBuilt < 1980, 1980, YearBuilt))
    
    state_name <- basename(i) %>%
      stringr::str_extract_all(.,"\\(?[0-9,]+\\)?")
    
    cl <- makeCluster(6)
    registerDoParallel(cl)
    
    foreach(j = unique(shp$YearBuilt), .packages = c('tidyverse', 'sf', 'raster')) %dopar% {
      
      filter_shp <- shp %>%
        filter(YearBuilt == j)
      
      ras_template <- raster_mask
      
      rst_tmp <- raster::rasterize(filter_shp, ras_template, fun = 'count', field = 'built_class')
      
      names(rst_tmp) <- j
      raster::writeRaster(rst_tmp, file.path(stacked_ztrax_rst_dir, paste0('ztrax_count_', state_name, '_', j, '.tif')), 
                          format = "GTiff", overwrite = TRUE)
    }
    stopCluster(cl)
    system(paste0("aws s3 sync ", anthro_dir, " ", s3_proc_anthro))
  }
  
  
  for(j in 1980:2016) {
    rst_list <- list.files(stacked_ztrax_rst_dir, pattern = paste0(j, '.tif'), full.names = TRUE)
    
    out_rst <- raster::stack(rst_list)
    out_rst <- calc(out_rst, sum, na.rm =TRUE) %>%
      mask(as(usa_shp, 'Spatial'))
    
    raster::writeRaster(out_rst, file.path(count_ztrax_rst_dir, paste0('ztrax_count_', j, '.tif')),
                        format = "GTiff", overwrite = TRUE)
    system(paste0("aws s3 sync ", anthro_dir, " ", s3_proc_anthro))
  }
  
  grid_list <- list.files(count_ztrax_rst_dir, full.names = TRUE)
  rsts <- lapply(grid_list, raster)
  rsts <- Reduce("+", rsts, accumulate = TRUE)
  
  lapply(seq_along(rsts), function(x) {
    name <- gsub("count", "cumsum", grid_list[x])
    
    writeRaster(rsts[[x]], name, datatype = 'GTiff')
  })
  system(paste0("aws s3 sync ", anthro_dir, " ", s3_proc_anthro))
  
} else {
  ztrax_grid <- raster::stack(final_list)
}

# Get the total residential homes per 1km grid 
if(!file.exists(file.path(extraction_anthro, 'fpa_ztrax_1k.rds'))) {
  fpa_slim <- fpa_clean %>%
    dplyr::select(fpa_id, fire_year)
  
  ztrax_extract <- velox(ztrax_grid)$extract_points(sp = fpa_slim) %>%
    as_tibble() 
  # Rename to the raster layer names
  colnames(ztrax_extract) <- names(ztrax_grid)
  #
  wnames <- names(ztrax_grid)
  nnames <- paste0('ztrax_', str_split_fixed(wnames, fixed("_"), 3)[, 3])
  
  fpa_ztrax_wide <- as.data.frame(ztrax_extract) %>%
    mutate(fpa_id = as.data.frame(fpa_slim)$fpa_id) %>%
    left_join(., as.data.frame(fpa_slim), by = 'fpa_id') %>%
    dplyr::select(-geom) %>%
    rename_all(tolower) %>% as_tibble() %>%
    rename_at(vars(wnames), ~ nnames)
  
  dt <- data.table(fpa_ztrax_wide)
  
  setkey(dt, "fire_year")
  for(i in unique(dt[["fire_year"]])){
    dt[fire_year == i, ztrax:= get(paste("ztrax", i, sep = "_"))]
  }
  fpa_ztrax <- dt[,.SD, .SDcols = c("fpa_id", "fire_year", "ztrax")] %>%
    as_tibble()
  write_rds(fpa_ztrax, file.path(extraction_anthro, 'fpa_ztrax_1k.rds'))
  
  } else {
    fpa_ztrax <- read_rds(file.path(extraction_anthro, 'fpa_ztrax_1k.rds')) %>%
      as_tibble()
  }
