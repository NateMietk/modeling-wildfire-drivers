
if(!file.exists(file.path(extraction_anthro, 'fpa_density_distance_to_transportation.gpkg'))) {
  # Create distance from ignition point
  
  transportation_inputs <- list('railroads', 'transmission_lines', 'primary_rds', 'secondary_rds', 'tertiary_rds')
  if(!file.exists(file.path(extraction_anthro, 'distance_fpa_to_transportation.gpkg'))) {
    distance_to_fire_list <- lapply(transportation_inputs, 
                                    FUN = function(x) { 
                                      if(!file.exists(file.path( proc_transportation_dir, paste0('distance_fpa_', x, '.rds')))) {
                                        print(paste0('Starting ', x))
                                        
                                        line_coords <- st_coordinates(get(x))
                                        fire_coords <- st_coordinates(fpa_clean)
                                        
                                        line_df <- as_tibble(line_coords) %>%
                                          mutate(line_ids = L1)
                                        
                                        # compute KNN between fires and urban poly vertices
                                        nearest_neighbors <- as_tibble(bind_cols(nabor::knn(data = line_coords[, c('X', 'Y')],
                                                                                            fire_coords,
                                                                                            k = 1))) %>%
                                          mutate(fpa_id = as.data.frame(fpa_clean)$fpa_id,
                                                 line_ids = nn.idx,
                                                 closest_centroid = as.numeric(nn.dists)) %>%
                                          left_join(., line_df, by = 'line_ids') %>%
                                          dplyr::select(fpa_id, vertex_ids, line_ids, closest_centroid) %>%
                                          left_join(., lines, by = 'line_ids') %>%
                                          st_as_sf(sf_column_name = "geom") %>%
                                          arrange(desc(fpa_id)) 
                                        
                                        distance_to_fire <- fpa_clean %>%
                                          arrange(desc(fpa_id)) %>%
                                          mutate(
                                            !!paste0('distance_to_', x) := st_distance(
                                              st_geometry(nearest_neighbors),
                                              st_geometry(.), by_element = TRUE)) %>%
                                          dplyr::select(fpa_id, !!paste0('distance_to_', x))
                                        
                                        write_rds(distance_to_fire,
                                                  file.path(
                                                    proc_transportation_dir,
                                                    paste0('distance_fpa_', x, '.rds')
                                                  ))
                                        system(paste0('aws s3 sync ', proc_transportation_dir, ' ', s3_proc_prefix))
                                      } else {
                                        print(paste0('Starting ', x))
                                        
                                        read_rds(file.path( proc_transportation_dir, paste0('distance_fpa_', x, '.rds')))
                                      }
                                    } 
    )
    
    distance_to_fire <- distance_to_fire_list %>%
      bind_cols() %>%
      dplyr::select(fpa_id, distance_to_rail_rds, distance_to_tl, distance_to_primary_rds, distance_to_secondary_rds, distance_to_tertiary_rds, shape)
    
    st_write(distance_to_fire,
             file.path(extraction_anthro, paste0('distance_fpa_to_transportation', '.gpkg')))
    system(paste0('aws s3 sync ', summary_dir, ' ', s3_proc_extractions))
  } else {
    distance_to_fire <- st_read(file.path(extraction_anthro, 'distance_fpa_to_transportation.gpkg')) %>%
      mutate(vector_railroad_distance = distance_to_rail_rds,
             vector_transmission_lines_distance = distance_to_tl,
             vector_primary_rds_distance = distance_to_primary_rds,
             vector_secondary_rds_distance = distance_to_secondary_rds,
             vector_tertiary_rds_distance = distance_to_tertiary_rds) %>%
      dplyr::select(fpa_id, vector_railroad_distance, vector_transmission_lines_distance, vector_primary_rds_distance, 
                    vector_secondary_rds_distance, vector_tertiary_rds_distance)
  }
  
  ## Create density of transportation at 1km grid
  
  if (!file.exists(file.path(proc_transportation_dir, "railroad_distance.tif"))) {
    
    rasterize_shapefile(shp = railroads, out_rst = 'railroads.tif')
    railroad_density <- get_density(out_pro = 'railroads.tif', out_den = 'railroad_density.tif')
    railroad_distance <- get_distance(out_rst = 'railroads.tif', out_dis = 'railroad_distance.tif')
    
    system(paste0("aws s3 sync ",  processed_dir, " ", s3_proc_prefix))
    
  } else {
    railroad_density <- raster(file.path(proc_transportation_dir, "railroad_density.tif"))
    gridded_railroad_distance <- raster(file.path(proc_transportation_dir, "railroad_distance.tif")) %>%
      mask(states)
  }
  
  if (!file.exists(file.path(proc_transportation_dir, "transmission_lines_distance.tif"))) {
    
    rasterize_shapefile(shp = transmission_lines, out_rst = 'transmission_lines.tif')
    transmission_lines_density <- get_density(out_pro = 'transmission_lines.tif', out_den = 'transmission_lines_density.tif')
    transmission_lines_distance <- get_distance(out_rst = 'transmission_lines.tif', out_dis = 'transmission_lines_distance.tif')
    
    system(paste0("aws s3 sync ", processed_dir, " ", s3_proc_prefix))
    
  } else {
    transmission_lines_density <- raster(file.path(proc_transportation_dir, "transmission_lines_density.tif"))
    gridded_transmission_lines_distance <- raster(file.path(proc_transportation_dir, "transmission_lines_distance.tif")) %>%
      mask(states)
  }
  
  if (!file.exists(file.path(proc_transportation_dir, "primary_rds_distance.tif"))) {
    
    rasterize_shapefile(shp = primary_rds, out_rst = 'primary_rds.tif')
    primary_rds_density <- get_density(out_pro = 'primary_rds.tif', out_den = 'primary_rds_density.tif')
    primary_rds_distance <- get_distance(out_rst = 'primary_rds.tif', out_dis = 'primary_rds_distance.tif')
    
    system(paste0("aws s3 sync ", processed_dir, " ", s3_proc_prefix))
    
  } else {
    primary_rds_density <- raster(file.path(proc_transportation_dir, "primary_rds_density.tif")) 
    gridded_primary_rds_distance <- raster(file.path(proc_transportation_dir, "primary_rds_distance.tif")) %>%
      mask(states)
  }
  
  if (!file.exists(file.path(proc_transportation_dir, "secondary_rds_distance.tif"))) {
    
    rasterize_shapefile(shp = secondary_rds, out_rst = 'secondary_rds.tif')
    secondary_rds_density <- get_density(out_pro = 'secondary_rds.tif', out_den = 'secondary_rds_density.tif')
    secondary_rds_distance <- get_distance(out_rst = 'secondary_rds.tif', out_dis = 'secondary_rds_distance.tif')
    
    system(paste0("aws s3 sync ", processed_dir, " ", s3_proc_prefix))
    
  } else {
    secondary_rds_density <- raster(file.path(proc_transportation_dir, "secondary_rds_density.tif"))
    gridded_secondary_rds_distance <- raster(file.path(proc_transportation_dir, "secondary_rds_distance.tif")) %>%
      mask(states)
  }
  
  if (!file.exists(file.path(proc_transportation_dir, "tertiary_rds_distance.tif"))) {
  
    for(i in unique(tertiary_rds$STUSPS)) {
      if(!file.exists(file.path(proc_transportation_dir, 'tertiary_states', paste0('tertiary_rds_', i, '.tif')))) {
        slim_rds <- tertiary_rds %>%
          filter(.$STUSPS == i)
        print(head(slim_rds))
        rastered <- raster_mask
        rastered[] <- 1:ncell(rastered)
        
        rsp <- rastered %>%
          crop(., as(slim_rds, 'Spatial')) %>%
          rasterToPolygons(.) %>%
          st_as_sf()
        rp <- sf::st_intersection(slim_rds, rsp) 
        rp$length <- sf::st_length(rp)/1000
        x <- tapply(rp$length, rp$layer, sum)
        
        rastered <- raster(rastered)
        rastered[as.integer(names(x))] <- x
        
        writeRaster(rastered, filename= file.path(proc_transportation_dir, 'tertiary_states', paste0('tertiary_rds_', i, '.tif')), format="GTiff")
        gc()
      } else {
          print(paste0('Skipping ', i, ': already made'))
        }
      }
    rm(tertiary_rds)
    gc()
    rst_list <- list.files(file.path(proc_transportation_dir, 'tertiary_states'), pattern = '.tif$', full.names = TRUE)
    rst <- raster::stack(rst_list)
    tertiary_rds_rst <- calc(rst, sum, na.rm = TRUE)
    writeRaster(tertiary_rds_rst, filename= file.path(proc_transportation_dir, paste0('tertiary_rds.tif')), 
                format="GTiff", overwrite=TRUE)
    
    tertiary_rds_density <- get_density(out_pro = 'tertiary_rds.tif', out_den = 'tertiary_rds_density.tif')
    tertiary_rds_distance <- get_distance(out_rst = 'tertiary_rds.tif', out_dis = 'tertiary_rds_distance.tif')
    system(paste0("aws s3 sync ", s3_proc_prefix , " ", processed_dir))
    
  }  else {
    tertiary_rds_density <- raster(file.path(proc_transportation_dir, "tertiary_rds_density.tif"))
    gridded_tertiary_rds_distance <- raster(file.path(proc_transportation_dir, "tertiary_rds_distance.tif")) %>%
      mask(states)
  }
  
  fpa_slim <- fpa_clean %>%
    dplyr::select(fpa_id, fire_year)
  
  transportation_list <- c(railroad_density, transmission_lines_density, primary_rds_density, secondary_rds_density, tertiary_rds_density,
                           gridded_railroad_distance, gridded_transmission_lines_distance, gridded_primary_rds_distance, 
                           gridded_secondary_rds_distance, gridded_tertiary_rds_distance)
  
  transportation_extract <- velox(raster::stack(transportation_list))$extract_points(sp = fpa_slim) %>%
    as_tibble() 
  # Rename to the raster layer names
  colnames(transportation_extract) <- names(raster::stack(transportation_list))
  
  fpa_transportation_wide <- as.data.frame(transportation_extract) %>%
    mutate(fpa_id = as.data.frame(fpa_slim)$fpa_id) %>%
    mutate(gridded_railroad_distance = railroad_distance,
           gridded_transmission_lines_distance = transmission_lines_distance,
           gridded_primary_rds_distance = primary_rds_distance,
           gridded_secondary_rds_distance = secondary_rds_distance,
           gridded_tertiary_rds_distance = tertiary_rds_distance) %>%
    dplyr::select(fpa_id, gridded_railroad_distance, gridded_transmission_lines_distance, gridded_primary_rds_distance, 
                  gridded_secondary_rds_distance, gridded_tertiary_rds_distance) %>%
    left_join(as.data.frame(fpa_slim), ., by = 'fpa_id') %>%
    dplyr::select(-geom) %>%
    rename_all(tolower) %>% as_tibble() 
  
  fpa_density_distance_to_transportation <- distance_to_fire %>%
    left_join(., fpa_transportation_wide, by = 'fpa_id')
  
  st_write(fpa_density_distance_to_transportation, file.path(extraction_anthro, 'fpa_density_distance_to_transportation.gpkg'))
  
} else {
  fpa_density_distance_to_transportation <- st_read(file.path(extraction_anthro, 'fpa_density_distance_to_transportation.gpkg'))
}
