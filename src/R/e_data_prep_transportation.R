
# Import ancillary data
# Railrods
if (!exists("railroads")) {
  if (!file.exists(file.path(transportation_processed_dir, "railroads.gpkg"))) {
    railroads <- sf::st_read(dsn = file.path(rails_prefix, 'tlgdb_2015_a_us_rails.gdb'), layer = 'Rails') %>%
      sf::st_transform(p4string_ea) %>%
      sf::st_intersection(., usa_shp) %>%
      dplyr::mutate(bool_rrds = 1)
    
    sf::st_write(railroads,
                 file.path(anthro_dir, "railroads.gpkg"),
                 driver = "GPKG",
                 update=TRUE,
                 delete_dsn=TRUE)
    
    system(paste0("aws s3 sync ",
                  transportation_processed_dir, " ",
                  s3_proc_prefix, "transportation/processed"))
  } else {
    
    railroads <- sf::st_read(dsn = file.path(transportation_processed_dir, "railroads.gpkg"))
  }
}

# Power transmission lines
if (!exists("transmission_lines")) {
  if (!file.exists(file.path(transportation_processed_dir, "transmission_lines.gpkg"))) {
    transmission_lines <- sf::st_read(dsn = file.path(tl_prefix, 'Electric_Power_Transmission_Lines.shp')) %>%
      sf::st_transform(p4string_ea) %>%
      sf::st_intersection(., usa_shp) %>%
      dplyr::mutate(bool_tl = 1) %>%
      dplyr::filter(st_is(., c("LINESTRING")))
    
    sf::st_write(transmission_lines,
                 file.path(anthro_dir, "transmission_lines.gpkg"),
                 driver = "GPKG")
    
    system(paste0("aws s3 sync ",
                  transportation_processed_dir, " ",
                  s3_proc_prefix, "transportation/processed"))
  } else {
    
    transmission_lines <- sf::st_read(dsn = file.path(transportation_processed_dir, "transmission_lines.gpkg"))
  }
}

# Primary Roads
if (!exists("primary_rds")) {
  if (!file.exists(file.path(transportation_processed_dir, "primary_rds.gpkg"))) {
    if (!exists("rds")) {
      rds <- sf::st_read(dsn = file.path(roads_prefix, "tlgdb_2015_a_us_roads.gdb"), layer = 'Roads')
    }
    
    primary_rds <- rds %>%
      dplyr::filter(MTFCC == "S1100") %>%
      sf::st_transform(p4string_ea) %>%
      sf::st_intersection(., st_transform(usa_shp, p4string_ea)) %>%
      dplyr::mutate(bool_prds = 1)
    
    sf::st_write(primary_rds,
                 file.path(transportation_processed_dir, "primary_rds.gpkg"),
                 driver = "GPKG")
    
    system(paste0("aws s3 sync ",
                  transportation_processed_dir, " ",
                  s3_proc_prefix, "transportation/processed"))
  } else {
    
    primary_rds <- sf::st_read(dsn = file.path(transportation_processed_dir, "primary_rds.gpkg"))
  }
}

# Secondary roads
if (!exists("secondary_rds")) {
  if (!file.exists(file.path(transportation_processed_dir, "secondary_rds.gpkg"))) {
    if (!exists("rds")) {
      rds <- sf::st_read(dsn = file.path(roads_prefix, "tlgdb_2015_a_us_roads.gdb"), layer = 'Roads')
    }
    
    secondary_rds <- rds %>%
      dplyr::filter(MTFCC == "S1200") %>%
      sf::st_transform(p4string_ea) %>%
      sf::st_intersection(., usa_shp) %>%
      dplyr::mutate(bool_srds = 1)
    
    
    sf::st_write(secondary_rds,
                 file.path(transportation_processed_dir, "secondary_rds.gpkg"),
                 driver = "GPKG")
    
    system(paste0("aws s3 sync ",
                  transportation_processed_dir, " ",
                  s3_proc_prefix, "transportation/processed"))
  } else {
    
    secondary_rds <- sf::st_read(dsn = file.path(transportation_processed_dir, "secondary_rds.gpkg"))
  }
}

# Tertiary roads
if (!exists("tertiary_rds")) {
  if (!file.exists(file.path(transportation_processed_dir, "tertiary_rds.gpkg"))) {
    if (!exists("rds")) {
      rds <- sf::st_read(dsn = file.path(roads_prefix, "tlgdb_2015_a_us_roads.gdb"), layer = 'Roads')
    }
    
    tertiary_rds <- rds %>%
      dplyr::filter(MTFCC == "S1400") %>%
      dplyr::select(LINEARID, STUSPS) %>%
      sf::st_transform(p4string_ea) %>%
      sf::st_intersection(., usa_shp) %>%
      dplyr::mutate(bool_trds = 1)
    
    rm(rds)
    gc()
    
    sf::st_write(tertiary_rds, file.path(transportation_processed_dir, "tertiary_rds.gpkg"),
                 driver = "GPKG", delete_layer = TRUE)
    
    system(paste0("aws s3 sync ", transportation_processed_dir, " ",
                  s3_proc_prefix, "transportation/processed"))
    
  } else {
    tertiary_rds <- sf::st_read(dsn = file.path(transportation_processed_dir, "tertiary_rds.gpkg")) 
  }
}
