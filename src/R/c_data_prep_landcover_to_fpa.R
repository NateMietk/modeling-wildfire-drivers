# extracting landcover to fpa points 
# Used 32 cores and 240 GB ram and it ran in 45 minutes per landcover layer
# only really needed about 60 or 70 GB ram with 32 cores

if(!file.exists(file.path(extraction_anthro, 'fpa_landcover.rds'))) {
  # fpa import ---------------------------------------------------------------
  
  system("aws s3 sync s3://earthlab-modeling-human-ignitions/processed modeling-human-ignition/data/processed")
  
  fpa <- fpa_clean %>%
    select(FPA_ID,STATE)
  
  # landfire ----------------------------------------------------------------
  lf_file <- "us_140esp1.tif"
  lf_path <- "data/raw/landfire_esp"
  lf_s3 <- "s3://earthlab-modeling-human-ignitions/raw/landfire_esp"
  lf_result_file <- "fpa_w_landfire_esp.gpkg"
  
  
  system(paste("aws s3 sync",
               lf_s3,
               lf_path))
  
  df <- ext_landcover(rst_ = file.path(lf_path, lf_file),
                      pts = fpa,
                      colname = "lf_esp")
  
  st_write(df, file.path(proc_landcover_dir
                         , lf_result_file))
  system(paste("aws s3 cp",
               file.path(proc_landcover_dir, lf_file),
               s3_proc_extractions))
  
  # nlcd ----------------------------------------------------------------------
  year <- (c("2001","2006", "2011"))
  type <- (c("impervious","landcover"))
  
  for(y in 1:length(years)){
    for(t in 1:length(type)){
      file <- paste0("nlcd_",year[y],"_",type[t],"_2011_edition_2014_10_10.img")
      path <-paste0("modeling-human-ignition/data/raw/nlcd_",year[y],"_",type[t],"_2011_edition_2014_10_10")
      s3 <-paste0("s3://earthlab-modeling-human-ignitions/raw/nlcd_",year[y],"_",type[t],"_2011_edition_2014_10_10")
      result_file <- paste0("fpa_w_nlcd_",type[t],year[y],".gpkg")
      
      system(paste("aws s3 sync", s3, path))
      
      if(type[t] == 'landcover'){
        df <- ext_landcover(rst_ = file.path(path, file),
                            pts = fpa,
                            colname = paste("nlcd","lc",year[y], sep = "_"))
      }
      if(type[t] == 'impervious'){
        df <- ext_landcover(rst_ = file.path(path, file),
                            pts = fpa,
                            colname = paste("nlcd","imp",year[y], sep = "_"),
                            FUN = 'mean')
      }
      
      st_write(df, file.path(proc_landcover_dir,result_file))
      system(paste("aws s3 cp",
                   file.path(proc_landcover_dir, result_file),
                   s3_proc_extractions))
      
    }
  }
  
  # merging into one dataframe ---------------------------------------------
  
  source_path <- "data/processed/landcover"
  
  system(paste("aws s3 sync s3://earthlab-modeling-human-ignitions/processed/landcover",
               source_path))
  
  files <- list.files("data/processed/landcover")
  
  cl <- makeCluster(detectCores()-1)
  registerDoParallel(cl)
  
  fpa_list <- list()
  fpa_list <- foreach(i = 1:length(files)) %dopar% {
    require(sf)
    require(dplyr)
    
    if (i>1){
      fpa_list[[i]] <- st_read(file.path(source_path,files[i])) %>%
        select(-STATE) %>% st_set_geometry(NULL)
    } else{fpa_list[[i]] <- st_read(file.path(source_path,files[i])) 
    }
  }
  stopCluster(cl)
  # final <- bind_cols(fpa_list)
  
  final <- left_join(fpa_list[[1]], fpa_list[[2]]) %>%
    left_join(fpa_list[[3]]) %>%
    left_join(fpa_list[[4]]) %>%
    left_join(fpa_list[[5]]) %>%
    left_join(fpa_list[[6]]) %>%
    left_join(fpa_list[[7]])
  
  st_write(final, "data/results/fpa_w_all_landcover.gpkg")
  
  system(paste("aws s3 cp data/results/fpa_w_all_landcover.gpkg s3://earthlab-modeling-human-ignitions/processed/"))
  
  
  download.file('https://www.landfire.gov/CSV/ESP_08232017.csv', 
                destfile = file.path(landcover_dir, "ESP_08232017.csv"))
  lf_lookup <- fread(file.path(landcover_dir, "ESP_08232017.csv")) %>%
    dplyr::select(VALUE, ESP_Name, ZONE_NAME, ESPLF_Name) %>% as_tibble()
  
  fpa_landcover <- st_read(file.path(extraction_dir, 'fpa_w_all_landcover.gpkg')) %>%
    as.data.frame(.) %>%
    as_tibble() %>%
    dplyr::select(-STATE, -geom) %>%
    rename_all(tolower) %>%
    left_join(., as.data.frame(fpa_clean) %>% dplyr::select(fpa_id, fire_year, -geom), by = 'fpa_id') %>%
    mutate(nlcd_code = case_when(
      fire_year >= 1992 & fire_year <= 2001 ~ nlcd_lc_2001,
      fire_year >= 2002 & fire_year <= 2006 ~ nlcd_lc_2006,
      fire_year >= 2007 & fire_year <= 2015 ~ nlcd_lc_2011),
      pct_impervious = case_when(
        fire_year >= 1992 & fire_year <= 2001 ~ nlcd_imp_2001,
        fire_year >= 2002 & fire_year <= 2006 ~ nlcd_imp_2006,
        fire_year >= 2007 & fire_year <= 2015 ~ nlcd_imp_2011),
      VALUE = as.integer(lf_esp)) %>%
    dplyr::select(fpa_id, VALUE, nlcd_code, pct_impervious) %>%
    mutate(nlcd = classify_nlcd(nlcd_code)) %>%
    left_join(., lf_lookup, by = 'VALUE') %>%
    dplyr::select(fpa_id, ESP_Name, ZONE_NAME, ESPLF_Name, nlcd, pct_impervious) %>%
    mutate_if(is.character, as.factor)
  
  write_rds(fpa_landcover, file.path(extraction_anthro, 'fpa_landcover.rds'))
} else {
  fpa_landcover <- read_rds(file.path(extraction_anthro, 'fpa_landcover.rds'))
  }
