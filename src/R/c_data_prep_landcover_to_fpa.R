# extracting landcover to fpa points 
# Used 32 cores and 240 GB ram and it ran in 45 minutes per landcover layer
# only really needed about 60 or 70 GB ram with 32 cores

source("src/R/a_make_dirs.R")
source("src/functions/landcover_extract.R")

# create scrap folder for results and s3 path

result_path <- "data/results"
result_s3 <- "s3://earthlab-modeling-human-ignitions/processed/landcover"
dir.create(result_path)


# fpa import ---------------------------------------------------------------

system("aws s3 sync s3://earthlab-modeling-human-ignitions/processed modeling-human-ignition/data/processed")

fpa <- st_read("data/processed/fpa_ll.gpkg") %>%
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

st_write(df, file.path(result_path
                       , lf_result_file))
system(paste("aws s3 cp",
             file.path(result_path, lf_file),
             result_s3))

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
    
    st_write(df, file.path(result_path,result_file))
    system(paste("aws s3 cp",
                 file.path(result_path, result_file),
                 result_s3))
    
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


# final <- plyr::join_all(fpa_list, by = 'FPA_ID', type = 'left') # didn't work
st_write(final, "data/results/fpa_w_all_landcover.gpkg")

system(paste("aws s3 cp data/results/fpa_w_all_landcover.gpkg s3://earthlab-modeling-human-ignitions/processed/"))

# fixing names without redoing the whole thing
# fpa_list[[1]] <- rename(fpa_list[[1]], lf_esp = lf)
# fpa_list[[2]] <- rename(fpa_list[[2]], nlcd_lc_2001 = lf)
# fpa_list[[3]] <- rename(fpa_list[[3]], nlcd_lc_2006 = lf)
# fpa_list[[4]] <- rename(fpa_list[[4]], nlcd_lc_2011 = lf)
# fpa_list[[5]] <- rename(fpa_list[[5]], nlcd_imp_2001 = lf)
# fpa_list[[6]] <- rename(fpa_list[[6]], nlcd_imp_2006 = lf)
# fpa_list[[7]] <- rename(fpa_list[[7]], nlcd_imp_2011 = lf)
