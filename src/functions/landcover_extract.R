# extract landcover by mode or mean
#

# landcover extraction function ---------------------------------------------------

ext_landcover <- function(rst_, pts, colname, buffer = 1000, FUN = 'mode'){
  require(parallel)
  require(foreach)
  require(raster)
  require(sf)
  require(dplyr)
  getmode <- function(v) {
    uniqv <- na.omit(unique(v))
    uniqv[base::which.max(tabulate(match(v, uniqv)))]
  }
  
  r <- raster(rst_)
  pts <- st_transform(pts, crs = crs(r, asText=TRUE))
  
  t0 <- Sys.time()
  corz <- detectCores()-1
  states <- unique(pts$STATE)
  
  cl <- makeCluster(corz)
  registerDoParallel(cl)
  
  results <- list()
  results <- foreach(i = 1:length(states)) %dopar% {
    require(sf)
    require(raster)
    
    sub_df <- pts[pts$STATE == states[i],]
    bb <- st_bbox(sub_df)
    bb[1] <- bb[1]-1000
    bb[2] <- bb[2]-1000
    bb[3] <- bb[3]+1000
    bb[4] <- bb[4]+1000
    pol <- st_as_sfc(bb)
    pol <- as_Spatial(pol)
    rst <- raster::crop(r, pol)
    rm(pol)
    rm(bb)
    
    # sub_df <- sub_df[1:10,] # for testing
    
    
    if(FUN == 'mode'){  
      sub_df$lf <- raster::extract(rst, sub_df, buffer = buffer,
                                   na.rm = TRUE, fun = function(x,...)getmode(x))
    }
    if(FUN == 'mean'){
      sub_df$lf <- raster::extract(rst, sub_df, buffer = buffer,
                                   na.rm = TRUE, fun = mean) 
    }
    
    return(sub_df)
    rm(rst)
    rm(sub_df)
    gc()
  }
  print(Sys.time()-t0)
  
  stopCluster(cl)
  
  t0 <- Sys.time()
  print(t0)
  df<- do.call("rbind",results) %>%
    rename(colname=lf)
  
  return(df)
}