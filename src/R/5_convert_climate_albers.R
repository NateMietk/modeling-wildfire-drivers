stat <- c('mean', '95th', 'numdays95th')
vars <- c('aet', 'def', 'ffwi', 'fm100', 'pdsi', 'pr', 'tmmx', 'vpd', 'vs')

for(j in stat) {
  for(i in vars) {
    var_rst <- list.files(file.path('data', 'climate', j), pattern = i, full.names = TRUE)
    
    if(length(var_rst) > 0) {
      
      pboptions(type = 'txt', use_lb = TRUE)
      cl <- makeCluster(getOption("cl.cores", detectCores()))
      
      pblapply(var_rst,
               FUN = function(x, p4string_ea, j) {
                 require(tidyverse)
                 require(raster)
                 require(rgdal)
                 
                 base <- basename(x) 
                 rst <- stack(x)
    
                 if(!file.exists(file.path('data', 'climate', 'albers', j, base))) {
                   
                   rst <- projectRaster(rst, res = 1000, crs = p4string_ea, method = 'bilinear')
                   writeRaster(rst, file.path('data', 'climate', 'albers', j, base), format = 'GTiff')
                   } 
                 },
               p4string_ea = p4string_ea,
               j = j,
               cl = cl)
      stopCluster(cl)
      }
    }
}

system(paste0("aws s3 sync data/climate ", s3_proc_climate))
