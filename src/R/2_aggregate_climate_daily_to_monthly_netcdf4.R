
source("src/R/a_make_dirs.R")

usa_shp <- st_read(dsn = file.path(raw_prefix, "cb_2016_us_state_20m/"), layer = "cb_2016_us_state_20m") %>%
                   st_transform("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

usa_shp <- as(usa_shp, "Spatial")

netcdf_import <- function(file, masks){

  x <- c("lubridate", "rgdal", "ncdf4", "raster", "tidyverse", "snowfall", "tools")
  lapply(x, require, character.only = TRUE)

  file_split <- file %>%
    basename %>%
    strsplit(split = "_") %>%
    unlist
  var <- file_split[1]
  year <- substr(file_split[2], start = 1, stop = 4)
  endyear <- substr(file_split[2], start = 5, stop = 8)

  # Create dirctories
  data_pro <- file.path(prefix,  "climate")
  data_var <- file.path(data_pro, var)
  dir_mean <- file.path(data_var, "monthly_mean")
  dir_90th <- file.path(data_var, "monthly_mean_90thpct")
  dir_95th <- file.path(data_var, "monthly_mean_95thpct")

  # Check if directory exists for all variable aggregate outputs, if not then create
  var_dir <- list(data_pro, data_var, dir_mean, dir_90th, dir_95th)
  lapply(var_dir, function(x) if(!dir.exists(x)) dir.create(x, showWarnings = FALSE))

  start_date <- as.Date(paste(year, "01", "01", sep = "-"))
  end_date <- as.Date(paste(ifelse(year == endyear, year, endyear), "12", "31", sep = "-"))
  date_seq <- seq(start_date, end_date, by = "1 month")
  month_seq <- month(date_seq)

  out_name <- basename(file_path_sans_ext(file))

  nc <- nc_open(file)
  nc_att <- attributes(nc$var)$names
  ncvar <- ncvar_get(nc, nc_att)
  rm(nc)
  tvar <- aperm(ncvar, c(3,2,1))
  rm(ncvar)
  proj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0 "

  rasters <- brick(tvar, crs= proj)
  rm(tvar)
  extent(rasters) <- c(-124.793, -67.043, 25.04186, 49.41686)
  names(rasters) <- paste(var, year(date_seq),
                                     unique(month(date_seq, label = TRUE)),
                                     sep = "_")
  # Mean
  monthly_mean <- mask(rasters, masks)

  for(i in 1979:2016) {
    if(!file.exists(file.path(dir_mean, paste0(var, "_", i, "_mean",".tif")))) {
      r_sub <- subset(monthly_mean,  grep(i, names(rasters))) # subset based on year
      writeRaster(r_sub, filename = file.path(dir_mean, paste0(var, "_", i, "_mean",".tif")),
                format = "GTiff")
      }
    }
}

monthly_files <- list.files(climate_prefix, pattern = "nc",
                          full.names = TRUE, recursive = TRUE)

sfInit(parallel = TRUE, cpus = parallel::detectCores())

sfExportAll()

sfLapply(monthly_files,
         netcdf_import,
         masks = usa_shp)
sfStop()
