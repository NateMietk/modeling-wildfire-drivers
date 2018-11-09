# General Helper functions

split_tibble_to_list <- function (x, f, drop = FALSE, ...) {
  faster_as_tibble <- function(x) {
    structure(x, class = c("tbl_df", "tbl", "data.frame"), row.names = as.character(seq_along(x[[1]])))
  }
  lapply(split(x = seq_len(nrow(x)), f = f,  ...),
         function(ind) faster_as_tibble(lapply(x, "[", ind)))
}

# convert to a data frame
flattenlist <- function(x){
  morelists <- sapply(x, function(xprime) class(xprime)[1]=="list")
  out <- c(x[!morelists], unlist(x[morelists], recursive=FALSE))
  if(sum(morelists)){
    Recall(out)
  }else{
    return(out)
  }
}

st_par <- function(sf_df, sf_func, n_cores, ...){
  # http://www.spatialanalytics.co.nz/post/2017/09/11/a-parallel-function-for-spatial-analysis-in-r/
  # Paralise any simple features analysis.
  # Create a vector to split the data set up by.
  split_vector <- rep(1:n_cores, each = nrow(sf_df) / n_cores, length.out = nrow(sf_df))

  # Perform GIS analysis
  split_results <- split(sf_df, split_vector) %>%
    mclapply(function(x) sf_func(x, ...), mc.cores = n_cores)

  # Combine results back together. Method of combining depends on the output from the function.
  if (class(split_results[[1]]) == 'list' ){
    result <- do.call(c, split_results)
    names(result) <- NULL
  } else {
    result <- do.call(rbind, split_results)
  }

  # Return result
  return(result)
}

# functions for b_get_anthro_data.R -------------------------------

load_data <- function(url, dir, layer, outname) {
  file <- paste0(dir, "/", layer, ".shp")

  if (!file.exists(file)) {
    download.file(url, destfile = paste0(dir, ".zip"))
    unzip(paste0(dir, ".zip"),
          exdir = dir)
    unlink(paste0(dir, ".zip"))

  }
  name <- paste0(outname, "_shp")
  name <- sf::st_read(dsn = dir, layer = layer)
  name
}

download_data <-  function(url, dir, layer, fld_name) {
  dest <- paste0(raw_prefix, ".zip")

  if (!file.exists(layer)) {
    download.file(url, dest)
    unzip(dest,
          exdir = raw_prefix)
    unlink(dest)
    assert_that(file.exists(layer))

    system(paste0('aws s3 sync ',
                  dir, " ",
                  s3_raw_prefix, fld_name))
  }
}

decompress_file <- function(file, exdir, .file_cache = FALSE) {

  if (.file_cache == TRUE) {
    print("decompression skipped")
  } else {

    # Run decompression
    decompression <-
      system2("unzip",
              args = c("-o", # include override flag
                       file,
                       "-d",
                       exdir),
              stdout = TRUE)

    # Test for success criteria
    # change the search depending on
    # your implementation
    if (grepl("Warning message", tail(decompression, 1))) {
      print(decompression)
    }
  }
}

# functions for 4_fpa_climate_summaries.R -------------------------------
# define an efficient extraction function to get mean values by polygon
fast_extract <- function(rasterfile, index_list) {
  r <- raster::brick(rasterfile)

  polygon_means <- lapply(index_list, FUN = function(x) {
    extracts <- raster::extract(r, x)
    colMeans(extracts, na.rm = TRUE)})

  list_of_dfs <- lapply(polygon_means, FUN = function(x) {
    df <- as.data.frame(x)
    tibble::rownames_to_column(df)})

  merged_dfs <- dplyr::bind_rows(list_of_dfs, .id = 'NA_L3NAME')
  wide_df <- tidyr::spread(merged_dfs, rowname, x)
  return(tibble::as_tibble(wide_df))
}


extract_one <- function(filename, shapefile_extractor, buff = FALSE) {

  # function to extract all climate time series based on shapefile input
  # this results in large list of all months/years within the raster climate data
  # each list is written out to a csv so this only needs to be run once.
  # inputs:
  # filename -> a list of all tif filenames with full path
  # shapefile_extractor -> the shapefile (point or polygon) to extract climate data

  out_name <- gsub('.tif', '.csv', filename)
  if (!file.exists(out_name)) {
    if (buff == FALSE) {
    res <- raster::extract(raster::stack(filename), shapefile_extractor,
                           na.rm = TRUE, fun = 'mean', df = TRUE)
    } else if (buff == TRUE) {
      res <- raster::extract(raster::raster(filename), shapefile_extractor,
                             na.rm = TRUE, fun = 'mean', df = TRUE, buffer = 1000)
      }
    write.csv(res, file = out_name)

  } else {
    res <- read.csv(out_name)
  }
  res
}

extract_anthro <- function(filename, shp_mask) {
  out_name <- gsub('.tif', '.csv', filename)
  if (!file.exists(out_name)) {
    r <- raster::raster(filename)
    raster::values(r)[raster::values(r) == -999] <- NA
    res <- raster::extract(r, shp_mask,
                           na.rm = TRUE, df = TRUE)
    write.csv(res, file = out_name)
  } else {
    res <- read.csv(out_name)
  }
  res
}

check_tifs <- function(j, i, ...) {
  # checks whether the statistic being evaluated has the variable data
  # if not then returns NULL which allows the larger loop below to skip the inputs

  tif <- tryCatch(list.files(file.path(climate_prefix, j),
                             pattern = i,
                             recursive = TRUE,
                             full.names = TRUE) %>%
                    Filter(function(x) grepl(".tif", x), .),
                  error = function(c) {
                    c$message <- paste0(c$message, " (in the", i, 'variable and ', j, 'statistic)')
                    warning(c)
                  }
  )

  # if the length of the tryCatch is greater than one then that indicates there
  # are data in the statistic/variable combination
  if (length(tif) > 1) {
    tif <- list.files(file.path(climate_prefix, j),
                      pattern = paste0(i, '_' , rep(1988:2015), "_", j, collapse = "|"),
                      recursive = TRUE,
                      full.names = TRUE) %>%
      Filter(function(x) grepl(".tif", x), .)
  } else {
    # if the tif length is 0 then that indicates there are no climate data for
    # that statistic/variable combination

    tif <- NULL
  }
}

# functions for c_prep_data.R ---------------------------------------------

mosaic_rasters <- function(files){

  # this function will take a list of raster data with full path names and:
  #  1. read in all rasters iteratively
  #  2. create a raster list of all created rasters
  #  3. mosaic all rasters, using mean if tiles overlap.
  #
  # the only input needed is the list of raster filenames

  #Internal function to make a list of raster objects from list of files.
  list_rasters <- function(list_names) {

    raster_list <- list() # initialise the list of rasters

    for (i in 1:(length(list_names))){

      rst_name <- list_names[i] # list_names contains all the names of the images in .grd format
      raster_file <- raster::raster(rst_name)

    }
    raster_list <- append(raster_list, raster_file) # update raster_list at each iteration
  }

  #convert every raster path to a raster object and create list of the results
  raster_list <- sapply(files, FUN = list_rasters)

  # make all raster names null
  names(raster_list) <- NULL

  # take the mean of overlapping raster images
  raster_list$fun <- mean

  # mosaic all rasters in list
  mos <- do.call(raster::mosaic, raster_list)

  #set crs of output
  crs(mos) <- crs(x = raster(files[1]))
  return(mos)
}

# functions for c_data_prep_transportation_density.R ---------------------------------------------

get_density <- function(x, grids, lines) {
  
  require(tidyverse)
  require(lubridate)
  require(sf)
  
  sub_grids <- grids %>%
    dplyr::filter(fishid4k == x)
  
  single_lines_fishid <- lines %>%
    sf::st_intersection(., sub_grids) %>%
    dplyr::select(fishid4k) %>%
    dplyr::mutate(length_line = st_length(.),
                  length_line = ifelse(is.na(length_line), 0, length_line))
  
  sub_grids <- sub_grids %>%
    sf::st_join(., single_lines_fishid, join = st_intersects) %>%
    dplyr::mutate(fishid4k = fishid4k.x) %>%
    dplyr::group_by(fishid4k) %>%
    dplyr::summarize(length_line = sum(length_line)) %>%
    dplyr::mutate(pixel_area = as.numeric(st_area(geom)),
                  density = length_line/pixel_area,
                  length_line = ifelse(is.na(length_line), 0, length_line),
                  density = ifelse(is.na(density), 0, density)) %>%
    dplyr::select(fishid4k, length_line, density, pixel_area) %>%
    st_cast('MULTIPOLYGON')
  return(sub_grids)
}


# functions for c_data_prep_human_density.R ---------------------------------------------

get_lags <- function(extract_to, extract_from, start_date, time_lag) {

  # capture the variable name and statistic to be incorporated in the output column name
  if (exists('extract_from$statistic')) {
    variable <- paste0(extract_from$variable[1], '_', extract_from$statistic[1])
  } else {
    variable <- extract_from$variable[1]
  }

  # internal function to create a lagged date
  lag_date <- function(start_date, time_lag) {
    require(magrittr)
    require(tidyverse)
    require(lubridate)

    # breakup the start date into its components
    y <- year(start_date)
    m <- month(start_date)
    d <- day(start_date)

    # calculate the new lagged year
    y <- y + (m - time_lag - 1) %/% 12

    # calculate the new lagged month
    m <- ifelse(((m - time_lag) %% 12) == 0, 12, (m - time_lag) %% 12)

    # stitch the new lagged date together
    as.Date(paste0(y, "-", m, "-", d))
  }

  # remove the sf data - increases efficiency
  if (exists('extract_to$geom')) {
    extract_to <- extract_to %>%
      as.data.frame() %>%
      select(-geom)
  }

  # pair down to the extract_from to allow for easier left_join
  extract_from <- extract_from %>%
    select('FPA_ID', 'year_month_day', 'value')

  for (j in 0:time_lag) {
    require(magrittr)
    require(tidyverse)

    # create a lagged data year column that can be joined and extracted upon
    extract_to[, paste0(variable, '_lag_', j)]  <- extract_to %>%
      dplyr::mutate(ymd_lagged = lag_date(start_date = start_date, time_lag = j)) %>%
      left_join(., extract_from, by = c('FPA_ID', 'ymd_lagged' = 'year_month_day')) %>%
      dplyr::select(value)
  }
  return(extract_to)
}

impute_density <- function(df) {
  # Interpolate for each month and year from 1992 - 2015
  # using a simple linear sequence given decadal values

  require(tidyverse)
  require(magrittr)

  year_seq <- min(df$year):max(df$year)
  predict_seq <- seq(min(df$year),
                     max(df$year),
                     length.out = (length(year_seq) - 1) * 12)
  preds <- spline(x = as.numeric(df$year),
                  y = as.numeric(df$value),
                  xout = predict_seq)
  res <- as_tibble(preds) %>%
    rename(t = x, value = y) %>%
    mutate(
      year = floor(t),
      month = rep(1:12, times = length(year_seq) - 1),
      day = '01',
      year_month_day = as.Date(paste(year, month, day, sep =
                                       '-'))
    ) %>%
    filter(year < 2016)
  res$FPA_ID <- unique(df$FPA_ID)
  res$variable <- unique(df$variable)
  res
}

impute_in_parallel <- function(data, x) {

  sub_data <- subset(data, data$FPA_ID == x)

  extraction_df <- sub_data %>%
    dplyr::select(-year_month_day) %>%
    gather(variable, value, -FPA_ID, -PBG00, -STATE) %>%
    mutate(value = ifelse(value == -999, is.na(value), value)) %>%
    filter(!is.na(value)) %>%
    mutate(
      year = case_when(
        .$variable == 'HDEN90' ~ 1990,
        .$variable == 'HDEN00' ~ 2000,
        .$variable == 'HDEN10' ~ 2010,
        .$variable == 'HDEN20' ~ 2020
      )
    ) %>%
    do(impute_density(.))

  # reduce the size of the dataframe to be joined during the get_climate_lags
  sub_df <- sub_data %>%
    dplyr::select(FPA_ID, PBG00, year_month_day) %>%
    mutate(year_month_day = as.Date(year_month_day, origin="1970-01-01"))

  fpa_out <-
    get_lags(
      extract_to = sub_df,
      extract_from = extraction_df,
      start_date = sub_df$year_month_day,
      time_lag = 0
    ) %>%
    dplyr::select(-year_month_day)

  fpa_out
}

impute_in_parallel_ciesin <- function(data, ids) {

  sub_data <- subset(data, data$FPA_ID == ids)

    extraction_df <- sub_data %>%
    dplyr::select(-year_month_day) %>%
    do(impute_density(.))

  # reduce the size of the dataframe to be joined during the get_climate_lags
  sub_df <- sub_data %>%
    dplyr::select(FPA_ID, year_month_day) %>%
    mutate(year_month_day = as.Date(year_month_day, origin="1970-01-01"))

  fpa_out <-
    get_lags(
      extract_to = sub_df,
      extract_from = extraction_df,
      start_date = sub_df$year_month_day,
      time_lag = 0
    ) %>%
    dplyr::select(-year_month_day) %>%
    distinct(FPA_ID, .keep_all = TRUE)

  fpa_out
}
mode <- function(x) {
  ux <- unique(x)
  ux=ux[!is.na(ux)]
  ux[which.max(tabulate(match(x, ux)))]
}
# Functions for `d_rasterize_anthro.R` ------------------------------------

shp_rst <- function(y, x, lvl, j){
  # This function splits shapefile based on the number of cores for parallel rasterization
  # y = input shapefile
  # x = number of splits to iterate on in parallel
  # lvl = the shapefile attribute to rasterize
  # j = the larger underlying raster (4k)
  # k = the smaller underlying raster (200m)
  features <- 1:nrow(y[,])
  parts <- split(features, cut(features, ncor))
  require(snowfall)
  require(raster)
  require(sf)
  require(tidyverse)

  outrst <- rasterize(as(y[parts[[x]],], "Spatial"), j, lvl)
}

combine_rst <- function(y){
  # A function to recombine the split data from `shp_rst`, calculate the distance and reproject to equal area
  # y = shp_rst output split rasters

  do.call(merge, y) %>%
    distance(.)  %>%
    aggregate(fact = 20, fun = mean) %>%
    projectRaster(., crs = p4string_ea, res = 4000) %>%
    crop(as(usa_shp, "Spatial")) %>%
    mask(as(usa_shp, "Spatial"))
}

# Functions for `e_model_creation.R` ------------------------------------------------------------------------------

sfc_as_cols <- function(x, names = c("x","y")) {
  stopifnot(inherits(x,"sf") && inherits(sf::st_geometry(x),"sfc_POINT"))
  ret <- sf::st_coordinates(x)
  ret <- tibble::as_tibble(ret)
  stopifnot(length(names) == ncol(ret))
  x <- x[ , !names(x) %in% names]
  ret <- setNames(ret,names)
  dplyr::bind_cols(x,ret)
}
