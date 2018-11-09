
get_percentile <- function (vars, prob = '95') {
  require(raster)
  require(lubridate)
  require(tidyverse)

  for (j in unique(vars)) {

    monthly_mean <- list.files(file.path('data', 'climate', 'mean'),
                               pattern = j,
                               recursive = TRUE,
                               full.names = TRUE)

    raster_mean <- raster::stack(monthly_mean)

    start_date <- as.Date(paste("1979", "01", "01", sep = "-"))
    end_date <- as.Date(paste("2016", "12", "31", sep = "-"))
    date_seq <- seq(start_date, end_date, by = "1 month")

    quantiles_fun <- quantile(raster_mean, probs = as.double(paste0('0.', prob)), na.rm = TRUE)

    quantile_mask <- calc(raster_mean, fun = function(x){x > quantiles_fun})

    quantile_means <- overlay(raster_mean, quantile_mask, fun = function(x, y) ifelse(y == 1, x, y))

    names(quantile_means) <- paste(year(date_seq), month(date_seq),
                                   day(date_seq), sep = "-")

    mean_pct <- stack()
    for (i in 1979:2016) {

      mean_pct <- subset(quantile_means,  grep(i, names(quantile_means))) # subset based on year

      start_date <- as.Date(paste(i, "01", "01", sep = "-"))
      end_date <- as.Date(paste(i, "12", "31", sep = "-"))
      date_seq <- seq(start_date, end_date, by = "1 month")

      names(mean_pct) <- paste(j, year(date_seq), month(date_seq), sep = "_")

      if(!file.exists(file.path('data', 'climate', paste0(prob, 'th'), paste0(j, '_', i, '_', prob, 'th',".tif")) )){
        writeRaster(mean_pct, filename = file.path('data', 'climate', paste0(prob, 'th'), paste0(j, "_", i, '_', prob, 'th',".tif")),
                    format = "GTiff")
      }
    }
  }
}

vars <- c('aet', 'def', 'ffwi', 'fm100', 'pdsi', 'pr', 'tmmx', 'vpd', 'vs')

get_percentile(vars)

sfInit(parallel = TRUE, cpus = parallel::detectCores())

sfLapply(as.list(vars), fun = get_percentile)

sfStop()
