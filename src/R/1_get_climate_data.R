
source("src/R/a_make_dirs.R")

vpd_nc <- file.path(climate_prefix, 'vpd_19792016.nc')
if (!file.exists(vpd_nc)) {
  loc <- "http://nimbus.cos.uidaho.edu/abatz/DATA/vpd19792016.nc"
  dest <- paste0(climate_prefix, "/vpd_19792016.nc")
  download.file(loc, dest)
  assert_that(file.exists(vpd_nc))
}

pdsi_nc <- file.path(climate_prefix, 'pdsi_19792016.nc')
if (!file.exists(pdsi_nc)) {
  loc <- "http://nimbus.cos.uidaho.edu/abatz/DATA/pdsi19792016.nc"
  dest <- paste0(climate_prefix, "/pdsi_19792016.nc")
  download.file(loc, dest)
  assert_that(file.exists(pdsi_nc))
}

aet_nc <- file.path(climate_prefix, 'aet_19792016.nc')
if (!file.exists(aet_nc)) {
  loc <- "http://nimbus.cos.uidaho.edu/abatz/DATA/aet19792016.nc"
  dest <- paste0(climate_prefix, "/aet_19792016.nc")
  download.file(loc, dest)
  assert_that(file.exists(aet_nc))
}

def_nc <- file.path(climate_prefix, 'def_19792016.nc')
if (!file.exists(def_nc)) {
  loc <- "http://nimbus.cos.uidaho.edu/abatz/DATA/def19792016.nc"
  dest <- paste0(climate_prefix, "/def_19792016.nc")
  download.file(loc, dest)
  assert_that(file.exists(def_nc))
}
