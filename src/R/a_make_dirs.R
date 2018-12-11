
# load all ibraries
packages <- c("raster", "ncdf4", "tidyverse", "sf", "rasterVis", "gridExtra", "data.table", "assertthat", "rvest", 'parallel', 'doParallel', 'lwgeom','pbapply',
              'parallel', 'foreach', "httr", "purrr", "rgdal", "maptools", "foreign", "purrr", "zoo", "lubridate", "magrittr", "snowfall", 'spatstat', 'velox', 'caret', 'ranger')
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  # automatically installs packages if not found
  install.packages(setdiff(packages, rownames(installed.packages())))  
  # loads the library once installed
  lapply(packages, library, character.only = TRUE, quietly = TRUE) 
} else {
  # if package is already install this loads it
  lapply(packages, library, character.only = TRUE, quietly = TRUE) 
}

# Load all external custom functions
file_sources <- list.files(file.path('src', 'functions'), pattern="*.R", 
                           full.names=TRUE, ignore.case=TRUE)
invisible(sapply(file_sources, source, .GlobalEnv))

# key projections
p4string_ea <- "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"   #http://spatialreference.org/ref/sr-org/6903/

# define the amount of cores st_par runs on
ncor <- parallel::detectCores()

# Model output
model_dir <- file.path('model_outputs')

# create main directories
data_dir <- ("data")
extraction_dir <- file.path(data_dir, "extractions")
processed_dir <- file.path(data_dir, 'processed')
climate_prefix <- file.path(data_dir, "climate")
bounds_dir <- file.path(data_dir, 'bounds')
anthro_dir <- file.path(data_dir, "anthro")

var_dir <- list(model_dir, data_dir, extraction_dir, processed_dir, climate_prefix, bounds_dir, anthro_dir)
lapply(var_dir, function(x) if(!dir.exists(x)) dir.create(x, showWarnings = FALSE))

# create main raw folder and all subfolders to hold raw/unprocessed data
raw_dir <- file.path(data_dir, "raw")
raw_us <- file.path(raw_dir, "cb_2016_us_state_20m")
raw_ecoregionl3 <- file.path(raw_dir, "us_eco_l3")
raw_ecoregionl4 <- file.path(raw_dir, "us_eco_l4")
raw_fpa <- file.path(raw_dir, "fpa-fod")
raw_roads <- file.path(raw_dir, "tlgdb_2015_a_us_roads")
raw_rails <- file.path(raw_dir, "tlgdb_2015_a_us_rails")
raw_landcover <- file.path(raw_dir, 'landcover')
raw_pd <- file.path(raw_dir, "county_pop")
raw_iclus <- file.path(raw_dir, 'housing_den')
raw_elev <- file.path(raw_dir, 'metdata_elevationdata')
raw_tl <- file.path(raw_dir, 'Electric_Power_Transmission_Lines')
var_dir <- list(raw_dir, raw_us, raw_ecoregionl3, raw_ecoregionl4, raw_fpa, raw_roads, raw_rails, 
                raw_landcover, raw_pd, raw_iclus, raw_elev, raw_tl)
lapply(var_dir, function(x) if(!dir.exists(x)) dir.create(x, showWarnings = FALSE))

# create processed directories
proc_terrain_dir <- file.path(processed_dir, 'terrain')
proc_anthro_dir <- file.path(processed_dir, 'anthro')
proc_landcover_dir <- file.path(proc_anthro_dir, 'landcover')

proc_transportation_dir <- file.path(processed_dir, 'transportation')
proc_tertiary_states <- file.path(proc_transportation_dir, "tertiary_states")
proc_gridded_census <- file.path(proc_anthro_dir, 'gridded_census')
proc_bounds_dir <- file.path(processed_dir, 'bounds')
proc_fire_dir <- file.path(processed_dir, 'fire')

var_dir <- list(proc_terrain_dir, proc_anthro_dir, proc_landcover_dir,proc_transportation_dir, proc_tertiary_states, proc_gridded_census, proc_bounds_dir, proc_fire_dir)
lapply(var_dir, function(x) if(!dir.exists(x)) dir.create(x, showWarnings = FALSE))

# create directoires to hold climate extraction outputs
extraction_climate_dir <- file.path(extraction_dir, "climate_extractions")
extraction_mean_climate_dir <- file.path(extraction_climate_dir, "mean")
extraction_95th_climate_dir <- file.path(extraction_climate_dir, "95th")
extraction_numdays95th_climate_dir <- file.path(extraction_climate_dir, "numdays95th")
extraction_terrain <- file.path(extraction_dir, "terrain_extractions")
extraction_anthro <- file.path(extraction_dir, "anthro_extractions")
var_dir <- list(extraction_climate_dir, extraction_mean_climate_dir, extraction_95th_climate_dir, extraction_numdays95th_climate_dir, 
                extraction_terrain, extraction_anthro)
lapply(var_dir, function(x) if(!dir.exists(x)) dir.create(x, showWarnings = FALSE))

# Create other directories
ztrax_dir <- file.path(anthro_dir, "ztrax")
raw_ztrax_dir <- file.path(ztrax_dir, "raw_built_up_gpkg")
stacked_ztrax_rst_dir <- file.path(ztrax_dir, "stacked_ztrax_rst")
count_ztrax_rst_dir <- file.path(ztrax_dir, "ztrax_raw_count_1980_2015_1k")
wui_dir <- file.path(anthro_dir, "wui")
landcover_dir <- file.path(anthro_dir, "landcover")

var_dir <- list(ztrax_dir, raw_ztrax_dir, stacked_ztrax_rst_dir, wui_dir, landcover_dir)
lapply(var_dir, function(x) if(!dir.exists(x)) dir.create(x, showWarnings = FALSE))

# for pushing and pulling to s3 using the system function
s3_base <- 's3://modeling-human-ignition-random-forests/'
s3_anc_prefix <- 's3://modeling-human-ignition-random-forests/ancillary/'
s3_proc_prefix <- 's3://modeling-human-ignition-random-forests/processed/'
s3_raw_prefix <- 's3://modeling-human-ignition-random-forests/raw/'
s3_proc_extractions <- 's3://modeling-human-ignition-random-forests/extractions/'
s3_proc_climate <- 's3://modeling-human-ignition-random-forests/climate/'
s3_proc_bounds <- 's3://modeling-human-ignition-random-forests/bounds/'
s3_proc_anthro <- 's3://modeling-human-ignition-random-forests/anthro/'