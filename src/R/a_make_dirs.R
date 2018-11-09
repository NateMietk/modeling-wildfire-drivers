
# load all ibraries
x <- c("raster", "ncdf4", "tidyverse", "sf", "rasterVis", "gridExtra", "data.table", "assertthat", "rvest", 'parallel', 'doParallel', 'lwgeom','pbapply',
       'parallel', 'foreach', "httr", "purrr", "rgdal", "maptools", "foreign", "purrr", "zoo", "lubridate", "magrittr", "snowfall", 'spatstat', 'velox')
lapply(x, library, character.only = TRUE, verbose = FALSE)

# load all functions
source('src/functions/helper_functions.R')

# key projections
p4string_ea <- "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"   #http://spatialreference.org/ref/sr-org/6903/

# define the amount of cores st_par runs on
ncor <- parallel::detectCores()

# create main directories
prefix <- ("data")
summary_dir <- file.path(prefix, "extractions")
processed_dir <- file.path(prefix, 'processed')

# create main raw folder and all subfolders to hold raw/unprocessed data
raw_prefix <- file.path(prefix, "raw")
us_prefix <- file.path(raw_prefix, "cb_2016_us_state_20m")
ecoregion_prefix <- file.path(raw_prefix, "us_eco_l3")
ecoregionl4_prefix <- file.path(raw_prefix, "us_eco_l4")
fpa_prefix <- file.path(raw_prefix, "fpa-fod")
roads_prefix <- file.path(raw_prefix, "tlgdb_2015_a_us_roads")
rails_prefix <- file.path(raw_prefix, "tlgdb_2015_a_us_rails")
landcover_dir <- file.path(raw_prefix, 'landcover')

pd_prefix <- file.path(raw_prefix, "county_pop")
iclus_prefix <- file.path(raw_prefix, 'housing_den')
elev_prefix <- file.path(raw_prefix, 'metdata_elevationdata')
tl_prefix <- file.path(raw_prefix, 'Electric_Power_Transmission_Lines')
climate_prefix <- file.path(prefix, "climate")
bounds_dir <- file.path(prefix, 'bounds')

# create processed directories
terrain_dir <- file.path(processed_dir, 'terrain')
anthro_proc_dir <- file.path(processed_dir, 'anthro')
transportation_dir <- file.path(processed_dir, 'transportation')
transportation_density_dir <- file.path(transportation_dir, 'density')
transportation_processed_dir <- file.path(transportation_dir, 'processed')
transportation_dist_dir <- file.path(transportation_dir, 'distance')
proc_gridded_census <- file.path(anthro_proc_dir, 'gridded_census')
per_state <- file.path(transportation_density_dir, "per_state")
proc_bounds_dir <- file.path(processed_dir, 'bounds')

# create direcotires to hold climate summary outputs
summaries_dir <- file.path(summary_dir, "climate_extractions")
summary_mean <- file.path(summaries_dir, "mean")
summary_95th <- file.path(summaries_dir, "95th")
summary_numdays95th <- file.path(summaries_dir, "numdays95th")
terrain_extract <- file.path(summary_dir, "terrain_extractions")
anthro_extract <- file.path(summary_dir, "anthro_extractions")
anthro_state_extract <- file.path(anthro_extract, "state")

anthro_dir <- file.path(prefix, "anthro")
ztrax_dir <- file.path(anthro_dir, "ztrax")
raw_ztrax_dir <- file.path(ztrax_dir, "raw_built_up_gpkg")
stacked_ztrax_rst_dir <- file.path(ztrax_dir, "stacked_ztrax_rst")

# for pushing and pulling to s3 using the system function
s3_base <- 's3://earthlab-modeling-human-ignitions/'
s3_anc_prefix <- 's3://earthlab-modeling-human-ignitions/ancillary/'
s3_proc_prefix <- 's3://earthlab-modeling-human-ignitions/processed/'
s3_raw_prefix <- 's3://earthlab-modeling-human-ignitions/raw/'
s3_proc_extractions <- 's3://earthlab-modeling-human-ignitions/extractions/'
s3_proc_climate <- 's3://earthlab-modeling-human-ignitions/climate/'
s3_proc_bounds <- 's3://earthlab-modeling-human-ignitions/bounds/'
s3_proc_anthro <- 's3://earthlab-modeling-human-ignitions/anthro/'

# Check if directory exists for all variable aggregate outputs, if not then create
var_dir <- list(prefix, raw_prefix, us_prefix, ecoregion_prefix, roads_prefix, summary_dir,
                fpa_prefix, rails_prefix, pd_prefix, iclus_prefix, climate_prefix, elev_prefix,
                tl_prefix, bounds_dir, anthro_dir, processed_dir, summaries_dir, summary_mean, transportation_dist_dir,
                summary_95th, summary_numdays95th, terrain_dir, transportation_dir, anthro_proc_dir,
                transportation_density_dir, transportation_processed_dir, anthro_dir, anthro_state_extract,
                terrain_extract, anthro_extract, per_state,ecoregionl4_prefix, ztrax_dir, raw_ztrax_dir,
                stacked_ztrax_rst_dir, proc_gridded_census, landcover_dir, proc_bounds_dir)

lapply(var_dir, function(x) if(!dir.exists(x)) dir.create(x, showWarnings = FALSE))

