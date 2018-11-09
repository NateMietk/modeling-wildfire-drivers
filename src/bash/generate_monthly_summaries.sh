#!/bin/bash

# download the data from remote server
source src/bash/metdata_wget.sh

# execute R script to produce monthly GeoTIFFs
Rscript --vanilla ../modeling-human-ignition/src/R/aggregate_climate_data.R

# push monthly GeoTIFFS to AWS S3
aws s3 sync ../data/processed/ s3://earthlab-natem/data/climate/historical/
