#!/bin/bash

# Use the get_data.R file if these are not available on S3
# Rscript --vanilla ../HumanIgnProb/src/R/get_data.R

# get ancillary data from S3
aws s3 sync s3://earthlab-natem/data/raw ../data/raw

# execute R script to rasterize shapefiles
Rscript --vanilla ../HumanIgnProb/src/R/prepare_data.R

# get ancillary data from S3
aws s3 sync ../data/processed s3://earthlab-natem/data/processed
