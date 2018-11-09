#!/bin/bash

# get ancillary data from S3
aws s3 sync s3://earthlab-natem/data/raw/cb_2016_us_state_20m ../data/raw/cb_2016_us_state_20m 
aws s3 sync s3://earthlab-natem/data/raw/us_eco_l3 ../data/raw/us_eco_l3 
aws s3 sync s3://earthlab-natem/data/raw/Electric_Power_Transmission_Lines ../data/raw/Electric_Power_Transmission_Lines 
aws s3 sync s3://earthlab-natem/data/raw/fpa-fod ../data/raw/fpa-fod
aws s3 sync s3://earthlab-natem/data/raw/metdata_elevationdata ../data/raw/metdata_elevationdata
aws s3 sync s3://earthlab-natem/data/raw/tlgdb_2015_a_us_roads ../data/raw/tlgdb_2015_a_us_roads 
aws s3 sync s3://earthlab-natem/data/raw/tlgdb_2015_a_us_rails ../data/raw/tlgdb_2015_a_us_rails
