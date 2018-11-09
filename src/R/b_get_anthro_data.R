# Download the FPA-FOD data

fpa_gdb <- file.path(fpa_prefix, "Data", "FPA_FOD_20170508.gdb")
if (!file.exists(fpa_gdb)) {
  pg <- read_html("https://www.fs.usda.gov/rds/archive/Product/RDS-2013-0009.4/")
  fils <- html_nodes(pg, xpath=".//dd[@class='product']//li/a[contains(., 'zip') and contains(., 'GDB')]")
  dest <- paste0(fpa_prefix, ".zip")
  walk2(html_attr(fils, 'href'),  html_text(fils),
        ~GET(sprintf("https:%s", .x), write_disk(dest), progress()))
  unzip(dest, exdir = fpa_prefix)
  unlink(dest)
  assert_that(file.exists(fpa_gdb))
  system(paste0('aws s3 sync ', #command
                fpa_gdb, "/ ", #source file
                s3_raw_prefix, "fpa-fod/",  "FPA_FOD_20170508.gdb/")) #destination
}

# Download ecoregion level 3
ecol3_shp <- file.path(ecoregion_prefix, 'us_eco_l3.shp')
download_data("ftp://newftp.epa.gov/EPADataCommons/ORD/Ecoregions/us/us_eco_l3.zip",
              ecoregion_prefix,
              ecol3_shp,
              'us_eco_l3')

# Download ecoregion level 4
ecol4_shp <- file.path(ecoregionl4_prefix, 'us_eco_l4_no_st.shp')
download_data("ftp://newftp.epa.gov/EPADataCommons/ORD/Ecoregions/us/us_eco_l4.zip",
              ecoregionl4_prefix,
              ecol4_shp,
              'us_eco_l4_no_st')

# Download the roads
roads_shp <- file.path(roads_prefix, 'tlgdb_2015_a_us_roads.gdb')
download_data("ftp://ftp2.census.gov/geo/tiger/TGRGDB15/tlgdb_2015_a_us_roads.gdb.zip",
roads_prefix,
roads_shp,
'tlgdb_2015_a_us_roads')

# Download the railrods
rails_shp <- file.path(rails_prefix, 'tlgdb_2015_a_us_rails.gdb')
download_data("ftp://ftp2.census.gov/geo/tiger/TGRGDB15/tlgdb_2015_a_us_rails.gdb.zip",
rails_prefix,
rails_shp,
'tlgdb_2015_a_us_rails')

# Download the tramission lines
tl_shp <- file.path(tl_prefix, 'Electric_Power_Transmission_Lines.shp')
download_data("https://hifld-dhs-gii.opendata.arcgis.com/datasets/75af06441c994aaf8e36208b7cd44014_0.zip",
tl_prefix,
tl_shp,
'Electric_Power_Transmission_Lines')

# Download population density by county from 2010-2100
pd_shp <- file.path(pd_prefix, 'cofips_upp.shp')
download_data("https://edg.epa.gov/data/Public/ORD/NCEA/county_pop.zip",
pd_prefix,
pd_shp,
'county_pop')

# Download housing density baseline scenario
iclus_nc <- file.path(iclus_prefix, 'hd_iclus_bc.nc')
download_data("https://cida.usgs.gov/thredds/fileServer/ICLUS/files/housing_density/hd_iclus_bc.nc",
iclus_prefix,
iclus_nc,
'housing_den')

# Download NLCD 1992
nlcd92_img <- file.path(nlcd92_prefix, "nlcd_1992_30meter_whole.img")
if (!file.exists(nlcd92_img)){
loc <- "http://www.landfire.gov/bulk/downloadfile.php?TYPE=nlcd92&FNAME=nlcd_1992_30meter_whole.zip"
dest <- paste0(raw_prefix, "/nlcd92.zip")
download.file(loc, dest)
system(paste0("unzip ",
              dest,
              " -d ",
              nlcd92_prefix))

unlink(dest)
assert_that(file.exists(nlcd92_img))
system(paste0("aws s3 sync ",
              nlcd92_prefix, "/ ",
              s3_raw_prefix, "nlcd_1992/"))
}

# Download NLCD 2001
nlcd01_img <- file.path(nlcd01_prefix, "nlcd_2001_landcover_2011_edition_2014_10_10.img")
if (!file.exists(nlcd01_img)){
  loc <- "http://www.landfire.gov/bulk/downloadfile.php?TYPE=nlcd2001v2&FNAME=nlcd_2001_landcover_2011_edition_2014_10_10.zip"
  dest <- paste0(raw_prefix, "/nlcd2001.zip")
  download.file(loc, dest)
  system(paste0("unzip ",
                dest,
                "-d ",
                raw_prefix))
  unlink(dest)
  assert_that(file.exists(nlcd01_img))
  system(paste0("aws s3 sync ",
                nlcd01_prefix, "/ ",
                s3_raw_prefix, "nlcd_2001_landcover_2011_edition_2014_10_10/"))

}

# Download NLCD 2006
nlcd06_img <- file.path(nlcd06_prefix, "nlcd_2006_landcover_2011_edition_2014_10_10.img")
if (!file.exists(nlcd06_img)){
  loc <- "http://www.landfire.gov/bulk/downloadfile.php?TYPE=nlcd2006&FNAME=nlcd_2006_landcover_2011_edition_2014_10_10.zip"
  dest <- paste0(raw_prefix, "/nlcd2006.zip")
  download.file(loc, dest)
  system(paste0("unzip ",
                dest,
                " -d ",
                raw_prefix))

  unlink(dest)
  assert_that(file.exists(nlcd06_img))
  system(paste0("aws s3 sync ",
                nlcd06_prefix, "/ ",
                s3_raw_prefix, "nlcd_2006_landcover_2011_edition_2014_10_10/"))
}

# Download the NLCD 2011

nlcd_img <- file.path(nlcd_prefix, 'nlcd_2011_landcover_2011_edition_2014_10_10.img')
if (!file.exists(nlcd_img)) {
  loc <- "http://www.landfire.gov/bulk/downloadfile.php?TYPE=nlcd2011&FNAME=nlcd_2011_landcover_2011_edition_2014_10_10.zip"
  dest <- paste0(raw_prefix, ".zip")
  download.file(loc, dest)
  print("extracting")
  system(paste0("unzip ",
                dest,
                "-d ",
                raw_prefix))
  unlink(dest)
  assert_that(file.exists(nlcd_img))
  system(paste0("aws s3 sync ",
                nlcd_prefix, "/ ",
                s3_raw_prefix, "nlcd_2011_landcover_2011_edition_2014_10_10/"))
}

# Download NLCD percent developed imperviousness 2001
nlcd_pdi_01_img <- file.path(nlcd_pdi_01_prefix, "nlcd_2001_impervious_2011_edition_2014_10_10.img")
if (!file.exists(nlcd_pdi_01_img)){
  loc <- "http://www.landfire.gov/bulk/downloadfile.php?TYPE=nlcd2001v2&FNAME=nlcd_2001_impervious_2011_edition_2014_10_10.zip"
  dest <- paste0(raw_prefix, "/nlcdpdi01.zip")
  download.file(loc, dest)
  system(paste0("unzip ",
                dest,
                " -d ",
                raw_prefix))

  unlink(dest)
  assert_that(file.exists(nlcd_pdi_01_img))
  system(paste0("aws s3 sync ",
                nlcd_pdi_01_prefix, "/ ",
                s3_raw_prefix, "nlcd_2001_impervious_2011_edition_2014_10_10/"))
}

# Download NLCD percent developed imperviousness 2006
nlcd_pdi_06_img <- file.path(nlcd_pdi_06_prefix, "nlcd_2006_impervious_2011_edition_2014_10_10.img")
if (!file.exists(nlcd_pdi_06_img)){
  loc <- "http://www.landfire.gov/bulk/downloadfile.php?TYPE=nlcd2001v2&FNAME=nlcd_2006_impervious_2011_edition_2014_10_10.zip"
  dest <- paste0(raw_prefix, "/nlcdpdi06.zip")
  download.file(loc, dest)
  system(paste0("unzip ",
                dest,
                " -d ",
                raw_prefix))

  unlink(dest)
  assert_that(file.exists(nlcd_pdi_06_img))
  system(paste0("aws s3 sync ",
                nlcd_pdi_06_prefix, "/ ",
                s3_raw_prefix, "nlcd_2006_impervious_2011_edition_2014_10_10/"))
}

# Download NLCD percent developed imperviousness 2011
nlcd_pdi_11_img <- file.path(nlcd_pdi_11_prefix, "nlcd_2011_impervious_2011_edition_2014_10_10.img")
if (!file.exists(nlcd_pdi_11_img)){
  loc <- "http://www.landfire.gov/bulk/downloadfile.php?TYPE=nlcd2001v2&FNAME=nlcd_2011_impervious_2011_edition_2014_10_10.zip"
  dest <- paste0(raw_prefix, "/nlcdpdi11.zip")
  download.file(loc, dest)
  system(paste0("unzip ",
                dest,
                " -d ",
                raw_prefix))

  unlink(dest)
  assert_that(file.exists(nlcd_pdi_11_img))
  system(paste0("aws s3 sync ",
                nlcd_pdi_11_prefix, "/ ",
                s3_raw_prefix, "nlcd_2011_impervious_2011_edition_2014_10_10/"))
}
