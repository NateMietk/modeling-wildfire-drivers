# Download the FPA-FOD data

fpa_gdb <- file.path(raw_fpa, "Data", "FPA_FOD_20170508.gdb")
if (!file.exists(fpa_gdb)) {
  pg <- read_html("https://www.fs.usda.gov/rds/archive/Product/RDS-2013-0009.4/")
  fils <- html_nodes(pg, xpath=".//dd[@class='product']//li/a[contains(., 'zip') and contains(., 'GDB')]")
  dest <- paste0(raw_fpa, ".zip")
  walk2(html_attr(fils, 'href'),  html_text(fils),
        ~GET(sprintf("https:%s", .x), write_disk(dest), progress()))
  unzip(dest, exdir = raw_fpa)
  unlink(dest)
  assert_that(file.exists(fpa_gdb))
  system(paste0('aws s3 sync ', #command
                fpa_gdb, "/ ", #source file
                s3_raw, "fpa-fod/",  "FPA_FOD_20170508.gdb/")) #destination
}

# Download ecoregion level 3
ecol3_shp <- file.path(raw_ecoregionl3, 'us_eco_l3.shp')
download_data("ftp://newftp.epa.gov/EPADataCommons/ORD/Ecoregions/us/us_eco_l3.zip",
              raw_ecoregionl3,
              ecol3_shp,
              'us_eco_l3')

# Download ecoregion level 4
ecol4_shp <- file.path(raw_ecoregionl4, 'us_eco_l4_no_st.shp')
download_data("ftp://newftp.epa.gov/EPADataCommons/ORD/Ecoregions/us/us_eco_l4.zip",
              raw_ecoregionl4,
              ecol4_shp,
              'us_eco_l4_no_st')

# Download the roads
roads_shp <- file.path(raw_roads, 'tlgdb_2015_a_us_roads.gdb')
download_data("ftp://ftp2.census.gov/geo/tiger/TGRGDB15/tlgdb_2015_a_us_roads.gdb.zip",
              raw_roads,
roads_shp,
'tlgdb_2015_a_us_roads')

# Download the railrods
rails_shp <- file.path(raw_rails, 'tlgdb_2015_a_us_rails.gdb')
download_data("ftp://ftp2.census.gov/geo/tiger/TGRGDB15/tlgdb_2015_a_us_rails.gdb.zip",
raw_rails,
rails_shp,
'tlgdb_2015_a_us_rails')

# Download the tramission lines
tl_shp <- file.path(raw_tl, 'Electric_Power_Transmission_Lines.shp')
download_data("https://hifld-dhs-gii.opendata.arcgis.com/datasets/75af06441c994aaf8e36208b7cd44014_0.zip",
              raw_tl,
              tl_shp,
              'Electric_Power_Transmission_Lines')

# Download population density by county from 2010-2100
pd_shp <- file.path(raw_pd, 'cofips_upp.shp')
download_data("https://edg.epa.gov/data/Public/ORD/NCEA/county_pop.zip",
raw_pd,
pd_shp,
'county_pop')

# Download housing density baseline scenario
iclus_nc <- file.path(raw_iclus, 'hd_iclus_bc.nc')
download_data("https://cida.usgs.gov/thredds/fileServer/ICLUS/files/housing_density/hd_iclus_bc.nc",
raw_iclus,
iclus_nc,
'housing_den')

# Download NLCD 1992
nlcd92_img <- file.path(nlcd92, "nlcd_1992_30meter_whole.img")
if (!file.exists(nlcd92_img)){
loc <- "http://www.landfire.gov/bulk/downloadfile.php?TYPE=nlcd92&FNAME=nlcd_1992_30meter_whole.zip"
dest <- paste0(raw_dir, "/nlcd92.zip")
download.file(loc, dest)
system(paste0("unzip ",
              dest,
              " -d ",
              nlcd92))

unlink(dest)
assert_that(file.exists(nlcd92_img))
system(paste0("aws s3 sync ",
              nlcd92, "/ ",
              s3_raw, "nlcd_1992/"))
}

# Download NLCD 2001
nlcd01_img <- file.path(nlcd01, "nlcd_2001_landcover_2011_edition_2014_10_10.img")
if (!file.exists(nlcd01_img)){
  loc <- "http://www.landfire.gov/bulk/downloadfile.php?TYPE=nlcd2001v2&FNAME=nlcd_2001_landcover_2011_edition_2014_10_10.zip"
  dest <- paste0(raw_dir, "/nlcd2001.zip")
  download.file(loc, dest)
  system(paste0("unzip ", dest, "-d ", raw_dir))
  unlink(dest)
  assert_that(file.exists(nlcd01_img))
  system(paste0("aws s3 sync ",
                nlcd01, "/ ",
                s3_raw, "nlcd_2001_landcover_2011_edition_2014_10_10/"))

}

# Download NLCD 2006
nlcd06_img <- file.path(nlcd06, "nlcd_2006_landcover_2011_edition_2014_10_10.img")
if (!file.exists(nlcd06_img)){
  loc <- "http://www.landfire.gov/bulk/downloadfile.php?TYPE=nlcd2006&FNAME=nlcd_2006_landcover_2011_edition_2014_10_10.zip"
  dest <- paste0(raw_dir, "/nlcd2006.zip")
  download.file(loc, dest)
  system(paste0("unzip ", dest, "-d ", raw_dir))
  unlink(dest)
  assert_that(file.exists(nlcd06_img))
  system(paste0("aws s3 sync ",
                nlcd06, "/ ",
                s3_raw, "nlcd_2006_landcover_2011_edition_2014_10_10/"))
}

# Download the NLCD 2011

nlcd_img <- file.path(nlcd, 'nlcd_2011_landcover_2011_edition_2014_10_10.img')
if (!file.exists(nlcd_img)) {
  loc <- "http://www.landfire.gov/bulk/downloadfile.php?TYPE=nlcd2011&FNAME=nlcd_2011_landcover_2011_edition_2014_10_10.zip"
  dest <- paste0(raw_dir, ".zip")
  download.file(loc, dest)
  print("extracting")
  system(paste0("unzip ", dest, "-d ", raw_dir))
  unlink(dest)
  assert_that(file.exists(nlcd_img))
  system(paste0("aws s3 sync ",
                nlcd, "/ ",
                s3_raw, "nlcd_2011_landcover_2011_edition_2014_10_10/"))
}

# Download NLCD percent developed imperviousness 2001
nlcd_pdi_01_img <- file.path(nlcd_pdi_01, "nlcd_2001_impervious_2011_edition_2014_10_10.img")
if (!file.exists(nlcd_pdi_01_img)){
  loc <- "http://www.landfire.gov/bulk/downloadfile.php?TYPE=nlcd2001v2&FNAME=nlcd_2001_impervious_2011_edition_2014_10_10.zip"
  dest <- paste0(raw_dir, "/nlcdpdi01.zip")
  download.file(loc, dest)
  system(paste0("unzip ", dest, "-d ", raw_dir))
  unlink(dest)
  assert_that(file.exists(nlcd_pdi_01_img))
  system(paste0("aws s3 sync ",
                nlcd_pdi_01, "/ ",
                s3_raw, "nlcd_2001_impervious_2011_edition_2014_10_10/"))
}

# Download NLCD percent developed imperviousness 2006
nlcd_pdi_06_img <- file.path(nlcd_pdi_06, "nlcd_2006_impervious_2011_edition_2014_10_10.img")
if (!file.exists(nlcd_pdi_06_img)){
  loc <- "http://www.landfire.gov/bulk/downloadfile.php?TYPE=nlcd2001v2&FNAME=nlcd_2006_impervious_2011_edition_2014_10_10.zip"
  dest <- paste0(raw_dir, "/nlcdpdi06.zip")
  download.file(loc, dest)
  system(paste0("unzip ", dest, "-d ", raw_dir))
  unlink(dest)
  assert_that(file.exists(nlcd_pdi_06_img))
  system(paste0("aws s3 sync ",
                nlcd_pdi_06, "/ ",
                s3_raw, "nlcd_2006_impervious_2011_edition_2014_10_10/"))
}

# Download NLCD percent developed imperviousness 2011
nlcd_pdi_11_img <- file.path(nlcd_pdi_11, "nlcd_2011_impervious_2011_edition_2014_10_10.img")
if (!file.exists(nlcd_pdi_11_img)){
  loc <- "http://www.landfire.gov/bulk/downloadfile.php?TYPE=nlcd2001v2&FNAME=nlcd_2011_impervious_2011_edition_2014_10_10.zip"
  dest <- paste0(raw_dir, "/nlcdpdi11.zip")
  download.file(loc, dest)
  system(paste0("unzip ", dest, "-d ", raw_dir))
  unlink(dest)
  assert_that(file.exists(nlcd_pdi_11_img))
  system(paste0("aws s3 sync ",
                nlcd_pdi_11, "/ ",
                s3_raw, "nlcd_2011_impervious_2011_edition_2014_10_10/"))
}
