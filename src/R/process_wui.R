#Import the Wildland-Urban Interface and process
if (!exists('wui')) {
  if (!file.exists(file.path(wui_out, "wui_bounds.gpkg"))) {
    # st_layers(dsn = file.path(wui_prefix, "CONUS_WUI_cp12_d.gdb"))
    
    wui <- st_read(dsn = file.path(wui_prefix, "CONUS_WUI_cp12_d.gdb"),
                   layer = "CONUS_WUI_cp12") %>%
      st_transform(st_crs(usa_shp)) %>%
      mutate(Class90 = classify_wui(WUICLASS90),
             Class00 = classify_wui(WUICLASS00),
             Class10 = classify_wui(WUICLASS10)) 
    
    st_write(wui, file.path(wui_out, "wui_bounds.gpkg"),
             driver = "GPKG",
             delete_layer = TRUE)
    system(paste0("aws s3 sync ", prefix, " ", s3_base))
    
  } else {
    wui <- st_read(dsn = file.path(wui_out, "wui_bounds.gpkg"))
  }
}

gridded_wui_1990 <- fasterize::fasterize(wui, raster_mask, field = "Class90")

gridded_wui_1990  <- ratify(gridded_wui_1990)
rat <- levels(gridded_wui_1990)[[1]]
rat$Class90 <- levels(wui$Class90)
levels(gridded_wui_1990) <- rat
# levels(x)

writeRaster(gridded_wui_1990, file.path('data', 'gridded_wui_1990.tif'), format = 'GTiff')

gridded_wui_2000 <- fasterize::fasterize(wui, raster_mask, field = "Class00")

gridded_wui_2000  <- ratify(gridded_wui_2000)
rat <- levels(gridded_wui_2000)[[1]]
rat$Class00 <- levels(wui$Class00)
levels(gridded_wui_2000) <- rat

writeRaster(gridded_wui_2000, file.path('data', 'gridded_wui_2000.tif'))

gridded_wui_2010 <- fasterize::fasterize(wui, raster_mask, field = "Class10")

gridded_wui_2010  <- ratify(gridded_wui_2010)
rat <- levels(gridded_wui_2010)[[1]]
rat$Class10 <- levels(wui$Class10)
levels(gridded_wui_2010) <- rat

writeRaster(gridded_wui_2010, file.path('data', 'gridded_wui_2010.tif'))