

if(!file.exists(file.path(extraction_anthro, 'ciesin_extraction.rds'))) {
  census_list <- list.files(proc_gridded_census, pattern = '*.tif', full.names = TRUE)
  
  # Reproject all grids and mask to lower 48
  if(length(census_list) != 19) {
    # List all decadal tiffs from the CIESIN gridded census products
    anthro_list <- list.files(file.path(anthro_dir, 'gridded_census'),
                              pattern = '*.tif', full.names = TRUE, recursive = TRUE)
    
    pboptions(type = 'txt', use_lb = TRUE)
    cl <- makeCluster(getOption("cl.cores", detectCores()/2)) # 8 cores on a m5d.4xlarge instance
    
    pblapply(anthro_list, function(x, raster_mask, masks, dir) {
      require(tidyverse)
      require(raster)
      
      names <- x %>%
        basename()
      if(!file.exists(file.path(dir, names))) {
        rst <- raster::raster(x) %>%
          raster::projectRaster(., raster_mask, method = 'bilinear') %>%
          raster::mask(masks)
        
        writeRaster(rst, file.path(dir, names))
      }
    }, 
    raster_mask = raster_mask, 
    masks = as(usa_shp, 'Spatial'),
    dir = proc_gridded_census, 
    cl = cl)
    
    stopCluster(cl)
  }

  census_stack <- raster::stack(census_list)
  
  fpa_slim <- fpa_clean %>%
    dplyr::select(fpa_id, fire_year)
  
  census_extract <- velox(census_stack)$extract_points(sp = fpa_slim) %>%
    as_tibble() 
  # Rename to the raster layer names
  colnames(census_extract) <- names(census_stack)
  
  fpa_census <- as.data.frame(census_extract) %>%
    mutate(fpa_id = as.data.frame(fpa_slim)$fpa_id) %>%
    left_join(as.data.frame(fpa_slim), ., by = 'fpa_id') %>%
    dplyr::select(-geom) %>%
    rename_all(tolower) %>% as_tibble() 
  
  extraction_vars <- fpa_census %>%
    mutate(
      usba10 = NA,
      usba20 = NA,
      ussevp10 = NA,
      ussevp20 = NA,
      uspov10 = NA,
      uspov20 = NA,
      ushu20 = NA,
      ushs10 = NA,
      ushs20 = NA,
      uslowi10 = NA,
      uslowi20 = NA,
      uspop20 = NA,
      ussea20 = NA
    ) %>%
    gather(variable, value,-fpa_id,-fire_year) %>%
    mutate(
      year = case_when(
        .$variable == 'usba90' ~ 1990, # Population with a bachelors degree
        .$variable == 'usba00' ~ 2000,
        .$variable == 'usba10' ~ 2010,
        .$variable == 'usba20' ~ 2020,
        .$variable == 'ussevp90' ~ 1990, # Population living 50% below the poverty line
        .$variable == 'ussevp00' ~ 2000,
        .$variable == 'ussevp10' ~ 2010,
        .$variable == 'ussevp20' ~ 2020,
        .$variable == 'uspov90' ~ 1990, # Population living 200% below the poverty line
        .$variable == 'uspov00' ~ 2000,
        .$variable == 'uspov10' ~ 2010,
        .$variable == 'uspov20' ~ 2020,
        .$variable == 'ushs90' ~ 1990, # Population with a high school degree
        .$variable == 'ushs00' ~ 2000,
        .$variable == 'ushs10' ~ 2010,
        .$variable == 'ushs20' ~ 2020,
        .$variable == 'ushu90' ~ 1990, # Number of housing units
        .$variable == 'ushu00' ~ 2000,
        .$variable == 'ushu10' ~ 2010,
        .$variable == 'ushu20' ~ 2020,
        .$variable == 'uslowi90' ~ 1990, # Population living below poverty level
        .$variable == 'uslowi00' ~ 2000,
        .$variable == 'uslowi10' ~ 2010,
        .$variable == 'uslowi20' ~ 2020,
        .$variable == 'uspop90' ~ 1990, # Population
        .$variable == 'uspop00' ~ 2000,
        .$variable == 'uspop10' ~ 2010,
        .$variable == 'uspop20' ~ 2020,
        .$variable == 'ussea90' ~ 1990, # Vacant housing unit, for seasonal, recreational or occasional use
        .$variable == 'ussea00' ~ 2000,
        .$variable == 'ussea10' ~ 2010,
        .$variable == 'ussea20' ~ 2020
      )
    ) %>%
  mutate(variable = case_when(
    grepl("usba", variable) ~ 'bachelors_degree',
    grepl("ussevp", variable) ~ 'pop_poverty_below_50',
    grepl("uspov", variable) ~ 'pop_poverty_below_200',
    grepl("ushs", variable) ~ 'highschool_degree',
    grepl("ushu", variable) ~ 'housing_units',
    grepl("uslowi", variable) ~ 'pop_poverty_below_line',
    grepl("uspop", variable) ~ 'population',
    grepl("ussea", variable) ~ 'seasonal_housing_units'))
                
extraction_vars2 <- extraction_vars %>%
  dplyr::mutate(yr = as.Date(as.character(year), format = "%Y"),
                variable = as.factor(variable)) %>% 
  dplyr::group_by(fpa_id, variable) %>%
  dplyr::mutate(value = zoo::na.spline(object = value, x = yr, xout = yr)$y)

,
                value = case_when(value < 0 ~ 0, TRUE ~ value))  %>%
  arrange(fpa_id, variable, year) %>%
  dplyr::ungroup() %>%
  unite(year_var, variable, year) %>%
  spread(year_var, value, fill = 0) 

approx(x = df$year,
       y = df$wmean,
       xout = predict_seq)

get_var_value <- function(x1, x2, x3) {
  case_when(
    year < 2000 ~ x,
    year >= 2000 | year <= 2009 ~ x,
    year >= 2010 ~ x, NA_real_)
}

extraction_vars3 <- extraction_vars2 %>%
  mutate(bachelors_degree = get_var_value(bachelors_degree_1990, bachelors_degree_2000, bachelors_degree_2010))



    bachelors_degree = case_when(
      year < 2000 ~ bachelors_degree_1990,
      year >= 2000 | year <= 2009 ~ bachelors_degree_2000,
      year >= 2010 ~ bachelors_degree_2010, NA_real_),
    highschool_degree = case_when(
      year < 2000 ~ highschool_degree_1990,
      year >= 2000 | year <= 2009 ~ highschool_degree_2000,
      year >= 2010 ~ highschool_degree_2010, NA_real_),
    pop_poverty_below_50 = case_when(
      year < 2000 ~ pop_poverty_below_50_1990,
      year >= 2000 | year <= 2009 ~ pop_poverty_below_50_2000,
      year >= 2010 ~ pop_poverty_below_50_2010, NA_real_),
    pop_poverty_below_200 = case_when(
      year < 2000 ~ pop_poverty_below_200_1990,
      year >= 2000 |
          year(year_month_day) <= 2009,
        pop_poverty_below_200_2000,
        ifelse(year(year_month_day) >= 2010, pop_poverty_below_200_2010, NA)
      )
    ),
    housing_units = ifelse(
      year(year_month_day) < 2000,
      housing_units_1990,
      ifelse(
        year(year_month_day) >= 2000 |
          year(year_month_day) <= 2009,
        housing_units_2000,
        ifelse(year(year_month_day) >= 2010, housing_units_2010, NA)
      )
    ),
    pop_poverty_below_line = ifelse(
      year(year_month_day) < 2000,
      pop_poverty_below_line_1990,
      ifelse(
        year(year_month_day) >= 2000 |
          year(year_month_day) <= 2009,
        pop_poverty_below_line_2000,
        ifelse(year(year_month_day) >= 2010, pop_poverty_below_line_2010, NA)
      )
    ),
    population = ifelse(
      year(year_month_day) < 2000,
      population_1990,
      ifelse(
        year(year_month_day) >= 2000 |
          year(year_month_day) <= 2009,
        population_2000,
        ifelse(year(year_month_day) >= 2010, population_2010, NA)
      )
    ),
    seasonal_housing_units = ifelse(
      year(year_month_day) < 2000,
      seasonal_housing_units_1990,
      ifelse(
        year(year_month_day) >= 2000 |
          year(year_month_day) <= 2009,
        seasonal_housing_units_2000,
        ifelse(year(year_month_day) >= 2010, seasonal_housing_units_2010, NA)
      )
    )
  ) %>%
  dplyr::select(
    FPA_ID,
    year_month_day,
    population,
    bachelors_degree,
    highschool_degree,
    pop_poverty_below_line,
    pop_poverty_below_50,
    pop_poverty_below_200,
    housing_units,
    seasonal_housing_units
  )

extraction_vars %>%
  write_rds(.,
            file.path(anthro_dir, 'gridded_census', "ciesin_extraction.rds"))

}



