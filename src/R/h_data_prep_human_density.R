

# List all decadal tiffs from the CIESIN gridded census products
anthro_list <- list.files(
  file.path(anthro_dir, 'gridded_census'),
  pattern = '*.tif',
  full.names = TRUE,
  recursive = TRUE
)

if (!file.exists(file.path(anthro_dir, 'gridded_census', "ciesin_extraction.rds"))) {
  # Extract all of the gridded products by each FPA-FOD fire iginition point in parallel
  sfInit(parallel = TRUE, cpus = parallel::detectCores())
  sfExport(list = c("fpa_clean"))
  
  extractions <- sfLapply(as.list(anthro_list),
                          fun = extract_anthro,
                          shp_mask = fpa_clean)
  sfStop()
  
  # Push the csv outputs to S3 so we do not have to re-run this
  system(
    "aws s3 sync data/anthro/gridded_census s3://earthlab-modeling-human-ignitions/anthro/gridded_census"
  )
  
  # Quick check to make sure the extractions have been taken for each of the FPA-FOD points
  stopifnot(all(lapply(extractions, nrow) == nrow(fpa_clean)))
  
  # Create a 'master' extractions table with all of the raw decadal values
  extraction_df <- extractions %>%
    bind_cols %>%
    as_tibble %>%
    mutate(index = ID) %>%
    dplyr::select(-starts_with("ID")) %>%
    rename(ID = index) %>%
    mutate(
      FPA_ID = data.frame(fpa_clean)$FPA_ID,
      year_month_day = data.frame(fpa_clean)$year_month_day,
      STATE = data.frame(fpa_clean)$STATE
    ) %>%
    dplyr::select(-starts_with('X'))
  
  extraction_df %>%
    write_rds(.,
              file.path(anthro_dir, 'gridded_census', "ciesin_extraction.rds"))
  
  system(
    "aws s3 sync data/anthro/gridded_census s3://earthlab-modeling-human-ignitions/anthro/gridded_census"
  )
} else {
  extraction_df <-
    read_rds(file.path(anthro_dir, 'gridded_census', "ciesin_extraction.rds"))
}

# Clean extraction data frame for imputing --------------------------------

extraction_vars <- extraction_df %>%
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
  gather(variable, value,-FPA_ID,-ID,-year_month_day,-STATE) %>%
  mutate(
    year = case_when(
      .$variable == 'usba90' ~ 1990,
      # Population with a bachelors degree
      .$variable == 'usba00' ~ 2000,
      .$variable == 'usba10' ~ 2010,
      .$variable == 'usba20' ~ 2020,
      .$variable == 'ussevp90' ~ 1990,
      # Population living 50% below the poverty line
      .$variable == 'ussevp00' ~ 2000,
      .$variable == 'ussevp10' ~ 2010,
      .$variable == 'ussevp20' ~ 2020,
      .$variable == 'uspov90' ~ 1990,
      # Population living 200% below the poverty line
      .$variable == 'uspov00' ~ 2000,
      .$variable == 'uspov10' ~ 2010,
      .$variable == 'uspov20' ~ 2020,
      .$variable == 'ushs90' ~ 1990,
      # Population with a high school degree
      .$variable == 'ushs00' ~ 2000,
      .$variable == 'ushs10' ~ 2010,
      .$variable == 'ushs20' ~ 2020,
      .$variable == 'ushu90' ~ 1990,
      # Number of housing units
      .$variable == 'ushu00' ~ 2000,
      .$variable == 'ushu10' ~ 2010,
      .$variable == 'ushu20' ~ 2020,
      .$variable == 'uslowi90' ~ 1990,
      # Population living below poverty level
      .$variable == 'uslowi00' ~ 2000,
      .$variable == 'uslowi10' ~ 2010,
      .$variable == 'uslowi20' ~ 2020,
      .$variable == 'uspop90' ~ 1990,
      # Population
      .$variable == 'uspop00' ~ 2000,
      .$variable == 'uspop10' ~ 2010,
      .$variable == 'uspop20' ~ 2020,
      .$variable == 'ussea90' ~ 1990,
      # Vacant housing unit, for seasonal, recreational or occasional use
      .$variable == 'ussea00' ~ 2000,
      .$variable == 'ussea10' ~ 2010,
      .$variable == 'ussea20' ~ 2020
    )
  ) %>%
  mutate(variable = if_else(
    grepl("usba", variable),
    'bachelors_degree',
    if_else(
      grepl("ussevp", variable),
      'pop_poverty_below_50',
      if_else(
        grepl("uspov", variable),
        'pop_poverty_below_200',
        if_else(
          grepl("ushs", variable),
          'highschool_degree',
          if_else(
            grepl("ushu", variable),
            'housing_units',
            if_else(
              grepl("uslowi", variable),
              'pop_poverty_below_line',
              if_else(
                grepl("uspop", variable),
                'population',
                if_else(grepl("ussea", variable), 'seasonal_housing_units', variable)
              )
            )
          )
        )
      )
    )
  )) %>%
  dplyr::group_by(FPA_ID, variable) %>%
  arrange(FPA_ID, variable, year) %>%
  dplyr::mutate(value = spline(x = year, y = value, xout = year)$y,
                value = if_else(value < 0, 0, value)) %>%
  arrange(FPA_ID, variable, year) %>%
  dplyr::ungroup() %>%
  unite(year_var, variable, year) %>%
  spread(year_var, value) %>%
  mutate(
    bachelors_degree = ifelse(
      year(year_month_day) < 2000,
      bachelors_degree_1990,
      ifelse(
        year(year_month_day) >= 2000 |
          year(year_month_day) <= 2009,
        bachelors_degree_2000,
        ifelse(year(year_month_day) >= 2010, bachelors_degree_2010, NA)
      )
    ),
    highschool_degree = ifelse(
      year(year_month_day) < 2000,
      highschool_degree_1990,
      ifelse(
        year(year_month_day) >= 2000 |
          year(year_month_day) <= 2009,
        highschool_degree_2000,
        ifelse(year(year_month_day) >= 2010, highschool_degree_2010, NA)
      )
    ),
    pop_poverty_below_50 = ifelse(
      year(year_month_day) < 2000,
      pop_poverty_below_50_1990,
      ifelse(
        year(year_month_day) >= 2000 |
          year(year_month_day) <= 2009,
        pop_poverty_below_50_2000,
        ifelse(year(year_month_day) >= 2010, pop_poverty_below_50_2010, NA)
      )
    ),
    pop_poverty_below_200 = ifelse(
      year(year_month_day) < 2000,
      pop_poverty_below_200_1990,
      ifelse(
        year(year_month_day) >= 2000 |
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

system(
  "aws s3 sync data/anthro/gridded_census s3://earthlab-modeling-human-ignitions/anthro/gridded_census"
)