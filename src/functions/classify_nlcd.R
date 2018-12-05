# convert to case_when
classify_nlcd <- function(x) {
  case_when(x == 11 ~ 'Open Water',
            x == 12 ~ 'Perennial Snow/Ice',
            x == 21 ~ 'Developed Open Space',
            x == 22 ~ 'Developed Low intensity',
            x == 23 ~ 'Developed Medium Intensity',
            x == 24 ~ 'Developed High Intensity',
            x == 31 ~ 'Barren Land',
            x == 41 ~ 'Deciduous Forest',
            x == 42 ~ 'Evergreen Forest',
            x == 43 ~ 'Mixed Forest',
            x == 52 ~ 'Shrub/Scrub',
            x == 71 ~ 'Grassland/Herbaceous',
            x == 81 ~ 'Pasture/Hay',
            x == 82 ~ 'Cultivated Crops',
            x == 90 ~ 'Woody Wetlands',
            x == 95 ~ 'Emergent Herbaceous Wetlands',
            NA_character_)
}