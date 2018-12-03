classify_nlcd <- function(x) {
  ifelse(x == 11, 'Open Water',
         ifelse(x == 12, 'Perennial Snow/Ice',
                ifelse(x == 21, 'Developed Open Space',
                       ifelse(x == 22, 'Developed Low intensity',
                              ifelse(x == 23, 'Developed Medium Intensity',
                                     ifelse(x == 24, 'Developed High Intensity',
                                            ifelse(x == 31, ' Barren Land',
                                                   ifelse(x == 41, 'Deciduous Forest',
                                                          ifelse(x == 42, 'Evergreen Forest',
                                                                 ifelse(x == 43, 'Mixed Forest',
                                                                        ifelse(x == 52, 'Shrub/Scrub',
                                                                               ifelse(x == 71, 'Grassland/Herbaceous',
                                                                                      ifelse(x == 81, 'Pasture/Hay',
                                                                                             ifelse(x == 82, 'Cultivated Crops',
                                                                                                    ifelse(x == 90, 'Woody Wetlands',
                                                                                                           ifelse(x == 95, 'Emergent Herbaceous Wetlands',
                                                                                                                  NA))))))))))))))))
}