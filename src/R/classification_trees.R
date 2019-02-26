# plot rpart of top 15 vars
training_parameters <- trainControl(summaryFunction = twoClassSummary,
                                    classProbs = TRUE,
                                    verboseIter  = TRUE,
                                    allowParallel = TRUE,
                                    savePredictions = TRUE)

model_weights <- ifelse(train$ignition == "Human",
                        how_unbalanced$Human, how_unbalanced$Lightning)

top_15_sig_importance %>%
  split(.$lvl2_ecoregion)


model_rpart_us <- caret::train(ignition ~ tmmx_mean_lag_0+vector_primary_rds_distance+vector_tertiary_rds_distance+
                                 vector_secondary_rds_distance+vector_railroad_distance+
                                 state+ffwi_mean_lag_0+def_mean_lag_0++vpd_min_3_month+def_mean_3_month+vpd_min_6_month,
                               data = train,
                               method = "rpart",
                               weights = model_weights,
                               trControl = training_parameters,
                               tuneLength = 15)
plot(model_rpart_us$finalModel)
text(model_rpart_us$finalModel)
library(rattle)
fancyRpartPlot(model_rpart_us$finalModel)
