# plot rpart of top 15 vars
ttt <- unique(top_15_sig_importance$lvl2_ecoregion)
for(i in ttt) {
  i <- 'Everglades'
  
  set.seed(224)
  subset <- fpa_all_vars %>% 
    filter(na_l2name == i ) %>%
    droplevels() %>%
    select_if(~ nlevels(.) > 1 | is.numeric(.))
  
  train <- subset %>%
    dplyr::sample_frac(0.6)
  # Create testing data - 40%
  test <- subset %>% 
    anti_join(., train, by = 'row_id')  %>%
    droplevels()
  
  how_unbalanced <- train %>%
    group_by(ignition) %>%
    summarise(counts = n()) %>%
    mutate(pct_unbalanced = counts/sum(counts)) %>%
    dplyr::select(-counts) %>%
    spread(ignition, pct_unbalanced)
  
  # Create the weights file for unbalanced data 
  model_weights <- ifelse(train$ignition == "Human",
                          how_unbalanced$Human, how_unbalanced$Lightning)
  
  importance_subset <- top_15_sig_importance %>% 
    filter(na_l2name == i ) %>%
    droplevels() %>%
    mutate(variables = case_when(
      str_detect(variables, "season") ~ "seasons",
      str_detect(variables, "state") ~ "state",
      TRUE ~ .$variables))  
  
  importance_val_list <- as.vector(importance_subset$variables)
  importance_val_list <- paste0("c('ignition', ", paste(shQuote(importance_val_list), collapse=", "), ')')

  train2 <- train %>%
    dplyr::select_(importance_val_list)

  training_parameters <- trainControl(method = "repeatedcv",
                                      number = 10,
                                      repeats = 5,
                                      summaryFunction = twoClassSummary,
                                      classProbs = TRUE,
                                      verboseIter  = TRUE)
  model_rpart <- caret::train(ignition ~ .,
                                 data = train2,
                                 method = "rpart",
                                 weights = model_weights,
                                 tuneLength = 5, 
                                 trControl = training_parameters)
  write_rds(model_rpart, file.path(model_dir, 'cart_', i, '.rds'))
  
  predict_rpart <- predict(model_rpart, test)
  write_rds(predict_rpart, file.path(model_dir, 'cart_prediction_', i, '.rds'))
  
  confusion_rpar <- confusionMatrix(predict_rpart, test$ignition)
  write_rds(confusion_rpar, file.path(model_dir, 'cart_confusion_', i, '.rds'))
  
           }

prunedtree <- prune(model_rpart$finalModel, cp= model_rpart$finalModel$cptable[which.min(model_rpart$finalModel$cptable[,"rel error"]),"CP"])

plot(prunedtree)
text(prunedtree)
library(rattle)
fancyRpartPlot(prunedtree)
