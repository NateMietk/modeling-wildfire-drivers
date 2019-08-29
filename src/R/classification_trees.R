# plot rpart of top 15 vars
for(i in unique(top_15_sig_importance$na_l2name)) {

  set.seed(224)
  subset <- fpa_all_vars %>% 
    filter(na_l2name == gsub(' ', '-', i)) %>%
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
    filter(na_l2name == i) %>%
    droplevels() %>%
    mutate(variables = case_when(
      str_detect(variables, "season") ~ "seasons",
      str_detect(variables, "state") ~ "state",
      str_detect(variables, "owner_descr") ~ "owner_descr",
      TRUE ~ .$variables))  
  
  importance_val_list <- as.vector(importance_subset$variables)
  importance_val_list <- paste0("c('ignition', ", paste(shQuote(importance_val_list), collapse=", "), ')')

  if(!file.exists(file.path(rpart_model_dir, paste0('rpart_', gsub(' ', '-', i), '.rds')))) {
    training_parameters <- trainControl(method = "repeatedcv",
                                        number = 10,
                                        repeats = 15,
                                        summaryFunction = twoClassSummary,
                                        classProbs = TRUE,
                                        verboseIter  = TRUE,
                                        savePredictions = TRUE)
    model_rpart <- caret::train(ignition ~ .,
                                data = train %>%
                                  dplyr::select_(importance_val_list),
                                method = "rpart",
                                weights = model_weights,
                                tuneLength = 5, 
                                trControl = training_parameters)
    write_rds(model_rpart, file.path(rpart_model_dir, paste0('rpart_', gsub(' ', '-', i), '.rds')))
    
    predict_rpart <- predict(model_rpart, test)
    write_rds(predict_rpart, file.path(rpart_model_dir, paste0('rpart_prediction_', gsub(' ', '-', i), '.rds')))
    
    confusion_rpar <- confusionMatrix(predict_rpart, test$ignition)
    write_rds(confusion_rpar, file.path(rpart_model_dir, paste0('rpart_confusion_', gsub(' ', '-', i), '.rds')))
    
    system(paste0('aws s3 sync ', model_dir, ' ', s3_proc_models, ' --delete'))
    
    }
}

model_rpart_roc <- model_rpart$pred %>%
  mutate(bool = ifelse(obs == 'Human', 1, 0)) %>%
  ggplot(aes(m = Human, d = bool)) + 
  geom_roc(n.cuts=0) + 
  coord_equal() +
  style_roc() +
  facet_wrap(~ na_l2name) 
model_rpart_roc <- model_rpart_roc +
  annotate("text", x=0.75, y=0.25, label=paste("AUC =", round(calc_auc(model_rpart_roc)$AUC, 4)))



prunedtree <- prune(model_rpart$finalModel, 
                    cp= model_rpart$finalModel$cptable[which.min(model_rpart$finalModel$cptable[,"rel error"]),"CP"])

plot(prunedtree)
text(prunedtree)
library(rattle)
fancyRpartPlot(prunedtree)
