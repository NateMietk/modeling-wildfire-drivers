# plot rpart of top 15 vars
for(i in unique(top_15_sig_importance$na_l2name)) {
  ecoreg <- gsub(' ', '-', i)
  print(ecoreg)
  
  subset <- fpa_all_vars %>% 
    filter(na_l2name == ecoreg) %>%
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
    mutate(pct_unbalanced = as.integer(round(counts/sum(counts), 2)*100)) %>%
    dplyr::select(-counts) %>%
    spread(ignition, pct_unbalanced)
  
  # Create the weights file for unbalanced data 
  model_weights <- ifelse(train$ignition == "Human",
                          how_unbalanced$Human, how_unbalanced$Lightning)
  
  importance_subset <- top_15_sig_importance %>% 
    filter(na_l2name == i) %>%
    droplevels() %>%
    filter(!str_detect(variables, "state")) %>%
    mutate(variables = case_when(
      str_detect(variables, "season") ~ "seasons",
      str_detect(variables, "ESPLF_Name") ~ "ESPLF_Name",
      str_detect(variables, "nlcd") ~ "nlcd",
      str_detect(variables, "ZONE_NAME") ~ "ZONE_NAME",
      str_detect(variables, "us_l3name") ~ "us_l3name",
      str_detect(variables, "owner_descr") ~ "owner_descr",
      TRUE ~ .$variables))  
  
  importance_val_list <- as.vector(importance_subset$variables)
  importance_val_list <- paste0("c('ignition', ", paste(shQuote(importance_val_list), collapse=", "), ')')
  
  if(!file.exists(file.path(ctree_model_dir, paste0('ctree_model_', gsub(' ', '-', i), '.rds')))) {
    if(ecoreg == 'Western-cordillera') {    
      training_parameters <- trainControl(verboseIter = TRUE,
                                          savePredictions = TRUE)
      } else {
        training_parameters <- trainControl(method = "cv",
                                            number = ifelse(ecoreg == 'Southeastern-usa-plains', 2, 10),
                                            verboseIter = TRUE,
                                            savePredictions = TRUE)
        }


    model_ctree <- caret::train(ignition ~ .,
                                data = train %>%
                                  dplyr::select_(importance_val_list),
                                method = "ctree",
                                weights = model_weights,
                                trControl = training_parameters)
    
    fitted_model_ctree <- partykit::ctree(ignition ~ ., 
                                          data = train %>%
                                            dplyr::select_(importance_val_list), 
                                          weights = model_weights,
                                          control = partykit::ctree_control(maxdepth = 3, 
                                                                            mincriterion = model_ctree$bestTune[1,1]
                                                                            # ,testtype = "MonteCarlo"
                                                                            # ,nresample = 10000
                                          ))

    write_rds(fitted_model_ctree, file.path(ctree_model_dir, paste0('ctree_model_', ecoreg, '.rds')))
  } else {
    fitted_model_ctree <- read_rds(file.path(ctree_model_dir, paste0('ctree_model_', ecoreg, '.rds')))
  }
  
  if(!file.exists(file.path(ctree_model_dir, paste0('ctree_confusion_', ecoreg, '.rds')))) {
    predict_ctree_class <- predict(fitted_model_ctree, test)
    predict_ctree <- as_tibble(data.frame(pred = predict_ctree_class,
                                          obs = test$ignition,
                                          Human = predict(fitted_model_ctree, test, type = "prob")[, 1],
                                          Lightning = predict(fitted_model_ctree, test, type = "prob")[, 2]))
    
    write_rds(predict_ctree, file.path(ctree_model_dir, paste0('ctree_prediction_', gsub(' ', '-', i), '.rds')))
    
    confusion_ctree <- confusionMatrix(predict_ctree_class, test$ignition)
    write_rds(confusion_ctree, file.path(ctree_model_dir, paste0('ctree_confusion_', gsub(' ', '-', i), '.rds')))
  }
  
  # if(!file.exists(file.path(rpart_model_dir, paste0('rpart_model_', ecoreg, '.rds')))) {
  #   training_parameters <- trainControl(method = "cv",
  #                                       number = 10,
  #                                       summaryFunction = twoClassSummary,
  #                                       classProbs = TRUE,
  #                                       verboseIter = TRUE,
  #                                       savePredictions = TRUE)
  # 
  #   model_rpart <- caret::train(ignition ~ .,
  #                               data = train %>%
  #                                 dplyr::select_(importance_val_list),
  #                               method = "rpart",
  #                               weights = model_weights,
  #                               metric = "ROC",
  #                               trControl = training_parameters)
  #   # 
  #   # fitted_model_rpart <- rpart::rpart(ignition ~ ., 
  #   #                                    data = train %>%
  #   #                                      dplyr::select_(importance_val_list), 
  #   #                                    weights = model_weights,
  #   #                                    method="class",
  #   #                                    control = rpart::rpart.control(
  #   #                                      minsplit = 1
  #   #                                      ,minbucket = 1
  #   #                                    ))
  #   
  #   pruned_model_rpart <- rpart::prune(model_rpart$finalModel, cp = model_rpart$bestTune[1,1])    
  #   
  #   # pruned_model_rpart <- as_tibble(fitted_model_rpart$cptable) %>%
  #   #   filter(nsplit > 0) %>%
  #   #   filter(xerror <= min(xerror) + xstd) %>%
  #   #   filter(xerror == max(xerror)) %>%
  #   #   select(CP) %>%
  #   #   unlist() %>%
  #   #   rpart::prune(fitted_model_rpart, cp = .)   
  #   
  #   write_rds(pruned_model_rpart, file.path(rpart_model_dir, paste0('rpart_model_', ecoreg, '.rds')))
  # } else {
  #   pruned_model_rpart <- read_rds(file.path(rpart_model_dir, paste0('rpart_model_', ecoreg, '.rds')))
  # }
  # 
  # if(!file.exists(file.path(rpart_model_dir, paste0('rpart_confusion_', ecoreg, '.rds')))) {
  #   predict_rpart_class <- predict(pruned_model_rpart, test, type="class")
  #   predict_rpart <- as_tibble(data.frame(pred = predict_rpart_class,
  #                                         obs = test$ignition,
  #                                         Human = predict(pruned_model_rpart, test, type = "prob")[, 1],
  #                                         Lightning = predict(pruned_model_rpart, test, type = "prob")[, 2]))
  #   
  #   write_rds(predict_rpart, file.path(rpart_model_dir, paste0('rpart_prediction_', ecoreg, '.rds')))
  #   
  #   confusion_rpar <- confusionMatrix(predict_rpart_class, test$ignition)
  #   write_rds(confusion_rpar, file.path(rpart_model_dir, paste0('rpart_confusion_', ecoreg, '.rds')))
  # }
  
  system(paste0('aws s3 sync ', model_dir, ' ', s3_proc_models, ' --delete'))
}

ctree_confusion_files <- list.files(ctree_model_dir, pattern = 'confusion', full.names = TRUE)
confusion_matrix <- do.call(rbind,lapply(ctree_confusion_files,
                                         function(x) {
                                           ecoreg_name <- unlist(strsplit(x, '_|\\.'))[4] %>%
                                             gsub('-', ' ', .)
                                           
                                           file_in <- read_rds(x)
                                           df_out <- as_tibble(data.frame(ecoreg_name, file_in$overall)) %>%
                                             rownames_to_column() %>%
                                             tidyr::spread(key = rowname, value = file_in.overall) %>%
                                             dplyr::select(na_l2name = as.factor(ecoreg_name), 
                                                           everything())
                                           return(df_out)
                                         }))

ctree_prediction_files <- list.files(ctree_model_dir, pattern = 'prediction', full.names = TRUE)
ctree_model_prediction <- do.call(rbind,lapply(ctree_prediction_files,
                                               function(x) {
                                                 ecoreg_name <- unlist(strsplit(x, '_|\\.'))[4] %>%
                                                   gsub('-', ' ', .) 
                                                 df <- read_rds(x) %>%
                                                   mutate(na_l2name = as.factor(ecoreg_name)) %>%
                                                   as_tibble()
                                                 return(df)}))

model_ctree_roc <- ctree_model_prediction %>%
  ggplot() + 
  geom_roc(aes(m = Human, d = ifelse(obs == 'Human', 1, 0)), n.cuts=0, color = 'red') +
  coord_equal() +
  style_roc() +
  facet_wrap(~ na_l2name, labeller = label_wrap_gen(width = 15), ncol = 7)

roc_values <- calc_auc(model_ctree_roc)

model_ctree_roc <- model_ctree_roc +
  annotate("text", x=0.75, y=0.25, label=paste("AUC =", round(roc_values$AUC, 3)))
model_ctree_roc

lapply(ctree_mod_files, 
       function(x) {
         ecoreg_name <- unlist(strsplit(x, '_|\\.'))[4] %>%
           gsub('-', ' ', .)
         
         df <- read_rds(x)$finalModel 
         
         pdf(file.path(ctree_plots_dir, paste0(gsub(' ', '-', ecoreg_name), ".pdf")))
         ctree.plot::ctree.plot(df, main = ecoreg_name)
         dev.off()
       }
)