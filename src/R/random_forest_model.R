
# Prep data frame
fpa_all_vars <- read_rds(file.path(extraction_dir, 'fpa_all_vars.rds')) %>%
  dplyr::select(-class, -stat_cause_descr) %>%
  mutate_if(is.numeric, replace_na, 0) %>% # random forests cannot deal with NA values
  mutate(row_id = row_number(),
         na_l2name = case_when(
           na_l2name == 'UPPER GILA MOUNTAINS (?)' ~ 'UPPER GILA MOUNTAINS',
           TRUE ~ as.character(na_l2name)),
         na_l2name = as.factor(na_l2name)) %>%
  na.omit() %>%
  droplevels() %>%
  dplyr::select(-fpa_id, -ESP_Name, -region, -na_l1name) %>%
  droplevels() %>%
  mutate_if(is.factor, funs(tolower)) %>%
  mutate_if(is.character, funs(capitalize)) %>%
  mutate_if(is.character, funs(gsub(' ', '-', .))) %>%
  mutate_if(is.character, funs(gsub('/', '-', .))) %>%
  mutate_if(is.character, as.factor) %>%
  mutate(state = as.factor(toupper(state)))

model_list <- list.files(file.path(model_dir), pattern = 'model_ranger_', full.names = TRUE)
if(length(model_list) != 20) {
  for(i in unique(fpa_all_vars$na_l2name)) {
    time_df <- NULL
    
    set.seed(224)
    subset <- fpa_all_vars %>% 
      filter(na_l2name == i ) %>%
      droplevels() %>%
      select_if(~ nlevels(.) > 1 | is.numeric(.))
    
    train <- subset %>%
      dplyr::sample_frac(0.6)
    
    print(paste('Ecoregion = ', i, '; Counts = ', nrow(train)))
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
    
    set.seed(432)
    mlr_tasked = makeClassifTask(data = train, target = "ignition", weights = model_weights)
    
    if(!file.exists(file.path(model_dir, 'tuneRanger_time.rds'))) {
      # Time estimation
      tuneRanger_time <- lubridate::seconds_to_period(estimateTimeTuneRanger(task = mlr_tasked, num.trees = 1000,
                                                                             num.threads = parallel::detectCores(), iters= 10))
      l2_ecoregion <- i
      time_df = rbind(time_df, data.frame(tuneRanger_time, l2_ecoregion))
      write_rds(time_df, file.path(model_dir, 'tuneRanger_time.rds'))
    }
    
    # Tuning process for the lower 48 states
    if(!file.exists(file.path(model_dir, paste0('tuned_ranger_', i, '.rds')))) {
      set.seed(432)
      mlr_tasked = makeClassifTask(data = train, target = "ignition", weights = model_weights)
      
      tuned_ranger <- tuneRanger(mlr_tasked, measure = list(multiclass.brier), num.trees = 1000, time.budget = 36000,
                                 num.threads = parallel::detectCores(), build.final.model = FALSE)
      tuned_ranger <- write_rds(tuned_ranger, file.path(model_dir, paste0('tuned_ranger_', i, '.rds')))
      system(paste0('aws s3 sync ', model_dir, ' ', s3_proc_models))
    } else {
      tuned_ranger <- read_rds(file.path(model_dir, paste0('tuned_ranger_', i, '.rds')))
    }
    
    # Model with the tuned hyperparameter from tuneRanger
    if(!file.exists(file.path(model_dir, paste0('model_ranger_', i, '.rds')))) {
      training_parameters <- trainControl(summaryFunction = twoClassSummary,
                                          classProbs = TRUE,
                                          verboseIter  = TRUE,
                                          allowParallel = TRUE,
                                          savePredictions = TRUE)
      tuning_grid <- expand.grid(
        .mtry = tuned_ranger$recommended.pars$mtry,
        .splitrule = "gini",
        .min.node.size = tuned_ranger$recommended.pars$min.node.size)
      
      model_ranger <- caret::train(ignition ~ .,
                                   data = train,
                                   method = "ranger",
                                   num.threads = parallel::detectCores(),
                                   metric = 'ROC',
                                   weights = model_weights,
                                   trControl = training_parameters,
                                   tuneGrid = tuning_grid,
                                   num.trees = 2500,
                                   importance = 'permutation')
      write_rds(model_ranger, file.path(model_dir, paste0('model_ranger_', i, '.rds'))) 
      system(paste0('aws s3 sync ', model_dir, ' ', s3_proc_models))
    } else {
      model_ranger <- read_rds(file.path(model_dir, paste0('model_ranger_', i, '.rds')))
    }
    
    if(!file.exists(file.path(model_dir, paste0('importance_pval_', i, '.rds')))) {
      print(paste0('Computing variable importance p-value for ', i))
      model_importance_pvalues <- importance_pvalues(model_ranger$finalModel, 
                                                     method = "altmann", formula = ignition ~ ., data = train)
      write_rds(model_importance_pvalues, file.path(model_dir, paste0('importance_pval_', i, '.rds'))) 
      system(paste0('aws s3 sync ', model_dir, ' ', s3_proc_models))
    }
    
    if(!file.exists(file.path(model_dir, paste0('confusion_ranger_', i, '.rds')))) {
      # predict the outcome on a test set
      ranger_pred <- predict(model_ranger, test)
      write_rds(predict_rpart, file.path(model_dir, 'prediction_ranger', i, '.rds'))
      # compare predicted outcome and true outcome
      confusion_matrix <- confusionMatrix(ranger_pred, test$ignition)
      
      write_rds(confusion_matrix, file.path(model_dir, paste0('confusion_ranger_', i, '.rds'))) 
      system(paste0('aws s3 sync ', model_dir, ' ', s3_proc_models))
    }
  }
} 
