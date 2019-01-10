
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
  filter(state %in% c('NY', 'NH', 'VT', 'ME', 'MA', 'CT','RI')) %>%
  dplyr::select(-fpa_id, -ESP_Name, -region, -na_namel1) %>%
  droplevels() 

# Data partitioning
set.seed(224)
# Create training data - 60% 
train <- fpa_all_vars %>% 
  dplyr::sample_frac(0.6)

# Create testing data - 40%
test <- fpa_all_vars %>% 
  anti_join(., train, by = 'row_id') 

how_unbalanced <- train %>%
  group_by(ignition) %>%
  summarise(counts = n()) %>%
  mutate(pct_unbalanced = counts/sum(counts)) %>%
  dplyr::select(-counts) %>%
  spread(ignition, pct_unbalanced)

# https://stackoverflow.com/questions/8704681/random-forest-with-classes-that-are-very-unbalanced
# Ranger random forests (https://www.rdocumentation.org/packages/ranger/versions/0.10.1/topics/ranger) 

# Create the weights file for unbalanced data 
model_weights <- ifelse(train$ignition == "Human",
                        how_unbalanced$Human, how_unbalanced$Lightning)

# Tuning process for the lower 48 states
if(!file.exists(file.path(model_dir, 'tuned_ranger_ne.rds'))) {
  set.seed(432)
  mlr_tasked = makeClassifTask(data = train, target = "ignition", weights = model_weights)
  
  tuned_ranger_ne <- tuneRanger(mlr_tasked, measure = list(multiclass.brier), num.trees = 1000,
                                num.threads = parallel::detectCores(), iters = 10, 
                                build.final.model = FALSE)
  tuned_ranger_ne <- write_rds(tuned_ranger_ne, file.path(model_dir, 'tuned_ranger_ne.rds'))
} else {
  tuned_ranger_ne <- read_rds(file.path(model_dir, 'tuned_ranger_ne.rds'))
}

# Model with the tuned hyperparameter from tuneRanger
# Model uses cross validation with 10 folds
if(!file.exists(file.path(model_dir, 'model_ranger_ne.rds'))) {
  training_parameters <- trainControl(#method="oob",
                                      # number = 1,
                                      summaryFunction = twoClassSummary,
                                      classProbs = TRUE,
                                      verboseIter  = TRUE,
                                      allowParallel = TRUE,
                                      savePredictions = TRUE)
  
  # This model must be run on an EC2 instance r5d.4xlarge
  #
  tuning_grid <- expand.grid(
    .mtry = tuned_ranger_ne$recommended.pars$mtry,
    .splitrule = "gini", #, 'extratrees'),
    .min.node.size = tuned_ranger_ne$recommended.pars$min.node.size)
  
  model_ranger_ne <- caret::train(ignition ~ .,
                                  data = train,
                                  method = "ranger",
                                  num.threads = parallel::detectCores(), # for parallel processing
                                  metric = 'ROC',
                                  weights = model_weights,
                                  trControl = training_parameters,
                                  tuneGrid = tuning_grid,
                                  num.trees = 1000,
                                  importance = 'permutation')
  write_rds(model_ranger_ne, file.path(model_dir, 'model_ranger_ne.rds')) 
} else {
  model_ranger_ne <- read_rds(file.path(model_dir, 'model_ranger_ne.rds'))
}

# Calculate the p value for the variable imporances 
importance_pvalues(model_ranger_ne$finalModel, method = "altmann", formula = ignition ~ ., data = train)

# Plot the ROC evaluation of the model 
selectedIndices <- model_ranger_ne$pred$mtry == tuned_ranger_ne$recommended.pars$mtry

model_ranger_roc <- model_ranger_ne$pred[selectedIndices, ] %>%
  mutate(bool = ifelse(obs == 'Human', 1, 0)) %>%
  ggplot(aes(m = Human, d = bool)) + 
  geom_roc(n.cuts=0) + 
  coord_equal() +
  style_roc() 
model_ranger_roc <- model_ranger_roc +
  annotate("text", x=0.75, y=0.25, label=paste("AUC =", round((calc_auc(model_ranger_roc))$AUC, 4)))
model_ranger_roc

plot(varImp(model_ranger_ne))

varImp(model_ranger_ne)$importance %>% 
  as.data.frame() %>%
  rownames_to_column() %>%
  arrange(desc(Overall)) %>%
  mutate(variable = forcats::fct_inorder(rowname )) %>%
  slice(1:15) %>%
  ggplot(aes(x=reorder(variable,Overall), y=Overall)) + 
  geom_bar(stat="identity", position="dodge", width = 0.01, fill = 'black') + 
  geom_point() +
  coord_flip() +
  ylab("Variable Importance") +
  xlab("") +
  ggtitle("Information Value Summary") +
  guides(fill=F) +
  theme_pub()

# predict the outcome on a test set
ranger_pred <- predict(model_ranger_ne, test)
# compare predicted outcome and true outcome
confusionMatrix(ranger_pred, test$ignition)

# plot rpart of top 15 vars
vars <- varImp(model_ranger_ne)$importance %>% 
  as.data.frame() %>%
  rownames_to_column() %>%
  arrange(desc(Overall)) %>%
  mutate(variable = forcats::fct_inorder(rowname )) %>%
  slice(1:10) %>%
  dplyr::select(variable) %>%
  pull(., variable) %>%
  droplevels()
test <- paste(vars, collapse = ' ')

model_rpart_us <- caret::train(ignition ~ tmmx_mean_lag_0+vector_primary_rds_distance+vector_tertiary_rds_distance+vector_secondary_rds_distance+vector_railroad_distance+
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
