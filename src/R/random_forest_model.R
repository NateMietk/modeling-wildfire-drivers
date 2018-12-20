
# Prep data frame
fpa_all_vars <- read_rds(file.path(extraction_dir, 'fpa_all_vars.rds')) %>%
  dplyr::select(-class, -stat_cause_descr) %>%
  mutate_if(is.numeric, replace_na, 0) %>% # random forests cannot deal with NA values
  mutate(row_id = row_number()) %>%
  na.omit() %>%
  droplevels() %>%
  dplyr::select(-fpa_id, -ESP_Name) 

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
# Penalize the model for predicting Human because it is 2/3 of the data set
model_weights <- ifelse(train$ignition == "Human",
                        how_unbalanced$Human, how_unbalanced$Lightning)

# Tuning process for the lower 48 states
if(!file.exists(file.path(model_dir, 'tuned_ranger_us.rds'))) {
  set.seed(432)
  mlr_tasked = makeClassifTask(data = train, target = "ignition", weights = model_weights)
  
  tuned_ranger_us <- tuneRanger(mlr_tasked, measure = list(multiclass.brier), num.trees = 2500,
                                num.threads = parallel::detectCores(), iters = 100, 
                                build.final.model = FALSE)
  tuned_ranger_us <- write_rds(tuned_ranger_us, file.path(model_dir, 'tuned_ranger_us.rds'))
} else {
  tuned_ranger_us <- read_rds(file.path(model_dir, 'tuned_ranger_us.rds'))
}

# Model with the tuned hyperparameter from tuneRanger
# Model uses cross validation with 10 folds
if(!file.exists(file.path(model_dir, 'model_ranger_us.rds'))) {
  training_parameters <- trainControl(method="cv",
                                      number = 10,
                                      summaryFunction = twoClassSummary,
                                      classProbs = TRUE,
                                      verboseIter  = TRUE,
                                      allowParallel = TRUE,
                                      savePredictions = TRUE)
  
  # This model must be run on an EC2 instance r5d.4xlarge
  tuning_grid <- expand.grid(
    .mtry = tuned_ranger_us$recommended.pars$mtry,
    .splitrule = c("gini", 'extratrees'),
    .min.node.size = tuned_ranger_us$recommended.pars$min.node.size)
  
  model_ranger_us <- caret::train(ignition ~ .,
                                  data = train,
                                  method = "ranger",
                                  num.threads = parallel::detectCores(), # for parallel processing
                                  preProc=c("center", "scale"), 
                                  weights = model_weights,
                                  trControl = training_parameters,
                                  tuneGrid = tuning_grid,
                                  num.trees = 2500,
                                  importance = 'permutation')
  write_rds(model_ranger_us, file.path(model_dir, 'model_ranger_us.rds')) 
} else {
  model_ranger_us <- read_rds(file.path(model_dir, 'model_ranger_us.rds'))
}

# Plot the ROC evaluation of the model 
selectedIndices <- model_ranger_us$pred$mtry == tuned_ranger_us$recommended.pars$mtry

model_ranger_roc <- model_ranger_us$pred[selectedIndices, ] %>%
  mutate(bool = ifelse(obs == 'Human', 1, 0)) %>%
  ggplot(aes(m = Human, d = bool)) + 
  geom_roc(n.cuts=0) + 
  coord_equal() +
  style_roc() +
  annotate("text", x=0.75, y=0.25, label=paste("AUC =", round((calc_auc(g))$AUC, 4)))


plot(varImp(ranger_model))

nrow(varImp(ranger_model)$importance)

varImp(ranger_model)$importance %>% 
  as.data.frame() %>%
  rownames_to_column() %>%
  arrange(desc(Overall)) %>%
  mutate(variable = forcats::fct_inorder(rowname )) %>%
  slice(1:15) %>%
  ggplot(aes(x=reorder(variable,Overall), y=Overall,fill=Overall)) + 
  geom_bar(stat="identity", position="dodge")+ coord_flip()+
  ylab("Variable Importance")+
  xlab("")+
  ggtitle("Information Value Summary")+
  guides(fill=F)+
  scale_fill_gradient(low="red", high="blue")


# How does the fitted model look?
trellis.par.set(caretTheme())
plot(ranger_model)

trellis.par.set(caretTheme())
plot(ranger_model, metric = "Kappa")

# predict the outcome on a test set
ranger_pred <- predict(ranger_model, test)
# compare predicted outcome and true outcome
confusionMatrix(ranger_pred, test$ignition)

# Evaluate accuracy via ROC curves
library(plotROC)
selectedIndices <- ranger_model$pred$mtry == 29
g <- ggplot(ranger_model$pred[selectedIndices, ],
            aes(m = M, d = factor(obs, levels = c("R", "M")))) + 
  geom_roc(n.cuts=0) + 
  coord_equal() +
  style_roc()

g + annotate("text", x=0.75, y=0.25, label=paste("AUC =", round((calc_auc(g))$AUC, 4)))

result.coords <- coords(result.roc, "best", best.method="closest.topleft", ret=c("threshold", "accuracy"))
print(result.coords)#to get threshold and accuracy
