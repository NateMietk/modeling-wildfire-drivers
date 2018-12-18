# Function
dummify_vars <- function(df) {
  dummified <- df %>%
    dplyr::select(-ignition) %>%
    dummyVars(" ~ .", data = ., sep = '_', fullRank = TRUE)
  
  dummified <- data.frame(predict(dummified, newdata = df)) %>%
    as_tibble()
  
  dummified <- df %>% 
    dplyr::select(ignition, row_id) %>%
    left_join(., dummified, by = 'row_id') %>%
    dplyr::select(ignition, everything(), -row_id)
  return(dummified)
}

# Prep data frame
fpa_all_vars <- read_rds(file.path(extraction_dir, 'fpa_all_vars.rds')) %>%
  dplyr::select(-class, -owner_code, -stat_cause_descr) %>%
  na.omit() %>% # random forests cannot deal with NA values
  mutate(row_id = row_number()) %>%
  droplevels() %>%
  dplyr::select(-fpa_id)

how_unbalanced <- fpa_all_vars %>%
  group_by(ignition) %>%
  summarise(counts = n()) %>%
  mutate(pct_unbalanced = counts/sum(counts)) %>%
  dplyr::select(-counts) %>%
  spread(ignition, pct_unbalanced)

# Data partitioning
set.seed(224)

# Create training data - 60% 
train <- fpa_all_vars %>% 
  dplyr::sample_frac(0.6)

# Create testing data - 20%
test <- fpa_all_vars %>% 
  anti_join(., train, by = 'row_id') %>%
  dplyr::sample_frac(0.5)

# Create validation data - 20%
validation <- fpa_all_vars %>% 
  anti_join(., train, by = 'row_id') %>%
  anti_join(., test, by = 'row_id') %>%
  dplyr::select(ignition, everything(), -row_id)

# Remove row_id column and reorganize
# Random forests can not handle categorical data, so we need to dummify the factors...
dummify_train <- train %>%
  dplyr::select(ignition, everything(), -row_id) %>%
  dummify_vars(.)

dummify_test <- test %>%
  dplyr::select(ignition, everything(), -row_id) %>%
  dummify_vars(.)

dummify_validation <- dummify_vars(validation)

# https://stackoverflow.com/questions/8704681/random-forest-with-classes-that-are-very-unbalanced
# Ranger random forests (https://www.rdocumentation.org/packages/ranger/versions/0.10.1/topics/ranger) 

# Create the weights file for unbalanced data 
# Penalize the model for predicting Human because it is 2/3 of the data set
model_weights <- ifelse(dummify_train$ignition == "Human",
                        (1/table(dummify_train$ignition)[1]) * how_unbalanced$Human,
                        (1/table(dummify_train$ignition)[2]) * how_unbalanced$Lightning)

# Set the mtry and min nodes to iterate through.
mtry_options <- unique(seq(4:12))
min_node_options <- unique(c(5, 10, 20))


for(mt in mtry_options) {
  for(mn in min_node_options) {
    set.seed(23587)
    
    tuning_grid <- expand.grid(
      .mtry = mt,
      .splitrule = "gini",
      .min.node.size = mn)
    
    training_parameters <- trainControl(method="oob",
                                        verboseIter  = TRUE,
                                        allowParallel = TRUE)
    
    # This model must be run on an EC2 instance r5d.4xlarge
    ranger_model <- train(ignition ~ .,
                          data = dummify_train,
                          method = "ranger",
                          num.threads = parallel::detectCores(), # for parallel processing
                          weights = model_weights,
                          trControl = training_parameters,
                          tuneGrid = tuning_grid,
                          num.trees = 1000,
                          importance = 'permutation')
    write_rds(ranger_model, file.path(model_dir, paste0('ranger_oob_', trees, 'trees_', mt, 'mtry_', mn, 'nodes.rds')))
  }
}

many_models <- list.files(model_dir, full.names = TRUE)

stats_df <- NULL
for(j in many_models) {
  stats <- read_rds(j)
  stats_df[[j]] <- stats$results
}
stats_df <- do.call('rbind', stats_df)

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





