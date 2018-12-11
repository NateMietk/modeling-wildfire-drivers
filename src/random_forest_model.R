

fpa_all_vars <- read_rds(file.path(extraction_dir, 'fpa_all_vars.rds')) %>%
  dplyr::select(-class, -owner_code, -fpa_id, -stat_cause_descr) %>%
  na.omit() # random forests cannot deal with NA values

how_unbalanced <- fpa_all_vars %>%
  group_by(ignition) %>%
  summarise(counts = n()) %>%
  mutate(pct_unbalanced = counts/sum(counts))

# https://stackoverflow.com/questions/8704681/random-forest-with-classes-that-are-very-unbalanced
# strata refers to which feature to do stratified sampling on.
# sampsize refers to the size of the bootstrap samples to be taken from each class. These samples will be taken as input
# for each tree. 
# Ranger random forests (https://www.rdocumentation.org/packages/ranger/versions/0.10.1/topics/ranger) 
model_weights <- model_weights <- ifelse(fpa_all_vars$ignition == "Human",
                                         (1/table(fpa_all_vars$ignition)[1]) * 0.641,
                                         (1/table(fpa_all_vars$ignition)[2]) * 0.359)

tuning_grid <- expand.grid(
  .mtry = 2:4,
  .splitrule = "gini",
  .min.node.size = c(10, 20))

training_parameters <- trainControl(method="repeatedcv",
                                    number = 10,
                                    repeats = 2,
                                    classProbs = TRUE,
                                    verboseIter  = TRUE,
                                    allowParallel = TRUE)

fit_results <- train(ignition ~ .,
                     data = fpa_all_vars,
                     method = "ranger",
                     num.threads = 4, # for parallel processing
                     weights = model_weights,
                     trControl = training_parameters,
                     tuneGrid = tuning_grid,
                     num.trees=1000,
                     importance = 'permutation')

# How does the fitted model look?
trellis.par.set(caretTheme())
plot(fit_results)

trellis.par.set(caretTheme())
plot(fit_results, metric = "Kappa")








