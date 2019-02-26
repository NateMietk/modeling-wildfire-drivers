
file <- list.files(model_dir, pattern = 'importance', full.names = TRUE)
impt_pval <- read_rds(file) %>%
  as.data.frame() %>%
  rownames_to_column('lvl2_ecoregion')

impt_pval %>%
  filter(pvalue <= 0.05) %>%
  arrange(desc(pvalue)) %>%
  ggplot(aes(x=reorder(lvl2_ecoregion,pvalue), y=pvalue)) + 
  geom_bar(stat="identity", position="dodge", width = 0.01, fill = 'black') + 
  geom_point() +
  coord_flip() +
  ylab("Variable Importance") +
  xlab("") +
  ggtitle("Information Value Summary") +
  guides(fill=F) +
  theme_pub()


# Plot the ROC evaluation of the model 
mod_files <- list.files(model_dir, pattern = 'model', full.names = TRUE)
pval_files <- list.files(model_dir, pattern = 'importance', full.names = TRUE)

importance <- do.call(rbind,lapply(mod_files,
                             function(x) {
                               ecoreg_name <- unlist(strsplit(x, '_|\\.'))[4] %>%
                                 gsub('-', ' ', .) 
                               df <- varImp(read_rds(x))$importance %>%
                                 rownames_to_column('variables') %>%
                                 mutate(lvl2_ecoregion = ecoreg_name) %>%
                                 as_tibble()
                               return(df)}))
pval <- do.call(rbind,lapply(pval_files,
                             function(x) {
                               ecoreg_name <- unlist(strsplit(x, '_|\\.'))[4] %>%
                                 gsub('-', ' ', .) 
                               df <- read_rds(x) %>%
                                 as.data.frame() %>%
                                 rownames_to_column('variables') %>%
                                 mutate(lvl2_ecoregion = ecoreg_name) %>%
                                 as_tibble()
                               return(df)
                             }))
importance_pval <- importance %>%
  left_join(., pval , by = c('lvl2_ecoregion', 'variables'))
preds<- do.call(rbind,lapply(files,
              function(x) {
                ecoreg_name <- unlist(strsplit(x, '_|\\.'))[4] %>%
                  gsub('-', ' ', .) 
                df <- read_rds(x)$pred %>%
                  mutate(lvl2_ecoregion = ecoreg_name) %>%
                  as_tibble()
                return(df)}))

model_ranger_roc <- preds %>%
  mutate(bool = ifelse(obs == 'Human', 1, 0)) %>%
  ggplot(aes(m = Human, d = bool)) + 
  geom_roc(n.cuts=0) + 
  coord_equal() +
  style_roc() +
  facet_wrap(~ lvl2_ecoregion) 
model_ranger_roc <- model_ranger_roc +
  annotate("text", x=0.75, y=0.25, label=paste("AUC =", round(calc_auc(model_ranger_roc)$AUC, 4)))

top_15_sig_importance <- importance_pval %>% 
  filter(pvalue <= 0.05) %>%
  group_by(lvl2_ecoregion) %>%
  top_n(n = 15, wt = Overall) 

top_15_sig_importance %>% 
  ggplot(aes(x = reorder_within(variables, Overall, lvl2_ecoregion), y = Overall)) + 
  facet_wrap(~ lvl2_ecoregion, scales = 'free_y',
             nrow = 3) +
  geom_bar(stat="identity", position="dodge", width = 0.01, fill = 'black') + 
  geom_point() +
  coord_flip() +
  scale_x_reordered() +
  ylab("Variable Importance") +
  xlab("") +
  guides(fill=F) +
  theme_pub()
