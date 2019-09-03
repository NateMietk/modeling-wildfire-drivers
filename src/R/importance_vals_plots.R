
mod_files <- list.files(janitza_dir, pattern = 'model', full.names = TRUE)
conf_files <- list.files(janitza_dir, pattern = 'confusion', full.names = TRUE)
pred_files <- list.files(janitza_dir, pattern = 'prediction', full.names = TRUE)
pval_files <- list.files(janitza_dir, pattern = 'pval', full.names = TRUE)

confusion_matrix <- do.call(rbind,lapply(conf_files,
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

importance <- do.call(rbind,lapply(mod_files,
                                   function(x) {
                                     ecoreg_name <- unlist(strsplit(x, '_|\\.'))[4] %>%
                                       gsub('-', ' ', .) 
                                     df <- varImp(read_rds(x))$importance %>%
                                       rownames_to_column('variables') %>%
                                       mutate(na_l2name = as.factor(ecoreg_name)) %>%
                                       as_tibble()
                                     return(df)}))
pval <- do.call(rbind,lapply(pval_files,
                             function(x) {
                               ecoreg_name <- unlist(strsplit(x, '_|\\.'))[4] %>%
                                 gsub('-', ' ', .) 
                               df <- readr::read_rds(x) %>%
                                 as.data.frame() %>%
                                 rownames_to_column('variables') %>%
                                 mutate(na_l2name = as.factor(ecoreg_name)) %>%
                                 as_tibble()
                               return(df)
                             }))

importance_pval <- importance %>%
  left_join(., pval , by = c('na_l2name', 'variables')) %>%
  na.omit()

preds <- do.call(rbind,lapply(pred_files,
                              function(x) {
                                ecoreg_name <- unlist(strsplit(x, '_|\\.'))[4] %>%
                                  gsub('-', ' ', .) 
                                df <- read_rds(x) %>%
                                  mutate(na_l2name = as.factor(ecoreg_name)) %>%
                                  as_tibble()
                                return(df)}))

# Plot the ROC evaluation of the model 
model_ranger_roc_h <- preds %>%
  ggplot(aes(m = Human, d = ifelse(obs == 'Human', 1, 0))) + 
  geom_roc(n.cuts=0) + 
  facet_wrap(~ na_l2name) 

roc_values <- calc_auc(model_ranger_roc_h)

model_ranger_roc <- preds %>%
  ggplot() + 
  geom_roc(aes(m = Human, d = ifelse(obs == 'Human', 1, 0)), n.cuts=0, color = 'red') + 
  # geom_roc(aes(m = Lightning, d = ifelse(obs == 'Lightning', 1, 0)), n.cuts=0, color = 'royalblue') + 
  coord_equal() +
  style_roc() +
  theme(strip.background = element_blank(),
        strip.text.x = element_text(size = 12)) +
  facet_wrap(~ na_l2name, labeller = label_wrap_gen(width = 15), ncol = 7) +
  annotate("text", x=0.75, y=0.35, label=paste("AUC =", round(roc_values$AUC, 3)))
model_ranger_roc

auc_values <- unique(ecoregions_l3$na_l2name) %>%
  as.tibble() %>%
  arrange(value) %>%
  select(na_l2name = value) %>%
  mutate(PANEL = as.factor(row_number())) %>%
  left_join(., roc_values, by = 'PANEL') %>%
  dplyr::select(na_l2name, AUC)

ecoregion_auc <- ecoregions_l3 %>%
  sf::st_buffer(0) %>%
  group_by(na_l2name) %>%
  summarise(geometry = sf::st_union(geom)) %>%
  ungroup() %>%
  left_join(., auc_values, by = 'na_l2name')

ecoregion_auc %>%
  sf::st_simplify(., preserveTopology = TRUE, dTolerance = 1000)  %>%
  ggplot() +
  geom_sf(aes(fill = AUC)) +
  scale_fill_viridis_c(option = "plasma")

regions <- as.data.frame(ecoregions_l3) %>%
  distinct(na_l2name, region) %>%
  as.tibble() 

top_15_sig_importance <- importance_pval %>% 
  filter(variables != 'row_id') %>%
  filter(pvalue <= 0.1) %>%
  group_by(na_l2name) %>%
  top_n(n = 10, wt = Overall) %>%
  left_join(., regions, by = 'na_l2name')

top_15_sig_importance %>% 
  filter(region == 'Central') %>%
  ggplot(aes(x = reorder_within(variables, Overall, na_l2name), y = Overall)) + 
  facet_wrap(~ na_l2name, labeller = label_wrap_gen(width = 15), nrow = 3, scales = 'free_y') +
  geom_bar(stat="identity", position="dodge", width = 0.05, fill = 'black') + 
  geom_point(size = 3) +
  coord_flip() +
  scale_x_reordered() +
  ylab("Variable Importance") +
  xlab("") +
  guides(fill=F) +
  theme_pub() +
  theme(strip.background = element_blank(),
        strip.text.x = element_text(size = 16))

ecoregion_importance <- ecoregions_l3 %>%
  left_join(., top_15_sig_importance, by = 'na_l2name')




