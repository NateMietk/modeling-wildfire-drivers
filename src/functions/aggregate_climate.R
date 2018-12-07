aggregate_climate <- function(df, do_sums = TRUE, do_sums_only = FALSE){
  month_first3 <- 'lag_0$|lag_1$|lag_2$|lag_3'
  month_first6 <- paste0(month_first3,'|lag_4|lag_5|lag_6')
  month_first12 <- paste0(month_first6,'|lag_7|lag_8|lag_9|lag_10|lag_11|lag_12')
  month_first18 <- paste0(month_first12,'|lag_13|lag_14|lag_15|lag_16|lag_17|lag_18')

  file_split <- df %>%
    dplyr::select_(., ~matches('lag_0$')) %>%
    names() %>%
    strsplit(split = "_") %>%
    unlist
  var <- file_split[1]
  
  var0 <- paste0(var, '_mean_lag_0')
  var1 <- paste0(var, '_mean_3_month')
  var3 <- paste0(var, '_mean_6_month')
  var5 <- paste0(var, '_mean_12_month')
  
  if(do_sums == TRUE) {
    
    var2 <- paste0(var, '_min_3_month')
    var4 <- paste0(var, '_min_6_month')
    var6 <- paste0(var, '_min_12_month')
    
    df_out <- as_tibble(as.data.frame(df)) %>% 
      mutate(!!ensym(var1) := dplyr::select_(., ~matches(month_first3)) %>% rowMeans(., na.rm=TRUE),
             !!ensym(var2) := dplyr::select_(., ~matches(month_first3)) %>% rpgm::rowMins(.),
             !!ensym(var3) := dplyr::select_(., ~matches(month_first6)) %>% rowMeans(., na.rm=TRUE),
             !!ensym(var4) := dplyr::select_(., ~matches(month_first6)) %>% rpgm::rowMins(.),
             !!ensym(var5) := dplyr::select_(., ~matches(month_first12)) %>% rowMeans(., na.rm=TRUE),
             !!ensym(var6) := dplyr::select_(., ~matches(month_first12)) %>% rpgm::rowMins(.),
             fpa_id = as.data.frame(df)$FPA_ID) %>%
      dplyr::select(fpa_id, !!ensym(var0), !!ensym(var1), !!ensym(var3), !!ensym(var5),
                    !!ensym(var2), !!ensym(var4), !!ensym(var6))
  } 
  if(do_sums == FALSE & do_sums_only == FALSE) {

    var2 <- paste0(var, '_sum_3_month')
    var4 <- paste0(var, '_sum_6_month')
    var6 <- paste0(var, '_sum_12_month')
    var7 <- paste0(var, '_sum_18_month')
    
    df_out <- as_tibble(as.data.frame(df)) %>% 
      mutate(!!ensym(var1) := dplyr::select_(., ~matches(month_first3)) %>% rowMeans(., na.rm=TRUE),
             !!ensym(var2) := dplyr::select_(., ~matches(month_first3)) %>% rowSums(.),
             !!ensym(var3) := dplyr::select_(., ~matches(month_first6)) %>% rowMeans(., na.rm=TRUE),
             !!ensym(var4) := dplyr::select_(., ~matches(month_first6)) %>% rowSums(.),
             !!ensym(var5) := dplyr::select_(., ~matches(month_first12)) %>% rowMeans(., na.rm=TRUE),
             !!ensym(var6) := dplyr::select_(., ~matches(month_first12)) %>% rowSums(.),
             !!ensym(var7) := dplyr::select_(., ~matches(month_first18)) %>% rowSums(.),
             fpa_id = as.data.frame(df)$FPA_ID) %>%
      dplyr::select(fpa_id, !!ensym(var0), !!ensym(var1), !!ensym(var3), !!ensym(var5),
                    !!ensym(var2), !!ensym(var4), !!ensym(var6), !!ensym(var7))
  }
  if(do_sums_only == TRUE) {
    
    var0p <- paste0(var, '_lag_0')
    var0 <- paste0(var, '_numdays95th_lag_0')
    var2 <- paste0(var, '_numdays95th_sum_3_month')
    var4 <- paste0(var, '_numdays95th_sum_6_month')
    var6 <- paste0(var, '_numdays95th_sum_12_month')
    var7 <- paste0(var, '_numdays95th_sum_18_month')
    
    df_out <- as_tibble(as.data.frame(df)) %>% 
      mutate(!!ensym(var2) := dplyr::select_(., ~matches(month_first3)) %>% rowSums(.),
             !!ensym(var4) := dplyr::select_(., ~matches(month_first6)) %>% rowSums(.),
             !!ensym(var6) := dplyr::select_(., ~matches(month_first12)) %>% rowSums(.),
             !!ensym(var7) := dplyr::select_(., ~matches(month_first18)) %>% rowSums(.),
             !!ensym(var0) := !!ensym(var0p),
             fpa_id = as.data.frame(df)$FPA_ID) %>%
      dplyr::select(fpa_id, !!ensym(var0), !!ensym(var2), !!ensym(var4), !!ensym(var6), !!ensym(var7))
  }
  
  return(df_out)
}
