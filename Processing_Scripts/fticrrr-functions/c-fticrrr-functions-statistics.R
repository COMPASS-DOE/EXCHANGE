# pca functions -----------------------------------------------------------
library(ggbiplot)
library(vegan)
library(patchwork)

fit_pca_function = function(dat){
  relabund_pca =
    dat %>% 
    ungroup %>% 
    dplyr::select(-c(abund, total)) %>% 
    spread(Class, relabund) %>% 
    replace(.,is.na(.),0)  %>% 
    dplyr::select(-1)
  
  num = 
    relabund_pca %>% 
    # dplyr::select(c(aliphatic, aromatic, `condensed aromatic`, `unsaturated/lignin`))
    dplyr::select(c(aliphatic:`unsaturated/lignin`))
  
  
  grp = 
    relabund_pca %>% 
    #dplyr::select(-c(aliphatic, aromatic, `condensed aromatic`, `unsaturated/lignin`)) %>% 
    dplyr::select(-c(aliphatic:`unsaturated/lignin`)) %>% 
    dplyr::mutate(row = row_number())
  
  pca_int = prcomp(num, scale. = T)
  
  list(num = num,
       grp = grp,
       pca_int = pca_int)
}

compute_fticr_pca = function(relabund_cores){
  ## PCA input files ----
  
  pca_overall = fit_pca_function(relabund_cores)
  pca_highCUE = fit_pca_function(relabund_cores %>% filter(A_priori_CUE == "High"))
  pca_lowCUE = fit_pca_function(relabund_cores %>% filter(A_priori_CUE == "Low"))
  
  
  ## PCA plots overall ----
  (  gg_pca_overall = 
       ggbiplot(pca_overall$pca_int, obs.scale = 1, var.scale = 1,
                groups = as.character(pca_overall$grp$Incubation_Temp), 
                ellipse = TRUE, circle = FALSE, var.axes = TRUE, alpha = 0) +
       geom_point(size=2,stroke=1, alpha = 1,
                  aes(shape = pca_overall$grp$A_priori_CUE,
                      color = groups))+
       scale_shape_manual(values = c(19, 1), name = "")+
       #scale_color_manual(values = c("red", "blue"), name = "")+
       #scale_fill_manual(values = c("red", "blue"), name = "")+
       xlim(-4,4)+
       ylim(-5,3.5)+
       labs(shape="",
            title = "all samples",
            subtitle = "incubation temperature, shapes = CUE")+
       theme_kp()+
       NULL)
  
  
  ## PCA plots highCUE ----
  (  gg_pca_highCUE = 
       ggbiplot(pca_highCUE$pca_int, obs.scale = 1, var.scale = 1,
                groups = as.character(pca_highCUE$grp$Incubation_Temp), 
                ellipse = TRUE, circle = FALSE, var.axes = TRUE, alpha = 0) +
       geom_point(size=2,stroke=1, alpha = 1,
                  aes(#shape = pca_highCUE$grp$A_priori_CUE,
                    color = groups))+
       scale_shape_manual(values = c(19, 1), name = "")+
       #scale_color_manual(values = c("red", "blue"), name = "")+
       #scale_fill_manual(values = c("red", "blue"), name = "")+
       xlim(-4,4)+
       ylim(-5,3.5)+
       labs(shape="",
            title = "high CUE")+
       theme_kp()+
       NULL)
  
  ## PCA plots lowCUE ----
  (  gg_pca_lowCUE = 
       ggbiplot(pca_lowCUE$pca_int, obs.scale = 1, var.scale = 1,
                groups = as.character(pca_lowCUE$grp$Incubation_Temp), 
                ellipse = TRUE, circle = FALSE, var.axes = TRUE, alpha = 0) +
       geom_point(size=2,stroke=1, alpha = 1,
                  aes(#shape = pca_lowCUE$grp$A_priori_CUE,
                    color = groups))+
       scale_shape_manual(values = c(19, 1), name = "")+
       #scale_color_manual(values = c("red", "blue"), name = "")+
       #scale_fill_manual(values = c("red", "blue"), name = "")+
       xlim(-4,4)+
       ylim(-5,3.5)+
       labs(shape="",
            title = "low CUE")+
       theme_kp()+
       NULL)
  
  list(gg_pca_overall = gg_pca_overall,
       gg_pca_highCUE = gg_pca_highCUE,
       gg_pca_lowCUE = gg_pca_lowCUE
  )
}

# permanova -----------------------------------------------------------

compute_permanova = function(relabund_cores){
  relabund_wide = 
    relabund_cores %>% 
    ungroup() %>% 
    mutate(Class = factor(Class, 
                          levels = c("aliphatic", "unsaturated/lignin", 
                                     "aromatic", "condensed aromatic"))) %>% 
    dplyr::select(-c(abund, total)) %>% 
    spread(Class, relabund) %>% 
    replace(is.na(.), 0)
  
  permanova_fticr_all = 
    adonis(relabund_wide %>% dplyr::select(where(is.numeric)) ~ 
             (Incubation_Temp + A_priori_CUE)^2, 
           data = relabund_wide)
  broom::tidy(permanova_fticr_all$aov.tab)
}

##  variables = c("sat_level", "treatment")
##  indepvar = paste(variables, collapse = " + ")
##  compute_permanova(indepvar)



# relabund anova ----------------------------------------------------------
compute_relabund_anova = function(relabund_cores){
  fit_anova = function(dat){
    l = lm(relabund ~ (length + saturation + drying)^2, data = dat, singular.ok = TRUE)
    a = car::Anova(l)
    broom::tidy((a)) %>% filter(term != "Residuals") %>% 
      mutate(`p.value` = round(`p.value`,4))
    
  }
  
  x = relabund_cores %>% 
    filter(Class != "other" & saturation != "timezero") %>% 
    group_by(Class) %>% 
    do(fit_anova(.)) %>% 
    dplyr::select(Class, `p.value`, term) %>% 
    pivot_wider(names_from = "Class", values_from = "p.value")
  
  
  
}