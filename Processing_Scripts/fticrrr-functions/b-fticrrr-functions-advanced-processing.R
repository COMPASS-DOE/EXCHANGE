## EXCHANGE-FTICR
##
## This is a data processing script for EXCHANGE, a sub-project of the DOE-funded 
## COMPASS project (https://compass.pnnl.gov/). 
##
## This script contains functions for additional processing and analysis of the cleaned FTICR data.
## 
## Created: November 2022
## Kaizad F. Patel

## These functions have been modified from the `fticrrr` package and workflow: 
## https://github.com/kaizadp/fticrrr

################################################## #

# `fticrrr-functions-advanced-processing.R`

################################################## #

## the functions in this script include:
## (a) processing FTICR data
## -- (a1)  convert intensities to presence/absence (highly recommended!)
## -- (a2)  make longform
## (b) analyzing the FTICR data
## -- (b1) Van Krevelen plots
## -- (b2) compute relative abundance per sample
## -- (b3) PCA and PERMANOVA

# INSTRUCTIONS:
## These functions use, as input, the data generated from `Processing_Scripts/water-fticr`

################################################## #
################################################## #


# Setup -------------------------------------------------------------------

library(tidyverse) # for general processing
library(PNWColors) # color palettes, compatible with ggplot2
library(soilpalettes) # color palettes, compatible with ggplot2
library(ggbiplot) # for PCA biplots
# devtools::install_github("miraKlein/ggbiplot")
library(vegan) # for PERMANOVA


#
# Processing the data -----------------------------------------------------

compute_presence = function(dat){
  dat %>% 
    pivot_longer(-c("Mass", "formula"), values_to = "presence", names_to = "sample_name") %>% 
    # convert intensities to presence==1/absence==0  
    dplyr::mutate(presence = if_else(presence>0,1,0)) %>% 
    # keep only peaks present
    filter(presence>0)
}

fticr_data_presence = 
  fticr_data %>% 
  compute_presence()


sample_key = 
  read.csv("Data/EC1_Metadata_KitLevel.csv") %>% 
  dplyr::select(kit_id, site_name, region) 

fticr_data_longform = 
  fticr_data_presence %>% 
  distinct(formula, sample_name, presence) %>% 
  rename(kit_id = sample_name) %>% 
  mutate(kit_id = str_remove(kit_id, "b")) %>% 
  left_join(sample_key)

#
# Graphs - Van Krevelen ---------------------------------------------------

gg_vankrev <- function(data,mapping){
  ggplot(data,mapping) +
    # plot points
    geom_point(size=0.5, alpha = 0.5) + # set size and transparency
    # axis labels
    ylab("H/C") +
    xlab("O/C") +
    # axis limits
    xlim(0,1.25) +
    ylim(0,2.5) +
    # add boundary lines for Van Krevelen regions
    geom_segment(x = 0.0, y = 1.5, xend = 1.2, yend = 1.5,color="black",linetype="longdash") +
    geom_segment(x = 0.0, y = 0.7, xend = 1.2, yend = 0.4,color="black",linetype="longdash") +
    geom_segment(x = 0.0, y = 1.06, xend = 1.2, yend = 0.51,color="black",linetype="longdash") +
    guides(colour = guide_legend(override.aes = list(alpha=1, size = 1)))
}

fticr_data_hcoc = 
  fticr_data_longform %>% 
  left_join(fticr_meta %>% dplyr::select(formula, HC, OC))

# gg_vk_domains = 
fticr_meta %>%     
  mutate(Class = factor(Class, levels = c("aliphatic", "unsaturated/lignin", "aromatic", "condensed aromatic"))) %>% 
  filter(!is.na(Class)) %>% 
  gg_vankrev(aes(x = OC, y = HC, color = Class))+
  scale_color_manual(values = PNWColors::pnw_palette("Sunset2", 4))+
  theme(legend.position = "top")+
  guides(color=guide_legend(nrow=2, override.aes = list(size = 1, alpha = 1)))+
  NULL

# Van Krevelens by region
fticr_data_hcoc %>% 
  distinct(formula, HC, OC, region) %>% 
  gg_vankrev(aes(x = OC, y = HC, color = region))+
  stat_ellipse(level = 0.90, show.legend = FALSE)+
 # scale_color_manual(values = PNWColors::pnw_palette("Sunset2", 4))+
  theme(legend.position = "top")+
  facet_wrap(~ region)+
  NULL
  

# Van Krevelens by region - frequency
## how many kits total?
fticr_data_longform %>% 
  distinct(region, kit_id) %>% 
  group_by(region) %>% dplyr::summarise(n = n())

fticr_data_hcoc_frequency = 
  fticr_data_hcoc %>% 
  group_by(formula, region) %>% 
  dplyr::summarise(n = n()) %>% 
  mutate(percentage = case_when(region == "Chesapeake Bay" ~ 100*n/37,
                                region == "Great Lakes" ~ 100*n/15)) %>% 
  left_join(fticr_meta %>% dplyr::select(formula, HC, OC))

gg_vankrev(data = fticr_data_hcoc_frequency,
           aes(x = OC, y = HC, color = percentage))+
  facet_wrap(~region)+
  scale_color_gradientn(colors = pnw_palette("Bay"))+
  labs(color = "percentage of kits where peak was identified",
       caption = "CB = 36 kits, GL = 15 kits")

#
# Relative abundance ------------------------------------------------------

compute_relabund_by_sample = function(fticr_data_longform, fticr_meta){
  fticr_data_longform %>% 
    # add the Class column to the data
    left_join(dplyr::select(fticr_meta, formula, Class), by = "formula") %>% 
    # calculate abundance of each Class as the sum of all counts
    group_by(kit_id, Class) %>%
    dplyr::summarise(abund = sum(presence)) %>%
    ungroup %>% 
    # create a new column for total counts per core assignment
    # and then calculate relative abundance  
    group_by(kit_id) %>% 
    dplyr::mutate(total = sum(abund),
                  relabund  = round((abund/total)*100,2))
}

fticr_relabund_per_sample = compute_relabund_by_sample(fticr_data_longform, fticr_meta)

fticr_relabund_per_sample %>%
  left_join(sample_key) %>% 
  ggplot(aes(x = kit_id, y = relabund, fill = Class))+
  geom_bar(stat = "identity")+
  facet_wrap(~region, scales = "free_x")

#

# Stats - PCA -------------------------------------------------------------

fit_pca_function = function(fticr_relabund_per_sample, sample_key){
  relabund_pca =
    fticr_relabund_per_sample %>% 
    left_join(sample_key %>% dplyr::select(kit_id, region)) %>% 
    ungroup %>% 
    dplyr::select(-c(abund, total)) %>% 
    spread(Class, relabund) %>% 
    filter(!is.na(region)) %>% 
    replace(.,is.na(.),0)
  
  num = 
    relabund_pca %>% 
    dplyr::select(where(is.numeric))
  
  grp = 
    relabund_pca %>% 
    dplyr::select(where(is.character)) %>% 
    dplyr::mutate(row = row_number())
  
  pca_int = prcomp(num, scale. = T)
  
  list(num = num,
       grp = grp,
       pca_int = pca_int)
}

pca_overall = fit_pca_function(fticr_relabund_per_sample, sample_key)

# PCA biplot
ggbiplot(pca_overall$pca_int, obs.scale = 1, var.scale = 1,
         groups = pca_overall$grp$region, 
         ellipse = TRUE, circle = FALSE, var.axes = TRUE, alpha = 0) +
  geom_point(size=2,stroke=1, alpha = 1,
             aes(shape = pca_overall$grp$region,
                 color = groups))+
  scale_shape_manual(values = c(19, 1), name = "")+
  xlim(-4,4)+
  ylim(-3.5,3.5)
  

#
# permanova -----------------------------------------------------------

compute_permanova = function(relabund_cores){
  relabund_wide = 
    fticr_relabund_per_sample %>% 
    left_join(sample_key %>% dplyr::select(kit_id, region)) %>% 
    ungroup %>% 
    dplyr::select(-c(abund, total)) %>% 
    spread(Class, relabund) %>% 
    filter(!is.na(region)) %>% 
    replace(.,is.na(.),0)
  
  permanova_fticr_all = 
    adonis(relabund_wide %>% dplyr::select(where(is.numeric)) ~ region, 
           data = relabund_wide)
  broom::tidy(permanova_fticr_all$aov.tab)
}



