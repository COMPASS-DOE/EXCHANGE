## This script imports raw data for bulk density measured on HYPROP rings collected
## for wetland, transition, upland at PNNL MCRL and exports clean, Level 1 QC'ed 
##data. Data are read in from Google Sheets.
## 
## Created: 2022-01-07
## Peter Regier
##Updated: 2023-04-24 
# Allison Myers-Pigg
# #############
# #############

# 1. Setup ---------------------------------------------------------------------

# load packages
require(pacman)
pacman::p_load(cowsay,
               tidyverse, 
               googlesheets4, # read_sheet 
               googledrive) # drive_upload

## set plot theme
theme_set(theme_bw())

## URL for bulk density data
bd_path = "https://docs.google.com/spreadsheets/d/1R00De3XmShwcaVdSuqeO-IaL_sdF200n14hKjmbdFM4/edit#gid=0"


# 2. Import data ---------------------------------------------------------------

## Read in L2 GWC data:

L2directory = "https://drive.google.com/drive/u/1/folders/1M-ASGuRoKqswiKbUWylWzoAyUmMPm367"

import_l2_gwc_data = function(directory){
  
  ## a. Create a list of files to download
  files <- 
    drive_ls(directory) %>% 
    filter(grepl("soil_gwc", name))
  
  ## b. Download files to local (don't worry, we'll delete em in a sec)
  lapply(files$id, drive_download, overwrite = TRUE)
  
  dat <- read.csv(files$name)
  
  ## d. delete the temporary files
  file.remove(c(files$name))  
  
  ## e. output
  dat
}

gwc_l2 = import_l2_gwc_data(L2directory)

## import bulk density data- these are measurements of field moist soils before rings were run on the hyprops.
bulk_density_raw <- read_sheet(bd_path) %>%
  filter(info == "above line") %>% 
  filter(duplicate == "FALSE")

#deal with the fact that this column didn't come in as a number...
bulk_density_raw$wt_ring_soil_lids_g = as.numeric(bulk_density_raw$wt_ring_soil_lids_g)

## Calculate bulk density using GWC to calculate dry weight
# wt_ring_g is weight of the ring in g as written on the side of the ring
## two 2.5" ring lids weigh 16.9g, with 5cm diameter and 5.1cm height. Thus, the 
## volume is pi * (d/2)^2 * headspace_cm. 
# Dry weight is calculated from GWC 
## based on the formula of (field_moist / ((GWC / 100) + 1)) from KP

ring_lids = 16.9

## If dry moisture content (%) = (Total Weight - Total Dry Weight) * 100 / (Total Dry Weight - pan weight)
# Then % dry moisture content = mass water / mass dry soil 
# Wet weight = dry weight x % dry moisture content
# If we know the dry basis moisture content in percent, then we can calculate the wet basis moisture content by:
# Mw = (Md/(100+Md)) X 100 
# And if we know dry GWC, we can calculate the Mass of the dry sample (Mdry):
# GWC % = GWC * 100%
# GWC = Mwater/Mdry = (Mwet - Mdry) / Mdry
# Bulk Density = Mdry/volume
# Therefore; if (Mwet-Mdry)/Mdry = GWC then,
# Mdry = Mwet / [(GWC/100) + 1] 


bulk_density_processed <- bulk_density_raw %>% 
  mutate(campaign = "EC1",
         kit_id = str_match(sample_id, "K0\\d\\d")[,1], 
         transect_location = str_to_lower(str_match(sample_id, "[:alpha:]{6,10}")), 
         wt_soil_fm_g = wt_ring_soil_lids_g - (wt_ring_g + ring_lids), #determine weight of soil in ring by subtracting ring and lid weights
         ring_diameter = ifelse(wt_ring_g > 150, 8, 5), #there are two different size rings with different weights. Set the ring diameter to 8 cm if ring weight is > 150g
         total_volume = ifelse(wt_ring_g > 150, 250, 100), #there are two different size rings with different weights. Set the ring volume to 250 cm3 if ring weight is > 150g
         headspace_volume = pi * (ring_diameter/2)^2 * height_headspace_cm, 
         volume_soil_cm3 = total_volume - headspace_volume) %>% 
  left_join(gwc_l2 %>% select(-campaign), by = c("kit_id", "transect_location")) %>% 
  mutate(wt_soil_dry_g = wt_soil_fm_g / ((moisturecontent_perc_drywtbasis / 100) + 1),
         bulk_density_g_cm3 = wt_soil_dry_g / volume_soil_cm3) %>%
  dplyr::select(campaign, kit_id, transect_location, moisturecontent_perc_drywtbasis, bulk_density_g_cm3)


# 3. QC data -------------------------------------------------------------------
## Wetland organic soil ranges 0.1-0.4g/cm3
## wetland mineral soil ranges 1.0-1.5 g/cm3

clean_data <- function(data) {
  data %>% 
    mutate(bulk_density_flag = case_when(bulk_density_g_cm3 < 0 | bulk_density_g_cm3 > 2 ~ "outside range",
                                         is.na(moisturecontent_perc_drywtbasis) ~ "no gravimetric water content value",
                                         TRUE ~ NA))
}

bulk_density_qc <- clean_data(bulk_density_processed)

# 4. Check with Metadata for missing:

source("./Processing_Scripts/Metadata_kit_list.R")

metadata_collected %>%
  filter(sample_type == "soil", sample_method == "hyprop") -> meta_filter

bulk_density_qc %>% 
  full_join(meta_filter, by = c("campaign", "kit_id", "transect_location")) %>% 
  mutate(bulk_density_g_cm3 = case_when(notes == "kit compromised" ~ NA,
                        TRUE ~ bulk_density_g_cm3),
         bulk_density_flag = case_when(is.na(bulk_density_g_cm3) & collected == FALSE ~ "sample not collected",
                             notes == "kit compromised" ~ "kit compromised",
                             TRUE ~ bulk_density_flag )) %>% 
  select(-sample_type, -collected, -sample_method, -notes, -moisturecontent_perc_drywtbasis) -> soil_bulk_density_full

# 6. Export cleaned data --------------------------------------------------

soil_bulk_density_full %>% write.csv(paste0("./ec1_soil_bulk_density_L1_", Sys.Date(), ".csv"), row.names = FALSE)

L1directory = "https://drive.google.com/drive/folders/1yhukHvW4kCp6mN2jvcqmtq3XA5niKVR3"

drive_upload(media = paste0("./ec1_soil_bulk_density_L1_", Sys.Date(), ".csv"), name= paste0("ec1_soil_bulk_density_L1_", Sys.Date(), ".csv"), path = L1directory )

file.remove(paste0("./ec1_soil_bulk_density_L1_", Sys.Date(), ".csv"))

