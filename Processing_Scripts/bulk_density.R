## This script imports raw data for bulk density measured on HYPROP rings collected
## for wetland, transition, upland at PNNL MCRL and exports clean, Level 1 QC'ed 
##data. Data are read in from Google Sheets.
## 
## Created: 2022-01-07
## Peter Regier
##
# #############
# #############

## TO DO: not correct! Need to use dry weight obtained from GWC for correct BD!!!

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

## Read in GWC data for correcting bulk density calculations

gwc_path <- list.files(path = "Data/Processed", pattern = "GWC")
gwc <- read_csv(paste0("Data/Processed/", gwc_path))

## import bulk density data (non-hyprop)
bulk_density_raw <- read_sheet(bd_path) 

## Calculate bulk density using GWC to calculate dry weight
## two 2.5" ring lids weigh 16.9g, with 5cm diameter and 5.1cm height. Thus, the 
## volume is pi * (d/2)^2 * headspace_cm. Dry weight is calculated from GWC 
## based on the formula of (field_moist / ((GWC / 100) + 1)) from KP
bulk_density_processed <- bulk_density_raw %>% 
  mutate(campaign = "EC1",
         kit_id = str_match(sample_id, "K0\\d\\d")[,1], 
         transect_location = str_to_title(str_match(sample_id, "[:alpha:]{6,10}")), 
         wt_soil_fm_g = wt_ring_soil_lids_g - (wt_ring_g + 17), 
         ring_diameter = ifelse(wt_ring_g > 150, 8, 5), 
         total_volume = ifelse(wt_ring_g > 150, 250, 100), 
         headspace_volume = pi * (ring_diameter/2)^2 * height_headspace_cm, 
         volume_soil_cm3 = total_volume - headspace_volume) %>% 
  left_join(gwc %>% select(-campaign), by = c("kit_id", "transect_location")) %>% 
  mutate(wt_soil_dry_g = wt_soil_fm_g / ((gwc_perc / 100) + 1),
         bulk_density_g_cm3 = wt_soil_dry_g / volume_soil_cm3) %>% 
  dplyr::select(campaign, kit_id, transect_location, bulk_density_g_cm3) %>% 
  mutate(bulk_density_g_cm3 = round(bulk_density_g_cm3, 2))


# 3. QC data -------------------------------------------------------------------

clean_data <- function(data) {
  data %>% 
    mutate(bulk_density_flag = ifelse(bulk_density_g_cm3 < 0 | bulk_density_g_cm3 > 2, "outside range", NA))
}

bulk_density <- clean_data(bulk_density_processed)


# 4. Write out dataset ---------------------------------------------------------
date_updated <- "20220531"

write_csv(bulk_density, paste0("Data/Processed/EC1_Soil_BulkDensity_L0B_", date_updated, ".csv"))


