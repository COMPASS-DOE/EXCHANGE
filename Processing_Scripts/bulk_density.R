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
pacman::p_load(tidyverse, 
               googlesheets4, # read_sheet 
               googledrive) # drive_upload

theme_set(theme_bw())

## URL for bulk density data
bd_path = "https://docs.google.com/spreadsheets/d/1R00De3XmShwcaVdSuqeO-IaL_sdF200n14hKjmbdFM4/edit#gid=0"


# 2. Import data ---------------------------------------------------------------

## Read in GWC data for correcting bulk density calculations 
gwc <- read_csv("Data/Processed/EC1_GWC_L0B.csv")

## import bulk density data (non-hyprop)
bd_raw <- read_sheet(bd_path) 

## Calculate bulk density using GWC to calculate dry weight
## two 2.5" ring lids weigh 16.9g, with 5cm diameter and 5.1cm height. Thus, the 
## volume is pi * (d/2)^2 * headspace_cm. Dry weight is calculated from GWC 
## based on the formula of (field_moist / ((GWC / 100) + 1)) from KP
bd <- bd_raw %>% 
  mutate(kit_id = str_match(sample_id, "K0\\d\\d")[,1], 
         site = str_to_title(str_match(sample_id, "[:alpha:]{6,10}")), 
         wt_soil_fm_g = wt_ring_soil_lids_g - (wt_ring_g + 17), 
         ring_diameter = ifelse(wt_ring_g > 150, 8, 5), 
         total_volume = ifelse(wt_ring_g > 150, 250, 100), 
         headspace_volume = pi * (ring_diameter/2)^2 * height_headspace_cm, 
         volume_soil_cm3 = total_volume - headspace_volume) %>% 
  left_join(gwc, by = c("kit_id", "site")) %>% 
  mutate(wt_soil_dry_g = wt_soil_fm_g / ((gwc_perc / 100) + 1),
         bulk_density_g_cm3 = wt_soil_dry_g / volume_soil_cm3) %>% 
  dplyr::select(kit_id, site, bulk_density_g_cm3)


# 3. QC data -------------------------------------------------------------------

clean_data <- function(data) {
  data %>% 
    mutate(bulk_density_flag = ifelse(bulk_density_g_cm3 > 0 | bulk_density_g_cm3 < 2, "TRUE", NA))
}

bulk_density <- clean_data(bd)


# 4. Write out dataset ---------------------------------------------------------

## Update with EC1 nomenclature when data are complete
write_csv(bulk_density, "Data/Processed/EC1_BulkDensity_LOB_20220418.csv")

