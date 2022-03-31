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

## import bulk density data (non-hyprop)
## two 2.5" ring lids weigh 16.9g, with 5cm diameter and 5.1cm height. Thus, the 
## volume is pi * (d/2)^2 * headspace_cm
bd <- read_sheet(bd_path) %>% 
  mutate(kit_id = str_match(sample_id, "K0\\d\\d")[,1], 
         site = str_to_title(str_match(sample_id, "[:alpha:]{6,10}")), 
         wt_soil_g = wt_ring_soil_lids_g - (wt_ring_g + 17), 
         ring_diameter = ifelse(wt_ring_g > 150, 8, 5), 
         total_volume = ifelse(wt_ring_g > 150, 250, 100), 
         headspace_volume = pi * (ring_diameter/2)^2 * height_headspace_cm, 
         volume_soil_ml = total_volume - headspace_volume, 
         bulk_density_g_ml = wt_soil_g / volume_soil_ml) %>% 
  dplyr::select(kit_id, site, bulk_density_g_ml)


# 3. QC data -------------------------------------------------------------------

clean_data <- function(data) {
  data %>% 
    filter(bulk_density_g_ml > 0 & bulk_density_g_ml < 10)
}

bulk_density <- clean_data(bd)


# 4. Write out dataset ---------------------------------------------------------

## Update with EC1 nomenclature when data are complete
write_csv(bulk_density, "Data/bulk_density_provisional.csv")

