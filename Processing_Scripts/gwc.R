## This is a data processing script for EXCHANGE, a sub-project of THE DOE-funded 
## COMPASS project (https://compass.pnnl.gov/). 
##
## This script imports raw data for gravimetric water content (GWC) measured by 
## drying ~5g of field-moist soils/sediments in an oven at 105C for >24 hours at 
## MCRL, then reweighing to determine mass of water lost.
## This script exports clean, Level 0B QC'ed data. 
## Data are read in from the COMPASS Google Drive.
## 
## Created: 2022-04-06
## Peter Regier
##
# ############# #
# ############# #

#
# 1. Setup ---------------------------------------------------------------------
cat("Setup")

# load packages
require(pacman)
pacman::p_load(cowsay, 
               tidyverse, 
               cowplot,
               grattantheme, 
               googlesheets4, # read_sheet 
               googledrive) # drive_upload

## Welcome
say("Welcome to EXCHANGE!", by = "random")

## URL for data
gwc_path = "https://docs.google.com/spreadsheets/d/1Osig5zxzW3l9z_1Bb0zNW2tfTdJ60hsh78qMvjgclQE/edit#gid=0"

## Define constants (we are not defining a maximum value for GWC)
gwc_min = 0

## Define analyte
var <- "gwc"

#
# 2. Import data ---------------------------------------------------------------
cat("Importing", var, "data...")

## read in raw data.
gwc_raw <- read_sheet(gwc_path) %>% 
  filter(`duplicate to ignore` == FALSE)

#
# 3. Process data --------------------------------------------------------------
cat("Processing", var, "data...")

## The formula for GWC is [(wt_crucible + wt_moist) - (wt_crucible + wt_dry)] / 
## [(crucible + dry) - (crucible)]
gwc_processed <- gwc_raw %>% 
  mutate(campaign = "EC1",
         kit_id = str_match(sample_id, "K0\\d\\d")[,1], 
         transect_location = str_to_title(str_match(sample_id, "[:alpha:]{6,10}")), 
         gwc_perc = ((wt_crucible_moist_g - wt_crucible_dry_g) / (wt_crucible_dry_g - wt_crucible_g))* 100) %>% 
  dplyr::select(campaign, kit_id, transect_location, gwc_perc)

#
# 4. Apply QC flags ------------------------------------------------------------
cat("Applying flags to", var, "data...")

clean_data <- function(data) {
  data %>% 
    mutate(gwc_perc = round(gwc_perc, 0)) %>% 
    mutate(gwc_flag = ifelse(gwc_perc < gwc_min, "outside range", NA)) 
}

gwc <- clean_data(gwc_processed) %>% 
  filter(!is.na(gwc_perc))

#
# 5. Write cleaned data to drive -----------------------------------------------

date_updated <- "20220714"

write_csv(gwc, paste0("Data/Processed/EC1_Soil_GWC_L0B_", date_updated, ".csv"))

## Check for duplicates
gwc %>% 
  group_by(kit_id, transect_location) %>% 
  filter(n() > 1) 