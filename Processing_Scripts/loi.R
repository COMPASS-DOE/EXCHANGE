## This is a data processing script for EXCHANGE, a sub-project of THE DOE-funded 
## COMPASS project (https://compass.pnnl.gov/). 
##
## This script imports raw data for loss on ignition (LOI) measured by 
## combusting ~5g of field-moist soils/sediments in an oven at 550C for 5 hours
## at MCRL, then reweighing to determine mass of water lost.
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
loi_path = "https://docs.google.com/spreadsheets/d/1Osig5zxzW3l9z_1Bb0zNW2tfTdJ60hsh78qMvjgclQE/edit#gid=0"

## Define constants
loi_min = 0
loi_max = 100

## Define analyte
var <- "loi"

#
# 2. Import data ---------------------------------------------------------------
cat("Importing", var, "data...")

## read in raw data
loi_raw <- read_sheet(loi_path)

#
# 3. Process data --------------------------------------------------------------
cat("Processing", var, "data...")

## The formula for GWC is [(wt_crucible + wt_dry) - (wt_crucible + wt_combusted)] / 
## [(crucible + dry) - (crucible)]
loi_processed <- loi_raw %>% 
  mutate(kit_id = str_match(sample_id, "K0\\d\\d")[,1], 
         site = str_to_title(str_match(sample_id, "[:alpha:]{6,10}")), 
         loi_perc = ((wt_crucible_dry_g - wt_crucible_combusted_g) / (wt_crucible_dry_g - wt_crucible_g)) * 100) %>% 
  dplyr::select(kit_id, site, loi_perc)

#
# 4. Apply QC flags ------------------------------------------------------------
cat("Applying flags to", var, "data...")

clean_data <- function(data) {
  data %>% 
    mutate(loi_perc = round(loi_perc, 0)) %>% 
    mutate(f_loi = loi_perc < loi_min | loi_perc > loi_max) 
}

loi <- clean_data(loi_processed)

#
# 5. Write cleaned data to drive -----------------------------------------------

write_csv(loi, "Data/Processed/EC1_LOI_L0B_20220419.csv")

## We should add Sys.date or hardcode date so we know when the L0B was born
## The file written out should be named following 
## [Campaign]_[Analyte]_[QC_level]_[Date_of_creation_YYYYMMDD].csv
#drive_upload(media = data_clean, path = data_path)


