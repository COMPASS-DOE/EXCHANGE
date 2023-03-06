## EXCHANGE-SOIL PH
##
## This script imports raw data for Soil pH and conductivity 
## and exports clean, Level 1 QCd data. 
##
## The measurements were done using a Myron Ultrameter II.
##
## Data are read in from Google Sheets.
## 
## Created: 2022-05-23
## Kaizad F. Patel
##
# #############
# #############

# 1. Setup ---------------------------------------------------------------------
cat("Setup")

# load packages
require(pacman)
pacman::p_load(cowsay,
               tidyverse, 
               googlesheets4, # read_sheet 
               googledrive) # drive_upload

## Welcome
say("Welcome to EXCHANGE!", by = "random")

## Define analyte
var <- "soil pH - specific conductance"


## URL for data
data_path = "1hNnMjyM8zelUqI-AwLeG_lrGGgLyTfztaLp1btzqxZ0"

#
# 2. Import data ---------------------------------------------------------------
cat("Importing", var, "data...")

## read in raw data
data_raw <- googlesheets4::read_sheet(data_path)

#
# 3. Process data ---------------------------------------------------------

soil_pH_data_processed = 
  data_raw %>% 
  filter(is.na(Notes)) %>% 
  mutate(specific_conductance_us_cm = Conductivity_uS_cm/ (1 + 0.02 * (Temp_C - 25)), # conversion from raw cond to specific cond
         specific_conductance_us_cm = signif(specific_conductance_us_cm, digits = 3)) %>% 
  mutate(Transect_location = tolower(Transect_location),
         Transect_location = factor(Transect_location, levels = c("upland", "transition", "wetland"))) %>% 
  dplyr::select(Kit_ID, Transect_location, pH, specific_conductance_us_cm, date_run) %>% 
  rename(transect_location = Transect_location,
         ph = pH) %>% 
  mutate(campaign = "EC1")


#
# 4. Apply QC flags ------------------------------------------------------------

soil_pH_qc = 
  soil_pH_data_processed %>% 
  mutate(ph_flag = case_when(ph < 0 ~ "below range",
                             ph > 14 ~ "above range"),
         specific_conductance_flag = case_when(specific_conductance_us_cm < 0 ~ "below range",
                                               specific_conductance_us_cm > 9999 ~ "above range")) %>% 
  dplyr::select(-starts_with("ph"), -starts_with("specific_conductance"), starts_with("ph"), starts_with("specific_conductance"), -date_run) %>% 
  janitor::clean_names()
  

#
# 5. Export cleaned data --------------------------------------------------

soil_pH_qc %>% googlesheets4::drive_upload("/EC1_Soil_pH_L0B_20220531.csv", row.names = FALSE)

# 6. Check with Metadata for missing: 


