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
  mutate(Specific_Conductance_uS_cm = Conductivity_uS_cm/ (1 + 0.02 * (Temp_C - 25)),
         Specific_Conductance_uS_cm = signif(Specific_Conductance_uS_cm, digits = 3)) %>% 
  mutate(Transect_location = tolower(Transect_location),
         Transect_location = factor(Transect_location, levels = c("upland", "transition", "wetland"))) %>% 
  dplyr::select(Kit_ID, Transect_location, pH, Specific_Conductance_uS_cm, date_run)


#
# 4. Apply QC flags ------------------------------------------------------------

soil_pH_qc = 
  soil_pH_data_processed %>% 
  mutate(pH_flag = case_when(pH < 0 ~ "below range",
                             pH > 14 ~ "above range"),
         Specific_Conductance_flag = case_when(Specific_Conductance_uS_cm < 0 ~ "below range",
                                               Specific_Conductance_uS_cm > 9999 ~ "above range")) %>% 
  dplyr::select(-starts_with("pH"), -starts_with("Specific_Conductance"), starts_with("pH"), starts_with("Specific_Conductance"))
  

#
# 5. Export cleaned data --------------------------------------------------

soil_pH_qc %>% write.csv("Data/Processed/EC1_soil_pH_L0B_2022-05-25.csv", row.names = FALSE, na = "")


