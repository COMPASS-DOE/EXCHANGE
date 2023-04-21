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
  mutate(Conductivity_uS_cm = case_when(grepl("measured as milliSiemens", Notes) ~ Conductivity_uS_cm * 1000,
                                        TRUE ~ Conductivity_uS_cm)) %>% 
  mutate(specific_conductance_us_cm = Conductivity_uS_cm/ (1 + 0.02 * (Temp_C - 25)), # conversion from raw cond to specific cond
         specific_conductance_us_cm = signif(specific_conductance_us_cm, digits = 3),
         transect_location = tolower(Transect_location),
         transect_location = factor(transect_location, levels = c("upland", "transition", "wetland")),
         campaign = "EC1") %>% 
  rename(ph = pH,
         kit_id = Kit_ID) %>% 
  filter(!grepl("SKIP", Notes)) %>% #remove error sample runs labeled "SKIP" in notes
  dplyr::select(campaign, kit_id, transect_location, ph, specific_conductance_us_cm, Notes) %>% 
  # switch wetland and transition names due to a...
  # ...sampling error: wetland soil was sampled and put into a jar labeled "transition" incorrectly
  mutate(transect_location = case_when(kit_id == "K046" & transect_location == "transition" ~ "wetland", 
                                       kit_id == "K046" & transect_location == "wetland" ~ "transition", 
                                       TRUE ~ transect_location)) 

#
# 4. Apply QC flags ------------------------------------------------------------

soil_pH_qc = 
  soil_pH_data_processed %>% 
  mutate(ph_flag = case_when(ph < 0 ~ "below range",
                             ph > 14 ~ "above range"),
         specific_conductance_flag = case_when(!grepl("measured as milliSiemens", Notes) & specific_conductance_us_cm < 0 ~ "below range",
                                               grepl("measured as milliSiemens", Notes) & specific_conductance_us_cm < 10 ~ "below range",
                                               !grepl("measured as milliSiemens", Notes) & specific_conductance_us_cm > 9999 ~ "above range",
                                               grepl("measured as milliSiemens", Notes) & specific_conductance_us_cm > 200000 ~ "above range")) %>% 
  dplyr::select(campaign, kit_id, transect_location, ph, specific_conductance_us_cm, ph_flag, specific_conductance_flag) %>% 
  janitor::clean_names()

# 5. Check with Metadata for missing:

source("./Processing_Scripts/Metadata_kit_list.R")
source("./Processing_Scripts/jars_metadata.R")

metadata_collected %>% 
  filter(sample_type == "soil", sample_method == "jar") -> meta_filter

soil_pH_qc %>% 
  full_join(meta_filter, by = c("campaign", "kit_id", "transect_location")) %>% 
  full_join(jars_clean, by = c("campaign", "kit_id", "transect_location"))  %>% 
  filter(transect_location != "sediment") %>% 
  mutate(ph = case_when(notes == "kit compromised" ~ NA,
                        TRUE ~ ph),
         specific_conductance_us_cm = case_when(notes == "kit compromised" ~ NA,
                                                TRUE ~ specific_conductance_us_cm),
         ph_flag = case_when(is.na(ph) & collected == FALSE ~ "sample not collected",
                             notes == "kit compromised" ~ "kit compromised",
                             notes == "sample compromised" ~ "sample compromised",
                             TRUE ~ ph_flag),
         specific_conductance_flag = case_when(is.na(specific_conductance_us_cm) & collected == FALSE ~ "sample not collected",
                                               notes == "kit compromised" ~ "kit compromised",
                                               notes == "sample compromised" ~ "sample compromised",
                                               TRUE ~ specific_conductance_flag)) %>% 
  select(-sample_type, -collected, -sample_method, -sample_weight_g) -> soil_ph_cond_full

#
# 6. Export cleaned data --------------------------------------------------

soil_ph_cond_full %>% write.csv(paste0("./ec1_soil_ph_cond_L1_", Sys.Date(), ".csv"), row.names = FALSE)

L1directory = "https://drive.google.com/drive/folders/1yhukHvW4kCp6mN2jvcqmtq3XA5niKVR3"

drive_upload(media = paste0("./ec1_soil_ph_cond_L1_", Sys.Date(), ".csv"), name= paste0("ec1_soil_ph_cond_L1_", Sys.Date(), ".csv"), path = L1directory )

file.remove(paste0("./ec1_soil_ph_cond_L1_", Sys.Date(), ".csv"))
