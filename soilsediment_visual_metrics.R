## This is a data processing script for EXCHANGE, a sub-project of THE DOE-funded 
## COMPASS project (https://compass.pnnl.gov/). 
##
## This script imports raw data for Soil and Sediment Visual Metrics and 
## standardizes responses.
## and exports clean, Level 0B QC'ed data. 

## Data are read in from the COMPASS Google Drive.
## 
## Created: 2023-12-08
## Stephanie Pennington
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
               googlesheets4, # read_sheet 
               googledrive) # drive_upload

## Welcome
say("Welcome to EXCHANGE!", by = "random")

## URL for data

data_path = "https://docs.google.com/spreadsheets/d/1BRsbySjVj55sxnPSM7eMcTO1ob8Gx-etrncPRgByz9A/edit?usp=sharing" 

## Define constants
# a_min = int
# a_max = int

## Define analyte
var <- "soil/sediment visual metrics"

#
# 2. Import data ---------------------------------------------------------------
cat("Importing", var, "data...")

## read in raw data
data_raw <- read_sheet(data_path) 

#
# 3. Process data --------------------------------------------------------------
cat("Processing", var, "data...")

## Note: common columns needed for every dataset: 
## "campaign", "kit_id", "transect_location"

data_processed <- data_raw %>% 
  janitor::clean_names() %>% 
  separate(sample, into = c("kit_id", "transect_location")) %>% 
  rename(root_thickness = root_thickness_all_roots_are_classified_as_coarse_roots_all_are_less_than_2mm_in_diameter,
         pebbles_angular_rounded = pebbles,  
         iron_oxidation = presence_of_iron_oxidation,
         glass_plastic = presence_of_plastics_or_glass,
         white_flakes = mysterious_white_flakes,
         root_presence = root_mass) %>% 
  dplyr::select(-do_not_use_had_misclassified, -sand_content) %>% 
  mutate(campaign = "EC1",
         root_thickness = tolower(root_thickness),
         pebbles_angular_rounded = case_match(pebbles_angular_rounded,
                                              c("angular, rounded", "rounded, angular") ~ "both",
                                              .default = pebbles_angular_rounded),
         root_presence = case_when(grepl("no roots", root_presence) ~ "none",
                                   grepl("few", root_presence) ~ "few",
                                   grepl("moderate", root_presence) ~ "moderate",
                                   grepl("root dominated", root_presence) ~ "many",
                                   .default = root_presence)) %>% 
  relocate(campaign)

#
# 4. Apply QC flags ------------------------------------------------------------
cat("Applying flags to", var, "data...")

## When naming flags, start the name with the parameter to make it easier to
## manipulate a parameter and its flag together
data_qc <- function(data) {
  data %>% 
    mutate(a = round(a, n_sig_figs)) %>% 
    mutate(a_flag_1 = a < a_min | a > a_max, 
           a_flag_n = a < a_min | a > a_max, ) 
}

data_clean <- data_qc(data_processed)

# 5. Check with Metadata for missing -------------------------------------------

source("./Processing_Scripts/Metadata_kit_list.R")

metadata_collected %>%
  filter(sample_method == "jar")-> meta_filter

data_processed %>%
  full_join(meta_filter, by = c("campaign", "kit_id", "transect_location"))  -> join
  # add rows for samples not collected, creating a "full" dataset of all possible samples

# 6. Write cleaned data to drive -----------------------------------------------

## We should add Sys.date or hardcode date so we know when the L0B was born
## The file written out should be named following 
## [Campaign]_[Analyte]_[QC_level]_[Date_of_creation_YYYYMMDD].csv
drive_upload(media = data_clean, path = data_path)
