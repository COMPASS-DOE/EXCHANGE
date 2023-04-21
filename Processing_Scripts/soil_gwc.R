## This is a data processing script for EXCHANGE, a sub-project of THE DOE-funded 
## COMPASS project (https://compass.pnnl.gov/). 
##
## This script imports raw data for gravimetric water content (GWC) aka soil moisture content.
## This was measured by drying ~5g of field-moist soils/sediments in an oven at 105C for >24 hours at 
## MCRL, then reweighing to determine mass of water lost.
## This script exports clean, Level 0B QC'ed data. 
## Data are read in from the COMPASS Google Drive.
## 
## Created: 2022-04-06
## Peter Regier
## Updated: 2023-03-07
## Allison Myers-Pigg & Stephanie Pennington
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
               googlesheets4, # read_sheet 
               googledrive) # drive_upload

## Welcome
say("Welcome to EXCHANGE!", by = "random")

## URL for data
gwc_path = "https://docs.google.com/spreadsheets/d/11sr5S6VTS7K0rcQXq-QNZkImdLxwm3cY5lvLvaExvk8/edit#gid=0"

## Define constants (we are not defining a maximum value for GWC)
gwc_min = 0

## Define analyte
var <- "gwc"

#
# 2. Import data ---------------------------------------------------------------
cat("Importing", var, "data...")

## read in raw data.
gwc_raw <- read_sheet(gwc_path) %>%
  rename(date_ran = `...1`)

#
# 3. Process data --------------------------------------------------------------
cat("Processing", var, "data...")

#Soil Dry Basis 
#Equation: dry moisture content (%) = (Total Weight - Total Dry Weight) * 100 / (Total Dry Weight - pan weight)

#Soil Wet Basis 
#Equation: wet moisture content (%) = (Total Weight - Total Dry Weight) * 100 / (Total Weight - pan weight)

#Equations from Reddy et al., 2013 Chapter 3 in Methods in Biogeochemistry of Wetlands 
# https://doi.org/10.2136/sssabookser10.c3 

#We are going to report ONLY the dry weight basis 

gwc_processed <- gwc_raw %>% 
  mutate(campaign = "EC1",
         kit_id = str_match(sample_id, "K0\\d\\d")[,1],
         transect_location = str_to_lower(str_match(sample_id, "[:alpha:]{6,10}")), 
         moisturecontent_perc_drywtbasis = ((wt_crucible_moist_g - wt_crucible_dry_g) / (wt_crucible_dry_g - wt_crucible_g))* 100,
         moisturecontent_perc_wetwtbasis = ((wt_crucible_moist_g - wt_crucible_dry_g) / (wt_crucible_moist_g - wt_crucible_g))* 100) %>% 
  dplyr::select(-c(sample_id:notes, date_ran))



# 4. Apply QC flags ------------------------------------------------------------
cat("Applying flags to", var, "data...")

clean_data <- function(data) {
  data %>% 
    mutate(gwc_flag = ifelse(moisturecontent_perc_drywtbasis < gwc_min | moisturecontent_perc_wetwtbasis < gwc_min, "outside range", NA)) %>%
    group_by(campaign, kit_id, transect_location) %>%
    mutate(duplicate = case_when(n() > 1 ~ "replicates averaged"),
           moisturecontent_perc_drywtbasis = mean(moisturecontent_perc_drywtbasis),
           moisturecontent_perc_wetwtbasis = mean(moisturecontent_perc_wetwtbasis),
           moisturecontent_perc_drywtbasis = round(moisturecontent_perc_drywtbasis, 0),
           moisturecontent_perc_wetwtbasis = round(moisturecontent_perc_wetwtbasis, 0),) %>%
    unite(gwc_flag, duplicate, sep = ", ", na.rm = TRUE) %>%
    distinct()
}

gwc_1 <- clean_data(gwc_processed)


# Need to add flag for samples taken from a different source: 
gwc <- gwc_1 %>%
 mutate(gwc_flag = case_when(kit_id == "K029" & transect_location == "transition" ~ "sample taken from hyprop ring", #this is based on raw data notes and cross reference with kit tracking sheet
                             kit_id == "K058" & transect_location == "wetland" ~ "sample taken from hyprop ring", #this is based on raw data notes and cross reference with kit tracking sheet
                             kit_id == "K060" & transect_location == "wetland" ~ "sample taken from hyprop ring", #this is based on raw data notes and cross reference with kit tracking sheet
                             TRUE ~ gwc_flag))

#

# 6. Check with Metadata for missing:

source("./Processing_Scripts/Metadata_kit_list.R")

#Bags
bags_inventory = googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1wTvUj0aIPZ31-cw1mYXk86sJDkL4yeO9KRx0ALn7QKc/edit#gid=0") %>% 
  select(campaign, kit_id, transect_location, does_bag_exist, notes)

metadata_collected %>%
  filter(sample_method == "bag") -> meta_filter

gwc %>% 
  filter(gwc_flag != "sample taken from hyprop ring") %>%
  full_join(meta_filter, by = c("campaign", "kit_id", "transect_location"))  %>%
  full_join(bags_inventory, by = c("campaign", "kit_id", "transect_location"))  %>%
  filter(collected == TRUE & is.na(moisturecontent_perc_drywtbasis) | collected == FALSE & !is.na(moisturecontent_perc_drywtbasis)) -> check_these

View(check_these)

could_rerun <- check_these %>% filter(does_bag_exist == TRUE)

## Finalize full dataset

gwc %>% 
  select(campaign, kit_id, transect_location, moisturecontent_perc_drywtbasis, gwc_flag) %>%
  full_join(meta_filter, by = c("campaign", "kit_id", "transect_location"))  %>%
  mutate(moisturecontent_perc_drywtbasis = case_when(gwc_flag == "sample taken from hyprop ring" ~ NA,
                              TRUE ~ moisturecontent_perc_drywtbasis),
         gwc_flag = case_when(kit_id == "K050" & transect_location== "upland" ~ "sample lost",
                              TRUE ~ gwc_flag)) %>% 
  mutate(gwc_flag = case_when(kit_id == "K055" & transect_location== "sediment" ~ "sample not analyzed",
                              TRUE ~ gwc_flag)) %>%
  mutate(moisturecontent_perc_drywtbasis = case_when(kit_id %in% c("K001","K007") ~ NA,
                                                     TRUE ~ moisturecontent_perc_drywtbasis),
         gwc_flag = case_when(kit_id %in% c("K001","K007") ~ "kit compromised",
                              TRUE ~ gwc_flag)) %>%
  mutate(gwc_flag = case_when(is.na(moisturecontent_perc_drywtbasis) & collected == FALSE ~ "sample not collected",
                             TRUE ~ gwc_flag)) %>%
  select(-sample_type, -collected, -sample_method, -notes) -> gwc_full

View(gwc_full)

# 5. Export cleaned data --------------------------------------------------

gwc_full %>% write.csv(paste0("./ec1_soil_gwc_L1_", Sys.Date(), ".csv"), row.names = FALSE)

L1directory = "https://drive.google.com/drive/folders/1yhukHvW4kCp6mN2jvcqmtq3XA5niKVR3"

drive_upload(media = paste0("./ec1_soil_gwc_L1_", Sys.Date(), ".csv"), name= paste0("ec1_soil_gwc_L1_", Sys.Date(), ".csv"), path = L1directory)

file.remove(paste0("./ec1_soil_gwc_L1_", Sys.Date(), ".csv"))

