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
         pebbles_angular_rounded = case_match(pebbles_angular_rounded,
                                              c("angular, rounded", "rounded, angular") ~ "angular and rounded",
                                              .default = pebbles_angular_rounded),
         root_presence = case_when(grepl("no roots", root_presence) ~ "none",
                                   grepl("few", root_presence) ~ "few",
                                   grepl("moderate", root_presence) ~ "moderate",
                                   grepl("root dominated", root_presence) ~ "many",
                                   .default = root_presence),
         uninhabited_shells = case_match(uninhabited_shells, 
                                         "intact shells" ~ "intact",
                                         c("intact, fragments", "intact, broken", "broken, intact") ~ "intact and fragments",
                                         "broken" ~ "fragments",
                                         .default = uninhabited_shells),
         visible_minerals = case_match(visible_minerals,
                                       "quarts" ~ "quartz",
                                       .default = visible_minerals)) %>% 
  mutate(root_thickness = case_when(root_presence == "none" ~ NA,
                                    root_thickness == "Coarse" ~ "fine",
                                    .default = root_thickness)) %>% 
  relocate(campaign)

#
# 4. Apply QC flags ------------------------------------------------------------
cat("Applying flags to", var, "data...")

## When naming flags, start the name with the parameter to make it easier to
## manipulate a parameter and its flag together
data_qc <- function(data) {
  data %>% 
    select(-notes, -cohesion_post_lyopholization, -white_flakes) %>% 
    mutate(
      # switch wetland and transition names due to a...
      # ...sampling error: wetland soil was sampled and put into a jar labeled "transition" incorrectly
      transect_location = case_when(kit_id == "K046" & transect_location == "transition" ~ "wetland", 
                                    kit_id == "K046" & transect_location == "wetland" ~ "transition", 
                                    TRUE ~ transect_location))
}

data_clean <- data_qc(data_processed)

# 5. Check with Metadata for missing -------------------------------------------

source("./Processing_Scripts/Metadata_kit_list.R")

metadata_collected %>%
  filter(sample_method == "jar")-> meta_filter

data_clean %>%
  full_join(meta_filter, by = c("campaign", "kit_id", "transect_location")) %>% 
  # add rows for samples not collected, creating a "full" dataset of all possible samples
  mutate(transect_location = factor(transect_location, levels = c("upland", "transition", "wetland", "sediment")),
         pebbles_angular_rounded = case_when(notes == "kit compromised" ~ NA,
                                             notes == "sample compromised" ~ NA,
                                             TRUE ~ pebbles_angular_rounded),
         root_presence = case_when(notes == "kit compromised" ~ NA,
                                   notes == "sample compromised" ~ NA,
                                   TRUE ~ root_presence),           
         root_thickness = case_when(notes == "kit compromised" ~ NA,
                                    notes == "sample compromised" ~ NA,
                                    TRUE ~ root_thickness),
         visible_minerals = case_when(notes == "kit compromised" ~ NA,
                                      notes == "sample compromised" ~ NA,
                                      TRUE ~ visible_minerals),
         visible_organisms = case_when(notes == "kit compromised" ~ NA,
                                       notes == "sample compromised" ~ NA,
                                       TRUE ~ visible_organisms),            
         uninhabited_shells = case_when(notes == "kit compromised" ~ NA,
                                        notes == "sample compromised" ~ NA,
                                        TRUE ~ uninhabited_shells),
         iron_oxidation = case_when(notes == "kit compromised" ~ NA,
                                    notes == "sample compromised" ~ NA,
                                    TRUE ~ iron_oxidation),           
         glass_plastic = case_when(notes == "kit compromised" ~ NA,
                                   notes == "sample compromised" ~ NA,
                                   TRUE ~ glass_plastic),
         notes = case_when(collected == FALSE ~ "sample not collected",
                           TRUE ~ notes)) %>%
  select(-c(sample_type, sample_method, collected)) %>% 
  arrange(kit_id, transect_location) -> vizmetrics_full

# 6. Write cleaned data to drive -----------------------------------------------

vizmetrics_full %>% filter(transect_location != "sediment") -> soil_viz_l1

vizmetrics_full %>% filter(transect_location == "sediment") -> sediment_viz_l1

write_csv(soil_viz_l1, paste0("~/Documents/ec1_soil_visual_metrics_L1_", Sys.Date(), ".csv"))

write_csv(sediment_viz_l1, paste0("~/Documents/ec1_sediment_visual_metrics_L1_", Sys.Date(), ".csv"))

#L1directory = "https://drive.google.com/drive/folders/1yhukHvW4kCp6mN2jvcqmtq3XA5niKVR3"

#drive_upload(media = paste0("./ec1_soilsediment_visualmetrics_L1_", Sys.Date(), ".csv"), name = paste0("ec1_soilsediment_visualmetrics_L1_", Sys.Date(), ".csv"), path = L1directory)

#file.remove(paste0("./ec1_soilsediment_visualmetrics_L1_", Sys.Date(), ".csv"))

