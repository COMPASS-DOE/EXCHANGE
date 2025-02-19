## This script imports raw data for soil texture measured using the hydrometer method.
## Data are read in from Google Drive
## 
## Created: 2024-11-13
## Kaizad F. Patel
##

# 1. Setup ---------------------------------------------------------------------

# load packages
require(pacman)
pacman::p_load(cowsay,
               tidyverse, # keep things tidy
               janitor, # useful for simplifying column names
               googlesheets4 # read_sheet 
               )

## Welcome
say("Welcome to EXCHANGE!", by = "random")

## Define analyte
var <- "soil texture"


## URL for data
data_path = "https://docs.google.com/spreadsheets/d/1PSKKbnmr3jVhjv172mPynmkt3mVlOH_5KQgSa7DTCIU"

#
# 2. Import data ---------------------------------------------------------------
cat("Importing", var, "data...")

## read in raw data from Googlesheet
data_raw <- googlesheets4::read_sheet(data_path, col_types = "c")

#
# 3. Process data ---------------------------------------------------------

# function to compute % sand-silt-clay from the hydrometer readings
compute_soil_texture = function(dat){
  
  ## This function will use the equations provided in Gee & Bauder
  ## to compute % sand, clay, silt
  
  ## % sand = fraction weight of material collected on the 53 μm sieve.
  ## % clay = computed using 90 and 1440 minute hydrometer readings
  ## % silt = 100 - (% sand + % clay)
  
  dat %>% 
    mutate(
      
      wt_sand_g = wt_sieve_soil_53um_g - wt_sieve_g,
      wt_fines = wt_jar_soil_g - wt_half_gallon_jar_g,
      wt_dry_soil_g = wt_sand_g + wt_fines,
      
      B = (30 * 0.0091) / (9.98 * (2.65 - 1)), # constant
      
      #h = 16.3 - (0.164 * R),
      h_90min = 16.3 - (0.164 * reading_90min),
      h_1440min = 16.3 - (0.164 * reading_1440min),
      
      theta_90min = 1000 * (B * h_90min)^0.5,
      theta_1440min = 1000 * (B * h_1440min)^0.5,
      
      # P = summation %
      P_90min = ((reading_90min - blank_90min)/wt_dry_soil_g) * 100, 
      P_1440min = ((reading_1440min - blank_1440min)/wt_dry_soil_g) * 100,
      
      # X = mean diameter
      X_90min = theta_90min * (90)^-0.5, 
      X_1440min = theta_1440min * (1440)^-0.5,
      
      m = (P_90min - P_1440min)/log(X_90min/X_1440min),
      
      # percent sand-silt-clay
      percent_clay = (m * log(2/X_1440min)) + P_1440min,
      percent_sand = ((wt_sieve_soil_53um_g - wt_sieve_g)/wt_dry_soil_g) * 100,
      percent_silt = 100 - (percent_sand + percent_clay)
    ) %>% 
    mutate_at(vars(c(percent_sand, percent_silt, percent_clay)), round, 2) %>% 
    dplyr::select(sample_id, starts_with("percent"), notes) %>% 
    force()
  
}

data_processed = 
  data_raw %>% 
  mutate_at(vars(-c(sample_id, date_started, skip, notes)), as.numeric) %>% 
  filter(is.na(skip)) %>% 
  compute_soil_texture() %>% 
  mutate(sample_id = paste0("EC1_", sample_id)) %>% 
  separate(sample_id, into = c("campaign", "kit_id", "transect_location")) %>% 
  mutate(transect_location = toupper(transect_location),
         transect_location = recode(transect_location, "U" = "Upland", "T" = "Transition", "W" = "Wetland")) %>% 
  arrange(kit_id, transect_location) %>% 
  filter(!is.na(percent_clay))

#
# 4. Apply QC flags ------------------------------------------------------------
compute_flag_texture = function(dat){
  
  # since all the values are %, they must be 0-100 %
  # anything outside of this range is flagged
  
  dat %>% 
    mutate(flag = case_when((percent_clay <= 0 | percent_clay >= 100 | 
                                       percent_sand <= 0 | percent_sand >= 100 | 
                                       percent_silt <= 0 | percent_silt >= 100) ~ "Out_of_Range"))
}

texture_with_flags = 
  data_processed %>% compute_flag_texture(.) 

texture_processed = 
  texture_with_flags %>% 
  filter(!is.na(flag)) %>% 
  mutate(percent_sand = NA, percent_clay = NA, percent_silt = NA) %>% 
  bind_rows(texture_with_flags %>% filter(is.na(flag))) %>% 
  mutate(campaign = "EC1") %>% 
  dplyr::select(campaign, kit_id, transect_location, everything()) %>% 
  arrange(kit_id, transect_location) %>% 
  dplyr::select(-notes)


texture_processed %>% write.csv("Data/Processed/soil_texture_2025-02-19.csv", na = "", row.names = F)
# 
# COMPLETION --------------------------------------------------------------

metadata = read.csv("metadata_collected.csv")
metadata2 = 
  metadata %>% 
  filter(collected) %>% 
  filter(sample_type == "soil") %>% 
  distinct(kit_id, transect_location)

texture_completion = 
  texture_with_flags2 %>% 
  mutate(completed = if_else(!is.na(percent_clay), TRUE, FALSE),
         transect_location = tolower(transect_location)) %>% 
  dplyr::select(kit_id, transect_location, completed) %>% 
  drop_na() %>% 
  full_join(metadata2) %>% 
  mutate(completed = case_when(is.na(completed) ~ FALSE, TRUE ~ completed)) %>% 
  pivot_wider(names_from = "transect_location", values_from = "completed") %>% 
  force()
