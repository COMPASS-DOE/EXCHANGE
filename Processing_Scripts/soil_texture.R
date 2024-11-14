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

## read in raw data
data_raw <- googlesheets4::read_sheet(data_path, col_types = "c")

#
# 3. Process data ---------------------------------------------------------

# function to compute % sand-silt-clay from the hydrometer readings
compute_soil_texture = function(dat){
  
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
  compute_soil_texture()
