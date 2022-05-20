
## This is a data processing script for EXCHANGE, a sub-project of THE DOE-funded 
## COMPASS project (https://compass.pnnl.gov/). 
##
## This script imports raw data for Total Suspended Solids measured using
## a filtration apparatus at MCRL
## and exports clean, Level 1 QC'ed data.
## Data are read in from the COMPASS Google Drive.
## 
## Created: 2022-04-25
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
               ggplot2,
               dplyr,
               readr,
               tidyr,
               googlesheets4, # read_sheet 
               lubridate,
               gsw) # drive_upload

## Welcome
say("Welcome to EXCHANGE!", by = "random")

## URL for data
data_path <- "https://docs.google.com/spreadsheets/d/13MBZYTh3K8bsog27UZOx2mXUGflXMukapq7Zfsl1ooM/edit#gid=768038889" 
titrator_path <- "https://docs.google.com/spreadsheets/d/1lP4ft29oknaR5Xthv3Qo29EZeco6R3WkRkH-TT_x1vc/edit#gid=1069041223"
gsheet_tab <- "Sheet1"

## Define constants
f4_min = 4
f4_max = 10000

## Define analyte
var <- "TSS"

#
# 2. Import data ---------------------------------------------------------------
cat("Importing", var, "data...")

## read in titrator data
titrator <- read_sheet(ss = titrator_path, range = "Data")

## read in raw data
data_raw <- read_sheet(ss = data_path, range = gsheet_tab, skip = 5,
                       col_types = "DDccddddddccDDDc", na = c("--", "?", ""))

#
# 3. Process data --------------------------------------------------------------
cat("Processing", var, "data...")

data_raw %>% 
  drop_na(`Kit #`) %>% # remove blank rows used for visual separation
  rename(date_received = `Date received`, date_filtered = `Date filtered`,
         kit_id = `Kit #`) %>% 
  mutate(filter_mass_g = `Top filter + foil dry weight (g)` - `Top filter + foil tare weight (g)`#,
         #bottom_filter_g = `Bottom filter + foil dry weight (g)` - `Bottom filter + foil tare weight (g)`
         ) %>% 
  group_by(kit_id, IGSN) %>% 
  mutate(sample_weight_g = first(`Sample wt pre filtration (g)`) - last(`Sample wt post filtration (g)`)) %>% 
  summarise(total_filter_mass_g = sum(filter_mass_g, na.rm = TRUE), 
            #total_bottom_filter_g = sum(bottom_filter_g, na.rm = TRUE),
            sample_weight_g = first(sample_weight_g),
            filters_used = n()) -> processed_interim

processed_interim %>% 
  filter(grepl("Blank", kit_id)) %>% 
  ungroup() %>% 
  summarise(blank_avg_g = mean(total_filter_mass_g)) -> blank_avg_g

processed_interim %>% 
  filter(!grepl("Blank", kit_id)) %>% 
  bind_cols(blank_avg_g) %>% 
#  mutate(total_filter_mass_g = ifelse(total_bottom_filter_g > 0, 
#                                      total_filter_mass_g - total_bottom_filter_g,
#                                      total_filter_mass_g)) %>% 
  left_join(titrator, by = "kit_id") %>% 
  mutate(campaign = "EC1",
         transect_location = "Water",
         SP = gsw_SP_from_C(C = spcond_mscm, t = 18.5, p = 0),
         SA = gsw_SA_from_SP(SP, 0, longitude = -123.046, latitude = 48.078),
         CT= gsw_CT_from_t(SA, t = 18.5, 0),
         density_water = gsw_rho(SA, CT, 0),
         volume_filtered_ml = sample_weight_g / (density_water / 1000),
         tss_mg_perl = ((total_filter_mass_g / volume_filtered_ml) * 1000 * 1000) - blank_avg_g) -> data_processed
 
#
# 4. Apply QC flags ------------------------------------------------------------
cat("Applying flags to", var, "data...")

data_qc <- function(data) {
  data %>% 
    mutate(flag_1 = ifelse(total_filter_mass_g < 0, T, F),
           #flag_3 = ifelse(total_bottom_filter_g > 0, T, F),
           flag_2 = ifelse(tss_mg_perl < f4_min | tss_mg_perl > f4_max, T, F)) 
}

data_qc(data_processed) %>% 
  select(campaign, kit_id, transect_location, tss_mg_perl, total_filter_mass_g, volume_filtered_ml, filters_used,
         flag_1, flag_2) -> data_clean 

#
# 5. Write cleaned data to drive -----------------------------------------------

## We should add Sys.date or hardcode date so we know when the L0B was born
## The file written out should be named following 
## [Campaign]_[Analyte]_[QC_level]_[Date_of_creation_YYYYMMDD].csv
#drive_upload(media = data_clean, path = data_path)

write_csv(data_clean, "~/Desktop/EC1_TCTN_L0B_20220520.csv")
