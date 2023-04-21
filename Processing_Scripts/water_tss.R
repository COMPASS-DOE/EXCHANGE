
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
               googledrive, # drive_download
               lubridate,
               gsw) # drive_upload

## Welcome
say("Welcome to EXCHANGE!", by = "random")

## URL for data
data_path <- "https://docs.google.com/spreadsheets/d/13MBZYTh3K8bsog27UZOx2mXUGflXMukapq7Zfsl1ooM/edit#gid=768038889" 
titrator_path <- "https://docs.google.com/spreadsheets/d/1lP4ft29oknaR5Xthv3Qo29EZeco6R3WkRkH-TT_x1vc/edit#gid=1069041223"
gsheet_tab <- "Sheet1"
drive_download("https://drive.google.com/file/d/1GjHoUT9lrAjQzNHuLbwidJbMaGzRXUG2/view",
                              "temp.csv", overwrite = TRUE)

kit_metadata <- read_csv("temp.csv")
unlink("temp.csv")

## Define constants
F4_MIN = 4
F4_MAX = 10000
COND_MA <- 17 # average conductivity of mid-Atlantic sites
COND_GL <- 0.5 # average conductivity of great lake sites

## Define analyte
var <- "TSS"

#
# 2. Import data ---------------------------------------------------------------
cat("Importing", var, "data...")

## read in titrator data
read_sheet(ss = titrator_path, range = "Data") %>% 
  left_join(kit_metadata, by = "kit_id") %>% 
  # we had missing titrator data for conductivity, so we used average conductivity by region from the other
  # available titrator data
  mutate(avg_cond = case_when(region == "Greak Lakes" ~ COND_GL,
                              region == "Mid-Atlantic" ~ COND_MA),
         spcond_mscm = ifelse(is.na(spcond_mscm), avg_cond, spcond_mscm)) -> titrator

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
  filter(kit_id != "Blank 1") %>% # blank outlier
  ungroup() %>% 
  summarise(blank_avg_g = mean(total_filter_mass_g),
            blank_avg_vol_l = mean(sample_weight_g) / 1000) %>% 
  mutate(blank_avg_mg = blank_avg_g * 1000,
         tss_blank_mgl = blank_avg_mg / blank_avg_vol_l) -> blank_avg

processed_interim %>% 
  filter(!grepl("Blank", kit_id)) %>% 
  bind_cols(blank_avg) %>% 
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
         # have to convert ml to L and g to mg
         tss_mg_perl = ((total_filter_mass_g / volume_filtered_ml) * 1000 * 1000),
         # round final numbers to 2 decimal places
         tss_mg_perl = round(tss_mg_perl, 2),
         total_filter_mass_g = round(total_filter_mass_g, 2),
         volume_filtered_ml = round(volume_filtered_ml, 2)) -> data_processed
 
#
# 4. Apply QC flags ------------------------------------------------------------
cat("Applying flags to", var, "data...")

data_qc <- function(data) {
  data %>% 
    mutate(`negative filter mass` = ifelse(total_filter_mass_g < 0, T, F),
           `used average conductivity` = ifelse(kit_id %in% c("K014", "K057"), T, F),
           #flag_3 = ifelse(total_bottom_filter_g > 0, T, F),
           `outside range` = ifelse(tss_mg_perl < F4_MIN | tss_mg_perl > F4_MAX, T, F)
           ) 
}

data_qc(data_processed) %>% 
  pivot_longer(cols = c(`negative filter mass`,`outside range`, `used average conductivity`), names_to = "tss_flag",
               values_to = "vals") %>% 
  filter(vals == TRUE) %>% select(-vals) %>% 
  group_by(kit_id, transect_location) %>% 
  summarise(tss_flag = toString(tss_flag)) -> flags

data_processed %>% 
  left_join(flags, by = c("kit_id", "transect_location")) %>% 
  select(campaign, kit_id, transect_location, tss_mg_perl, total_filter_mass_g, 
         volume_filtered_ml, filters_used, tss_flag) -> data_clean 

# 5. Check with Metadata for missing: 

source("./Processing_Scripts/Metadata_kit_list.R") 

metadata_collected %>% 
  filter(sample_method == "bottle_1l")-> meta_filter 

data_clean %>% 
  mutate(transect_location = tolower(transect_location)) %>% 
  full_join(meta_filter, by = c("campaign", "kit_id", "transect_location")) %>% 
  mutate(tss_mg_perl = case_when(notes == "kit compromised" ~ NA,
                                 TRUE ~ tss_mg_perl),
         total_filter_mass_g = case_when(notes == "kit compromised" ~ NA,
                                         TRUE ~ total_filter_mass_g),
         volume_filtered_ml = case_when(notes == "kit compromised" ~ NA,
                                        TRUE ~ volume_filtered_ml),
         filters_used = case_when(notes == "kit compromised" ~ NA,
                                  TRUE ~ filters_used),
         tss_flag = case_when(notes == "sample compromised" ~ "sample compromised",
                              notes == "kit compromised" ~ "kit compromised",
                              TRUE ~ tss_flag)) %>% 
  select(-sample_type, -collected, -sample_method, - notes) -> tss_full

#
# 6. Write cleaned data to drive -----------------------------------------------

## We should add Sys.date so we know when the L1 was born
## The file written out should be named following 
## [Campaign]_[Analyte]_[QC_level]_[Date_of_creation_YYYYMMDD].csv

tss_full %>% write.csv(paste0("./ec1_water_tss_L1_", Sys.Date(), ".csv"), row.names = FALSE)

L1directory = "https://drive.google.com/drive/folders/1yhukHvW4kCp6mN2jvcqmtq3XA5niKVR3"

drive_upload(media = paste0("./ec1_water_tss_L1_", Sys.Date(), ".csv"), name= paste0("ec1_water_tss_L1_", Sys.Date(), ".csv"), path = L1directory)

file.remove(paste0("./ec1_water_tss_L1_", Sys.Date(), ".csv"))

