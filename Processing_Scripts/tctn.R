
## This is a data processing script for EXCHANGE, a sub-project of THE DOE-funded 
## COMPASS project (https://compass.pnnl.gov/). 
##
## This script imports raw data for Total Carbon and Total Nitrogen measured using
## an Elemental Analyzer at MCRL
## and exports clean, Level 1 QC'ed data.
## Data are read in from the COMPASS Google Drive.
## 
## Created: 2022-04-01
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
               googledrive,
               stringr,
               lubridate) # drive_upload

## Welcome
say("Welcome to EXCHANGE!", by = "random")

## URL for data
folder_path <- "https://drive.google.com/drive/u/2/folders/1OVhQADClTIcfMtbJenoWfCD8fnODx_it" 
gsheet_tab <- "Summary Table"
naming_key_path <- "https://docs.google.com/spreadsheets/d/15o0bZ79WIOHlxZlaUxSv7pLYzuH39yP-d3GCE_gfk4c/edit#gid=363825852"
lod_path <- "https://docs.google.com/spreadsheets/d/14r_bVSGGxgM7f1ENuBFKC5t6UzP_uRgJQ2SPDMKOSoE/edit#gid=225278968"


## Define constants
f1_min <- 0
f1_max <- 100

## Define analyte
var <- "TC/TN"

# Create function to use in lapply that reads in a google sheet 
read_tctn <- function(x) {
  df <- read_sheet(ss = x, range = gsheet_tab, skip = 2, 
                   col_types = "ccccccccccccccccccc")
}

#
# 2. Import data ---------------------------------------------------------------
cat("Importing", var, "data...")

# Read in naming key
read_sheet(ss = naming_key_path, range = "Sheet1") %>% 
  select(`Reassigned Sample ID`, `Original Instrument ID`) -> key

# Read in LOD blanks
lod <- read_sheet(ss = lod_path, range = gsheet_tab, skip = 2, 
           col_types = "ccccccccccccccccccc")

## read in raw data
all_files <- drive_ls(path = folder_path, pattern = "2022")
gsheet_files <- all_files[endsWith(all_files$name, "2022"),2]

lapply(gsheet_files$id, read_tctn) %>% 
  bind_rows() -> data_raw

#
# 3. Process data --------------------------------------------------------------
cat("Processing", var, "data...")

# Process LOD data
lod %>% 
  rename(instrument_id = "...1", sample_id = "...3",
               total_nitrogen_mg = "Weight\n[mg]...8",
               total_carbon_mg = "Weight\n[mg]...15") %>% 
  left_join(key, by = c("instrument_id" = "Original Instrument ID")) %>% 
  separate(sample_id, into = c("type", "weight", "run"), sep = "_") %>% 
  select(type, weight, run, total_nitrogen_mg, total_carbon_mg) %>% 
  filter(weight == "0.1", type == "ACN") %>% 
  mutate(total_nitrogen_mg = as.numeric(total_nitrogen_mg),
         total_carbon_mg = as.numeric(total_carbon_mg)) %>% 
  summarise(lod_tn_average = mean(total_nitrogen_mg),
            lod_tn_sd = sd(total_nitrogen_mg),
            lod_tc_average = mean(total_carbon_mg),
            lod_tc_sd = sd(total_carbon_mg)) -> lod_processed

data_raw %>% 
  rename(instrument_id = "...1", sample_id = "...3",
         total_nitrogen_perc = "Weight\n[%]...9",
         total_nitrogen_mg = "Weight\n[mg]...8",
         total_carbon_perc = "Weight\n[%]...16",
         total_carbon_mg = "Weight\n[mg]...15") %>% 
  select(instrument_id, sample_id, total_nitrogen_perc, total_nitrogen_mg,
         total_carbon_perc, total_carbon_mg) %>% 
  filter(str_detect(instrument_id, "^EC1")) %>% # filter out blanks
  left_join(key, by = c("instrument_id" = "Original Instrument ID")) %>% 
  separate('Reassigned Sample ID', into = c("campaign", "kit_id","transect_location", 
                                            "acidification", "set", "run"), sep = "_") %>% 
  mutate(transect_location = case_when(transect_location == "WET" ~ "Wetland",
                                       transect_location == "TRANS" ~ "Transition",
                                       transect_location == "UPL" ~ "Upland"),
         acidification = case_when(acidification == "UnAc" ~ FALSE)) %>% 
  separate(instrument_id, into = c("one", "two", "three", "Month", "Day"), sep = "_") %>% 
  mutate(Year = "2022", date_ran = make_date(day = Day, month = Month, year = Year)) %>% 
  bind_cols(lod_processed) -> data_processed

#
# 4. Apply QC flags ------------------------------------------------------------
cat("Applying flags to", var, "data...")

data_qc <- function(data) {
  data %>% 
    mutate(  #a = round(a, n_sig_figs),
           tn_flag_1 = ifelse(total_nitrogen_perc < f1_min | total_nitrogen_perc > f1_max, T, F),
           tc_flag_1 = ifelse(total_carbon_perc < f1_min | total_carbon_perc > f1_max, T, F),
           tn_flag_2 = ifelse(total_nitrogen_mg < (lod_tn_sd * 3), T, F),
           tc_flag_2 = ifelse(total_carbon_mg < (lod_tc_sd * 3), T, F)
           )
}

data_qc(data_processed) %>% 
  select(campaign, kit_id, transect_location, total_nitrogen_perc, total_nitrogen_mg, 
         total_carbon_perc, total_carbon_perc, acidification, date_ran, set, run) -> data_clean

#
# 5. Write cleaned data to drive -----------------------------------------------

## We should add Sys.date or hardcode date so we know when the L0B was born
## The file written out should be named following 
## [Campaign]_[Analyte]_[QC_level]_[Date_of_creation_YYYYMMDD].csv
drive_upload(media = data_clean, path = data_path)


