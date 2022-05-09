
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
               lubridate) # drive_upload

## Welcome
say("Welcome to EXCHANGE!", by = "random")

## URL for data
data_path = "https://docs.google.com/spreadsheets/d/13MBZYTh3K8bsog27UZOx2mXUGflXMukapq7Zfsl1ooM/edit#gid=768038889" 
gsheet_tab <- "Sheet1"

## Define constants
a_min = int
a_max = int

## Define analyte
var <- "TSS"

#
# 2. Import data ---------------------------------------------------------------
cat("Importing", var, "data...")

## read in raw data
data_raw <- read_sheet(ss = data_path, range = gsheet_tab, skip = 5)

#
# 3. Process data --------------------------------------------------------------
cat("Processing", var, "data...")

data_raw %>% 
  drop_na(`Kit #`) %>% # remove blank rows used for visual separation
  rename(Date_Received = `Date received`, Date_Filtered = `Date filtered`,
         Kit_ID = `Kit #`) %>% 
  group_by(Kit_ID) %>% 



#
# 4. Apply QC flags ------------------------------------------------------------
cat("Applying flags to", var, "data...")

data_qc <- function(data) {
  data %>% 
    mutate(a = round(a, n_sig_figs)) %>% 
    mutate(f_a = a < a_min | a > a_max) 
}

data_clean <- data_qc(data_processed)

#
# 5. Write cleaned data to drive -----------------------------------------------

## We should add Sys.date or hardcode date so we know when the L0B was born
## The file written out should be named following 
## [Campaign]_[Analyte]_[QC_level]_[Date_of_creation_YYYYMMDD].csv
drive_upload(media = data_clean, path = data_path)

