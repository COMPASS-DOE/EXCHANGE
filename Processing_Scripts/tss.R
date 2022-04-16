############ DELETE THIS HEADER FROM INSTRUMENT SCRIPTS ###################### #
## This script is a template to standardize the import and processing workflow
## for data streams associated with the EXCHANGE EC1 campaign, and not functional 
## code. Once we find a drive home for R scripts and QC'ed data, we shouldn't 
## need any local dependencies.

## All variables for EXCHANGE datasets need to have units appended. Please 
## follow 
############ DELETE THIS HEADER FROM INSTRUMENT SCRIPTS ###################### #


## This is a data processing script for EXCHANGE, a sub-project of THE DOE-funded 
## COMPASS project (https://compass.pnnl.gov/). 
##
## This script imports raw data for [insert parameters of interest] measured using
## [insert relevant instrument and method details] at [insert location, eg. MCRL].
## and exports clean, Level 1 QC'ed data. [add additional relevant details]. 
## Data are read in from the COMPASS Google Drive.
## 
## Created: YYYY-MM-DD
## [Your name here!]
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
data_path = "xxxx" 

## Define constants
a_min = int
a_max = int

## Define analyte
var <- "TSS"

#
# 2. Import data ---------------------------------------------------------------
cat("Importing", var, "data...")

## read in raw data
data_raw <- read_sheet("") 

#
# 3. Process data --------------------------------------------------------------
cat("Processing", var, "data...")

data_processed <- data_raw %>% 
  rename(rename any weirdly named columns) %>% 
  mutate(create any new columns needed (eg calculations)) %>% 
  dplyr::select(columns needed)

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

