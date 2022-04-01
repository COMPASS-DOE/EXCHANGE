
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
               googledrive) # drive_upload

## Welcome
say("Welcome to EXCHANGE!", by = "random")

## URL for data
folder_path <- "https://drive.google.com/drive/u/2/folders/1OVhQADClTIcfMtbJenoWfCD8fnODx_it" 
gsheet_tab <- "Summary Table"

## Define constants
f1_min <- 0
f1_max <- 100

## Define analyte
var <- "TC/TN"

read_tctn <- function(x) {
  read_sheet(ss = x, range = gsheet_tab)
}

#
# 2. Import data ---------------------------------------------------------------
cat("Importing", var, "data...")

## read in raw data
all_files <- drive_ls(path = folder_path, pattern = "2022")
gsheet_files <- all_files[endsWith(files, "2022"),2]

lapply(gsheet_files$id, read_tctn) %>% 
  bind_rows() -> data_raw

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


