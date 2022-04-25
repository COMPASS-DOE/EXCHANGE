
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
naming_key_path <- "https://docs.google.com/spreadsheets/d/1IuGt6izCwXKJvg2Vr8Y_jtHAO_RZvaSBvdD-vp7_BSw/edit?usp=sharing"

## Define constants
f1_min <- 0
f1_max <- 100

## Define analyte
var <- "TC/TN"

# Create function to use in lapply that reads in a google sheet 
read_tctn <- function(x) {
  df <- read_sheet(ss = x, range = gsheet_tab, skip = 2)
}

#
# 2. Import data ---------------------------------------------------------------
cat("Importing", var, "data...")

# Read in naming key
read_sheet(ss = naming_key_path, range = "Sheet1") %>% 
  select(`Reassigned Sample ID`, `Original Instrument ID`) -> key

## read in raw data
all_files <- drive_ls(path = folder_path, pattern = "2022")
gsheet_files <- all_files[endsWith(all_files$name, "2022"),2]

lapply(gsheet_files$id, read_tctn) %>% 
  bind_rows() -> data_raw

#
# 3. Process data --------------------------------------------------------------
cat("Processing", var, "data...")

data_raw %>% 
  rename(Instrument_ID = "...1", Sample_ID = "...3",
         Nitrogen_Weight_perc = "Weight\n[%]...9",
         Carbon_Weight_perc = "Weight\n[%]...16") %>% 
  select(Instrument_ID, Sample_ID, Nitrogen_Weight_perc, Carbon_Weight_perc) %>% 
  filter(str_detect(Instrument_ID, "^EC1")) %>% # filter out blanks
  left_join(key, by = c("Instrument_ID" = "Original Instrument ID")) %>% 
  separate('Reassigned Sample ID', into = c("Campaign", "Kit_ID","Transect_Location", 
                                            "Acidification", "Set", "Run"), sep = "_") %>% 
  mutate(Transect_Location = case_when(Transect_Location == "WET" ~ "Wetland",                                                                               Transect_Location == "TRANS" ~ "Transition"),
         Acidification = case_when(Acidification == "UnAc" ~ FALSE)) %>% 
  separate(Instrument_ID, into = c("one", "two", "three", "Month", "Day"), sep = "_") %>% 
  mutate(Year = "2022", Date_Ran = make_date(day = Day, month = Month, year = Year)) %>% 
  select(Campaign, Kit_ID, Transect_Location, Nitrogen_Weight_perc, Carbon_Weight_perc,
         Acidification, Date_Ran, Set, Run) -> data_processed


#
# 4. Apply QC flags ------------------------------------------------------------
cat("Applying flags to", var, "data...")

data_qc <- function(data) {
  data %>% 
    mutate(  #a = round(a, n_sig_figs),
           f_1_N = ifelse(Nitrogen_Weight_perc < f1_min | Nitrogen_Weight_perc > f1_max, T, F),
           f_1_C = ifelse(Carbon_Weight_perc < f1_min | Carbon_Weight_perc > f1_max, T, F))
}

data_clean <- data_qc(data_processed)

#
# 5. Write cleaned data to drive -----------------------------------------------

## We should add Sys.date or hardcode date so we know when the L0B was born
## The file written out should be named following 
## [Campaign]_[Analyte]_[QC_level]_[Date_of_creation_YYYYMMDD].csv
drive_upload(media = data_clean, path = data_path)


