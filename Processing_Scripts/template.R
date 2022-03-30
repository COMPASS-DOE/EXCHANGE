############ DELETE THIS HEADER FROM INSTRUMENT SCRIPTS ########################
## This script is a template to standardize the import and processing workflow
## for data streams associated with the EXCHANGE EC1 campaign, and not functional 
## code. Once we find a drive home for R scripts and QC'ed data, we shouldn't 
## need any local dependencies
############ DELETE THIS HEADER FROM INSTRUMENT SCRIPTS ########################


## This script imports raw data for [insert parameters of interest] measured using
## [insert relevant instrument and method details] at [insert location, eg. MCRL].
## and exports clean, Level 1 QC'ed data. [add additional relevant details]. 
## Data are read in from Google Sheets.
## 
## Parameters imported, their lower/upper limits of detection, and sig figs
## 
## Created: 2022-01-05
## Peter Regier
##
# #############
# #############


# 1. Setup ---------------------------------------------------------------------

# load packages
require(pacman)
pacman::p_load(tidyverse, 
       googlesheets4, # read_sheet 
       googledrive) # drive_upload

## URL for data
data_path = "xxxx"

# 2. Import data ---------------------------------------------------------------

## read in raw data
data_raw <- read_sheet("") %>% 
  rename(rename any weirdly named columns) %>% 
  mutate(create any new columns needed (eg calculations)) %>% 
  dplyr::select(columns needed)


# 3. QC data -------------------------------------------------------------------

clean_data <- function(data) {
  data %>% 
    filter(f_a = a < a_min | a > a_max) %>% 
    filter(etc)
}

data_clean <- clean_data(data_raw)

## clean up workspace to leave only the final dataframe
rm(list=setdiff(ls(), "x"))


# 4. Write cleaned data to drive -----------------------------------------------

drive_upload(media = data_clean, path = data_path)


