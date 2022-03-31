## This script imports raw data for GWC and LOI measured at PNNL MCRL and exports 
## clean, Level 1 QC'ed data. [add additional relevant details]. 
## Data are read in from Google Sheets.
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
               cowplot,
               grattantheme, 
               googlesheets4, # read_sheet 
               googledrive) # drive_upload

## set ggplot theme
theme_set(theme_bw())

## URL for data
loi_path = "https://docs.google.com/spreadsheets/d/1Osig5zxzW3l9z_1Bb0zNW2tfTdJ60hsh78qMvjgclQE/edit#gid=0"


# 2. Import data ---------------------------------------------------------------

## read in raw data
loi_raw <- read_sheet(loi_path) %>% 
  mutate(kit_id = str_match(sample_id, "K0\\d\\d")[,1], 
         site = str_to_title(str_match(sample_id, "[:alpha:]{6,10}"))) %>% 
  dplyr::select(kit_id, site, gwc, loi)


# 3. QC data -------------------------------------------------------------------

clean_data <- function(data) {
  data %>% 
    filter(gwc > 0 | is.na(loi)) %>% 
    filter(loi > 0 | is.na(loi))
}

loi <- clean_data(loi_raw)


# 4. Write out dataset ---------------------------------------------------------

## Update with EC1 nomenclature when data are complete
write_csv(loi, "Data/gwc_loi_provisional.csv")
