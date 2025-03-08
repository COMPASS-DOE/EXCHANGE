## This is a data processing script for EXCHANGE, a sub-project of THE DOE-funded 
## COMPASS project (https://compass.pnnl.gov/). 
##
## This script adds IGSNs to existing EC1 datasets.
## 
## Created: 2025-03-07
## Stephanie Pennington | stephanie.pennington@pnnl.gov
##
# ############# #

# 1. Setup ---------------------------------------------------------------------

# load packages
require(pacman)
pacman::p_load(cowsay,
               tidyverse,
               googlesheets4, # read_sheet 
               googledrive # drive_upload
)

## Welcome
say("Welcome to EXCHANGE!", by = "random")

## Load files
sample_kit <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1eidz4hdJqOQYucTWFrDOwtQGXWfway9iZh-fmsElE1w/edit?usp=sharing", "sample_kit")

mapping <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1eidz4hdJqOQYucTWFrDOwtQGXWfway9iZh-fmsElE1w/edit?usp=sharing", "mapping")

multiple <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1eidz4hdJqOQYucTWFrDOwtQGXWfway9iZh-fmsElE1w/edit?usp=sharing", "multiple")

igsn <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1eidz4hdJqOQYucTWFrDOwtQGXWfway9iZh-fmsElE1w/edit?usp=sharing", "igsn")

download_directory <- "https://drive.google.com/drive/u/1/folders/1MvCS-u6S47XbVLvTGZ-V7RXTwF6Nb98k"
upload_directory <- ""

## Identify data files
regex <- "^ec1_(soil|sediment|water).*\\.csv$"

files <- drive_ls(directory, recursive = TRUE) %>% 
  filter(grepl(regex, name)) %>% 
  filter(!name %in% c("ec1_water_fticrms_L2.csv", "ec1_water_fticrms_meta_L2.csv"))


# 2. Process -------------------------------------------------------------------
add_igsn <- function(files) {
  
  drive_download() 
  
  read_csv(file) %>% 
    mutate(file_name = name) %>% 
    left_join(mapping, by = "file_name") %>% 
    mutate(sample_name = toupper(paste(campaign, kit_id, transect_location, mapping_id, sep = "_"))) %>% 
    left_join(igsn, by = "sample_name") %>% 
    select(-c(file_name, multiple_ids, mapping_id, file_path, sample_name)) %>% 
    relocate(igsn, .before = campaign)
  
}


