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

download_directory <- "https://drive.google.com/drive/u/1/folders/1M-ASGuRoKqswiKbUWylWzoAyUmMPm367"
save_directory <- "./Data/IGSN/"

## Identify data files
regex <- "^ec1_soil.*\\.csv$"

files <- drive_ls(download_directory, recursive = TRUE) %>% 
  filter(grepl(regex, name)) %>% 
  filter(!name %in% c("ec1_water_fticrms_L2.csv", "ec1_water_fticrms_meta_L2.csv"))


# 2. Process -------------------------------------------------------------------
add_igsn <- function(filename) {

    read_csv(filename) %>% 
    mutate(file_name = filename) %>% 
    left_join(mapping, by = "file_name") -> t

  # need to figure out how to index into mapping based on filename
  if(mapping$multiple_ids[mapping$file_name == filename] == TRUE) {
    multiple %>% 
      filter(file_name == filename) -> multiple_file
    
    t %>% 
      left_join(multiple_file, by = c("campaign", "kit_id", "transect_location", "file_name")) %>% 
      mutate(mapping_id = case_when(!is.na(new_mapping_id) ~ new_mapping_id,
                                    .default = mapping_id)) %>% 
      select(-new_mapping_id) -> t
    
  }
  t %>% 
    mutate(sample_name = toupper(paste(campaign, kit_id, transect_location, mapping_id, sep = "_"))) %>% 
    left_join(igsn, by = "sample_name") %>% 
    select(-c(file_name, multiple_ids, mapping_id, file_path, sample_name)) %>% 
    relocate(igsn, .before = campaign) %>% 
    write_csv(file = paste0(save_directory, file_name))
}

for(i in 1:nrow(files)) {
  
  drive_download(files$id[i], overwrite = TRUE)
  
  file_name <- paste(files$name[i])
  
  add_igsn(file_name)
  
  file.remove(file_name)
}


