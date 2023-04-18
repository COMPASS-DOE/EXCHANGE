## EXCHANGE-FTICR
##
## This is a data processing script for EXCHANGE, a sub-project of the DOE-funded 
## COMPASS project (https://compass.pnnl.gov/). 
##
## This script imports and processes FTICR data obtained from Formularity 
## 
## Created: November 2022
## Kaizad F. Patel

## These functions have been modified from the `fticrrr` package and workflow: 
## https://github.com/kaizadp/fticrrr

################################################## #

# The functions for this script can be found in `Processing_Scripts/fticrrr-functions`
# See function description for more details

##############################
##############################


# 0. load packages --------------------------------------------------------
library(tidyverse)
library(googledrive)


# 1. SET input file paths -------------------------------
REPORT = "Data/fticr/fticr_surface_water.csv"
REPORT_path = "https://drive.google.com/drive/folders/1a3AzfT386Pf97fhO8nwdYCiT2GWv1p5_"

import_data = function(directory){
  
  ## a. Create a list of files to download
  files <- 
    drive_ls(directory) %>% 
    filter(grepl("fticr_surface_water.csv", name))
  
  ## b. Download files to local (don't worry, we'll delete em in a sec)
  lapply(files$id, drive_download, overwrite = TRUE)
  
  dat <- read.csv(files$name)
  
  
  ## c. pull a list of file names
  ## then read all files and combine
  # 
  # filePaths <- files$id
  # dat <- 
  #   do.call(rbind, lapply(filePaths, function(path){
  #     # then add a new column `source` to denote the file name
  #     df <- read.csv(files$name)
  #     #  df <- read.delim(path, skip = 2)
  #     df[["source"]] <- rep(path, nrow(df))
  #     df}))
  # 
  ## d. delete the temporary files
  file.remove(c(files$name))  
  
  ## e. output
  dat
}

## import the raw data files
REPORT = import_data(REPORT_path) %>% bind_rows()


# 2. source the functions --------------------------------------------------------
source("Processing_Scripts/fticrrr-functions/a-fticrrr-functions-initial-processing.R")

report = REPORT %>% rename(K001 = K001b, K004 = K004b)

fticr_meta = make_fticr_meta(report)$meta2
fticr_data = make_fticr_data_intensities(report)$data_samples_blank_corrected
fticr_blanks = make_fticr_data_intensities(report)$data_blanks

#

# # 3.  Export processed data -----------------------------------------------
# fticr_data %>% write.csv("Data/Processed/EC1_Water_FTICR_L2_20230308.csv", row.names = FALSE)
# fticr_meta %>% write.csv("Data/Processed/EC1_Water_FTICR_meta_L2_20230308.csv", row.names = FALSE)

# 3. Export processed data --------------------------------------------------

L1directory = "https://drive.google.com/drive/folders/1yhukHvW4kCp6mN2jvcqmtq3XA5niKVR3"

fticr_data %>% write.csv("./ec1_water_fticrms_l1.csv", row.names = FALSE)
fticr_meta %>% write.csv("./ec1_water_fticrms_meta_l2.csv", row.names = FALSE)

L2directory = "https://drive.google.com/drive/folders/1M-ASGuRoKqswiKbUWylWzoAyUmMPm367"


drive_upload(media = "./ec1_water_fticrms_l1.csv", name= "ec1_water_fticrms_l1.csv", path = L1directory)
drive_upload(media = "./ec1_water_fticrms_meta_l2.csv", name= "ec1_water_fticrms_meta_l2.csv", path = L2directory)

file.remove("./ec1_water_fticrms_l1.csv")
file.remove("./ec1_water_fticrms_meta_l2.csv")

# 4. Check with Metadata for missing:

import_l1_data = function(directory){
  
  ## a. Create a list of files to download
  files <- 
    drive_ls(directory) %>% 
    filter(name == "ec1_water_fticrms_L1.csv")
  
  ## b. Download files to local (don't worry, we'll delete em in a sec)
  lapply(files$id, drive_download, overwrite = TRUE)
  
  dat <- read.csv(files$name)
  
  ## d. delete the temporary files
  file.remove(c(files$name))  
  
  ## e. output
  dat
}



## import the L2 data files
fticr_data_l1 = import_l1_data(L1directory)

fticr_sample_list = colnames(fticr_data_l1)

fticr_sample_kits = as_tibble(fticr_sample_list) %>% 
  filter(stringr::str_detect(value, "K[0-9]{3}")) %>%
  mutate(kit_id= stringr::str_extract(value, "K[0-9]{3}"),
         campaign = "EC1",
         transect_location = "water",
         data_collected = TRUE) %>%
  dplyr::select(campaign, kit_id, transect_location, data_collected)


source("./Processing_Scripts/Metadata_kit_list.R")

metadata_collected %>%
  filter(sample_method == "bottle_1l") -> meta_filter

fticr_sample_kits %>%
  full_join(meta_filter, by = c("campaign", "kit_id", "transect_location")) %>%
  mutate(data_collected = case_when(is.na(data_collected) ~ FALSE,
                                    TRUE ~ data_collected),
         note = case_when(kit_id %in% c("K001","K007") ~ "kit compromised",
                          kit_id %in% c("K027") ~ "sample compromised",
                          kit_id %in% c("K044") ~ "sample not sent for analysis")
         ) %>%
  rename(sample_analyzed = data_collected) %>%
  select(campaign, kit_id, transect_location,sample_analyzed, note) %>%
  arrange(kit_id)-> fticr_sample_list

View(fticr_sample_list)

#save sample list
fticr_sample_list %>% write.csv("./ec1_water_fticrms_sample_list.csv", row.names = FALSE)
drive_upload(media = "./ec1_water_fticrms_sample_list.csv", name= "ec1_water_fticrms_sample_list.csv", path = L2directory)
file.remove("./ec1_water_fticrms_sample_list.csv")

#save L2 fticrms data 
fticr_data %>% select(-K001, -BLANK) %>% write.csv("./ec1_water_fticrms_l2.csv", row.names = FALSE)
drive_upload(media = "./ec1_water_fticrms_l2.csv", name= "ec1_water_fticrms_L2.csv", path = L2directory)
file.remove("./ec1_water_fticrms_l2.csv")

