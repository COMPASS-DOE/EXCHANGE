## This script imports raw data for soil texture measured using the hydrometer method.
## Data are read in from Google Drive
## 
## Created: 2024-11-13
## Kaizad F. Patel
##

# 1. Setup ---------------------------------------------------------------------

# load packages
require(pacman)
pacman::p_load(cowsay,
               tidyverse, # keep things tidy
               janitor, # useful for simplifying column names
               googlesheets4, # read_sheet
               googledrive
)

## Welcome
say("Welcome to EXCHANGE!", by = "random")

## Define analyte
var <- "soil mineralogy"


## URL for data
data_path = "https://docs.google.com/spreadsheets/d/1OGJQutImSzMqGYH-lOnrB3DsFLQXBnP3/"

#
# 2. Import data ---------------------------------------------------------------

import_xrd = function(data_path){
  
  drive_download(data_path, path = "xrd_temp.xlsx", overwrite = T)
 
  dat = readxl::read_xlsx("xrd_temp.xlsx")
  
  file.remove("xrd_temp.xlsx")  
  
  dat 
}  
xrd_data = import_xrd(data_path)

#
# 3. Process data ---------------------------------------------------------

xrd_processed = 
  xrd_data %>% 
  mutate_all(as.character) %>% 
  filter(is.na(skip)) %>% 
  dplyr::select(-skip) %>% 
  pivot_longer(-c(File), names_to = "mineral", values_to = "percent") %>% 
  filter(!grepl("\\...", mineral)) %>% 
  mutate(percent = as.numeric(percent),
         percent = round(percent, 4)) %>% 
  arrange(mineral) %>% 
  pivot_wider(names_from = "mineral", values_from = "percent") %>% 
  replace(.,is.na(.),0) %>% 
  separate(File, sep = "_", into = c("kit_id", "transect_location")) %>% 
  mutate(transect_location = tolower(transect_location),
         campaign = "EC1") %>% 
  dplyr::select(campaign, kit_id, transect_location, everything()) %>% 
  arrange(kit_id, transect_location) %>% 
  force() %>% 
  # switch wetland and transition names due to a...
  # ...sampling error: wetland soil was sampled and put into a jar labeled "transition" incorrectly
  mutate(transect_location = case_when(kit_id == "K046" & transect_location == "transition" ~ "wetland", 
                                       kit_id == "K046" & transect_location == "wetland" ~ "transition", 
                                       TRUE ~ transect_location)) 

# Make all columns headers lowercase
names(xrd_processed) <- tolower(names(xrd_processed))

# 
# Missing sample check ---------------------------------------------------------

source("./Processing_Scripts/Metadata_kit_list.R")

metadata_collected %>% 
  filter(sample_method == "jar", sample_type == "soil") -> meta_filter

xrd_processed %>% 
  full_join(meta_filter, by = c("campaign", "kit_id", "transect_location")) %>% 
  mutate(notes = case_when(collected == FALSE & is.na(albite) ~ "sample not collected",
                           collected == TRUE & is.na(notes) & is.na(albite) ~ "sample not analyzed",
                          .default = notes)) %>% 
  select(-c("sample_type", "collected", "sample_method")) -> xrd_full

xrd_full %>% write.csv("Data/Processed/ec1_soil_mineralogy_2025-02-28.csv", na = "", row.names = F)
