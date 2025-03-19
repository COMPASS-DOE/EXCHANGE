## This script imports HYPROP processed data.
## Data are read in from Google Drive
## 
## This script only collates and processes the Van Genuchten parameters to generate a single csv.
## For QC of all samples, including water retention curves and graphs, see the corresponding QC report (https://github.com/COMPASS-DOE/EXCHANGE/blob/main/Processing_Scripts/soil_wrc_qc_report.Rmd)
## 
## Updated: 2025-02-12
## Kaizad F. Patel
##

# 1. Setup ---------------------------------------------------------------------

# load packages
require(pacman)
pacman::p_load(tidyverse, # keep things tidy
               janitor, # useful for simplifying column names
               googlesheets4, # read_sheet 
               googledrive, # drive_ functions
               readxl)

## Set theme
theme_set(theme_bw())

## Set GDrive URL for HYPROP data files
directory = "https://drive.google.com/drive/folders/18vcnFCtMJA2CaqwLHKeLEFGKTp90_yQ0"

#
# 2. Download data -------------------------------------------------------------
## download and import the Van Genuchten parameters

import_data = function(directory){
  
  ## a. Create a list of files to download
  files <- 
    drive_ls(directory) %>% 
    filter(grepl(".xlsx", name))
  
  ## b. Download files to local (don't worry, we'll delete em in a sec)
  lapply(files$id, drive_download, overwrite = TRUE)
  
  
  ## c. pull a list of file names
  ## then read all files and combine
  
  filePaths <- files$name
  dat = 
    do.call(bind_rows, lapply(filePaths, function(path){
      # then add a new column `source` to denote the file name
      df <- readxl::read_xlsx(path, sheet = "Fitting-Parameter value")
      df[["source"]] <- basename(path)
      
      df
    }))
  
  ## d. delete the temporary files
  file.remove(c(files$name))  
  
  ## e. output
  dat
   
}
wrc_parameters = import_data(directory)

#
# 4. Process data ---------------------------------------------------------

wrc_processed = 
  wrc_parameters %>% 
  separate(source, into = c("campaign", "kit_id", "transect_location"), sep = "_", remove = F) %>% 
  mutate(transect_location = str_remove(transect_location, ".xlsx"),
         transect_location = tolower(transect_location),
         transect_location = factor(transect_location, levels = c("upland", "transition", "wetland"))) %>% 
  filter(Parameter %in% c("alpha", "n", "th_r", "th_s")) %>% 
  arrange(kit_id, transect_location) %>% 
  dplyr::select(campaign, kit_id, transect_location, Parameter, Value) %>% 
  mutate(Value = str_remove(Value, "\\*"),
         Value = as.numeric(Value)) %>% 
  pivot_wider(names_from = "Parameter", values_from = "Value")

# 
# Missing sample check ---------------------------------------------------------

source("./Processing_Scripts/Metadata_kit_list.R")

metadata_collected %>% 
  filter(sample_method == "hyprop") -> meta_filter

wrc_processed %>% 
  full_join(meta_filter, by = c("campaign", "kit_id", "transect_location")) %>% 
  mutate(notes = case_when(kit_id == "K016" & transect_location == "upland" ~ "missing sample",
                           kit_id == "K023" & transect_location == "wetland" ~ "missing sample",
                           kit_id == "K023" & transect_location == "upland" ~ "missing sample",
                           kit_id == "K030" & transect_location == "wetland" ~ "missing sample",
                           kit_id == "K036" & transect_location == "upland" ~ "missing sample",
                           kit_id == "K050" & transect_location == "upland" ~ "missing sample",
                           kit_id == "K052" & transect_location == "upland" ~ "missing sample",
                           kit_id == "K055" & transect_location == "upland" ~ "missing sample",
                           kit_id == "K060" & transect_location == "wetland" ~ "missing sample",
                           kit_id == "K061" & transect_location == "upland" ~ "missing sample",
                           kit_id == "K047" & transect_location == "wetland" ~ "sample not run",
                           kit_id == "K048" & transect_location == "wetland" ~ "sample not run",
                           kit_id == "K052" & transect_location == "wetland" ~ "sample not run",
                           kit_id == "K017" & transect_location == "upland" ~ "curve inconsistent",
                           kit_id == "K034" & transect_location == "wetland" ~ "curve inconsistent",
                           kit_id == "K037" & transect_location == "wetland" ~ "curve inconsistent",
                           kit_id == "K039" & transect_location == "wetland" ~ "curve inconsistent",
                           kit_id == "K041" & transect_location == "transition" ~ "curve inconsistent",
                           kit_id == "K041" & transect_location == "upland" ~ "curve inconsistent",
                           kit_id == "K042" & transect_location == "transition" ~ "curve inconsistent",
                           kit_id == "K046" & transect_location == "upland" ~ "curve inconsistent",
                           kit_id == "K048" & transect_location == "transition" ~ "curve inconsistent",
                           kit_id == "K053" & transect_location == "transition" ~ "curve inconsistent",
                           kit_id == "K058" & transect_location == "transition" ~ "curve inconsistent",
                           collected == FALSE & is.na(alpha) ~ "sample not collected",
                           .default = notes)) %>% 
  select(-c("sample_type", "collected", "sample_method")) -> wrc_full

wrc_full %>% write.csv("Data/Processed/EC1_soil_wrc_2025-02-10.csv", na = "", row.names = F)
