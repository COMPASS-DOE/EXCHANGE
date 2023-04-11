# This script contains two parts: 
# 1) Generating sample list FOR the matlab input
# 2) Generating post-matlab output kit list for L2 data for publication

### 1) CDOM data <- generating sample lists for running through matlab ----

## Pre-processing step for CDOM data
## populating the SampleLog file, so it can be used in the MatLab script

library(tidyverse)

#  1a) create empty SampleLog template ----------

col_names = c("index",
              "Analysis",
              "Date",
              "Sample Description (60 Character Max) - Will display on final EEM figure",
              "Sample ID",
              "Analytical Replicate No",
              "Integration Time",
              "EEM file name",
              "Absorbance File Name",
              "Absorbance Pathlength",
              "Dilution Factor *Decimal Format…e.g. 1 part sample and 1 part water = 0.50",
              "DOC (mg/L)")

samplelog_empty = 
  as_tibble(matrix(nrow = 0, ncol = length(col_names)), 
            .name_repair = ~ col_names) %>% 
  mutate_all(as.character)

#
# 1b) extract metadata from file names ---- 

FILEPATH = "Data/cdom/EXCHANGE_Aqualog_Data"
filePaths <- list.files(path = FILEPATH, pattern = ".dat", full.names = TRUE, recursive = TRUE)
cdom_list = as_tibble(filePaths)

cdom_list_2 = 
  cdom_list %>% 
  mutate(filename = fs::path_file(value)) %>% 
  mutate(Date = str_extract(value, "/[0-9]{4}-[0-9]{2}-[0-9]{2}/"),
         Date = str_remove_all(Date, "/"),
         Kit_ID = str_extract(filename, "K[0-9]{3}"),
         Integration_Time = str_extract(filename, "[0-9]s"),
         Dilution = str_extract(filename, "[0-9]xd"),
         Dilution = parse_number(Dilution),
         Absorbance = case_when(grepl("Abs", filename) ~ filename),
       #  PEM = case_when(grepl("PEM", filename) ~ filename),
         EEM = case_when(grepl("Processed Graph_ RM_IFE_NRM", filename) ~ filename)
         ) %>% 
    dplyr::select(-value)
  
cdom_list_kitnames = 
  cdom_list_2 %>% 
  filter(!is.na(Kit_ID)) %>% 
  filter(!is.na(Integration_Time)) %>% 
  filter(!is.na(Absorbance) | !is.na(EEM)) %>% 
  dplyr::select(-filename) %>% 
  pivot_longer(-c(Kit_ID, Date, Integration_Time, Dilution)) %>% 
  filter(!is.na(value)) %>% 
  pivot_wider() %>% 
  rename(`Absorbance File Name` = Absorbance,
         `EEM file name` = EEM,
         `Dilution Factor *Decimal Format…e.g. 1 part sample and 1 part water = 0.50` = Dilution,
         ) %>% 
  mutate_all(as.character) 

samplelog_filled = 
  bind_rows(samplelog_empty, cdom_list_kitnames) %>% 
  dplyr::select(Kit_ID, everything()) %>% 
  arrange(Kit_ID)

#
# 1c) import DOC data ----
doc_data = read.csv("Data/Processed/EC1_NPOC_L0B_20220509_copyKFP.csv", na = "NA")
doc_data_subset = 
  doc_data %>% 
  dplyr::select(kit_id, npoc_mgl) %>% 
  rename(`DOC (mg/L)` = npoc_mgl,
         Kit_ID = kit_id)

samplelog_filled2 = 
  samplelog_filled %>% 
  dplyr::select(-`DOC (mg/L)`) %>% 
  left_join(doc_data_subset) %>% 
  mutate(`Sample Description (60 Character Max) - Will display on final EEM figure` = Kit_ID)
#
# 1d) export ----

samplelog_filled2 %>% write.csv("data/CDOM_SampleLog.csv", row.names = FALSE, na = "")

## 2022-05-23 KFP NOTE: this formats the list only when file names have the `K000`, `0s`, `0xdil` format
## the other files will be done next.

#### 2) Samples Check from Post-Processed Data ----

# 1. Setup ---------------------------------------------------------------------
require(pacman)
pacman::p_load(tidyverse,
               stringr,
               googlesheets4, # read_sheet 
               googledrive # drive_upload
)

## Welcome
say("Welcome to EXCHANGE!", by = "random")

#Absorbance and Fluorescence data are in their folders: 

abs_l2_directory = 'https://drive.google.com/drive/folders/1nAj4DPuFDwgc6hQbaQMzjUt1FZA72DGS'

abs_files <- drive_ls(abs_l2_directory) %>% 
  filter(grepl("Abs", name))

abs_list = as_tibble(abs_files) %>% dplyr::select(name) %>% 
  mutate(campaign = "EC1",
         transect_location = "water",
         kit_id= stringr::str_extract(name, "K[0-9]{3}"),
         data_collected = "TRUE") %>%
  dplyr::select(campaign, kit_id, transect_location, data_collected)

# 2. Check with Metadata for missing:

source("./Processing_Scripts/Metadata_kit_list.R")

#Water

metadata_collected %>%
  filter(sample_method == "vial_40ml") -> meta_filter

abs_list %>%
  full_join(meta_filter, by = c("campaign", "kit_id", "transect_location")) %>%
  filter(is.na(data_collected))-> check_these_abs

View(check_these_abs)

#Absorbance and Fluorescence data are in their folders: 

eems_l2_directory = 'https://drive.google.com/drive/folders/1TdX8y9B1y_AnA4Lvx8lLHYUqr8d3gofO'

eems_files <- drive_ls(eems_l2_directory) %>% 
  filter(grepl("RamNorm", name))

eems_list = as_tibble(eems_files) %>% dplyr::select(name) %>% 
  mutate(campaign = "EC1",
         transect_location = "water",
         kit_id= stringr::str_extract(name, "K[0-9]{3}"),
         data_collected = "TRUE") %>%
  dplyr::select(campaign, kit_id, transect_location, data_collected)

# 2. Check with Metadata for missing:

source("./Processing_Scripts/Metadata_kit_list.R")

#Water

metadata_collected %>%
  filter(sample_method == "vial_40ml") -> meta_filter

eems_list %>%
  full_join(meta_filter, by = c("campaign", "kit_id", "transect_location")) %>%
  filter(is.na(data_collected))-> check_these_eems

View(check_these_eems)

