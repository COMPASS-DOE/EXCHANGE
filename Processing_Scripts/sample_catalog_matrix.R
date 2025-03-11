
## This is a data processing script for EXCHANGE, a sub-project of THE DOE-funded 
## COMPASS project (https://compass.pnnl.gov/). 
##
## This script creates a sample catalog for a each analyte file in EC1, 
## flags provided for any non-available sample data. 
## If data is provided and text reads anything other than "data available", 
## this constitutes warnings provided for any samples diverging from normal protocols 
## (e.g. less than 3 replicate values used for total carbon). 
## Values "not applicable" mean that the data type is not applicable for the sample type
## 
## Created: 2022-04-27
## Allison Myers-Pigg and Stephanie Pennington
##

# 1. Setup ---------------------------------------------------------------------
cat("Setup")

# load packages
require(pacman)
pacman::p_load(cowsay, tidyverse, googlesheets4, googledrive, purrr, stringr)

## Welcome
say("Welcome to EXCHANGE!", by = "random")

## Define analyte
var <- "sample inventory"

## URL for data
L1directory <- "https://drive.google.com/drive/folders/1yhukHvW4kCp6mN2jvcqmtq3XA5niKVR3"

# 2. Import data ---------------------------------------------------------------
cat("Importing", var, "data...")

files <-
  drive_ls(L1directory, recursive = TRUE) %>%
  filter(grepl("L1", name))

## b. Download files to local (don't worry, we'll delete em in a sec)
lapply(files$id, drive_download, overwrite = TRUE)

csvs_2 <- list.files("./", pattern = "soil|water", full.names = TRUE) |>
  set_names() |>
  map_dfr(read_delim, .id = "file") %>% 
  mutate()

file.remove(c(files$name))  

source("./Processing_Scripts/Metadata_kit_list.R")

analyte_meta <- read_sheet("https://docs.google.com/spreadsheets/d/1MKsWaZBcKJIj8mO8yB_5pcZ4DUofj11eCIviOeLPZWc/edit#gid=0")

#csvs <- list.files("./", pattern = "soil|water", full.names = TRUE)

#data <- lapply(csvs, read_csv)

sample_catalog <- csvs_2 %>% 
  mutate(data_type = (stringr::str_split(file,"_",simplify=TRUE)[,3])) %>%
  mutate(data_type = case_when(data_type == "bulk" ~ "bulkdensity",
                               data_type =="ph" ~ "soilphspc",
                               data_type == "gwc" & transect_location == "sediment" ~ "sediment_gwc", #separate sediment and gwc columns
                               data_type == "gwc" & transect_location %in% c("upland", "transition", "wetland") ~ "soil_gwc",
                               data_type == "visual" & transect_location != "sediment" ~ "soil_vizmetrics",
                               TRUE ~ data_type)) %>% 
  select(data_type, campaign, kit_id, transect_location, note, contains("_flag"), notes_flags, notes) %>% 
  pivot_wider(id_cols=c(campaign, kit_id, transect_location), names_from = data_type, values_from = c(note, contains("_flag"), notes_flags, notes)) %>%
  janitor::remove_empty(which = "cols") %>%
  rename(ec1_water_cdom_L2 = note_cdom,
         ec1_water_fticrms_L2 = note_fticrms,
         ec1_water_alkalinity_L2.csv = alk_flag_waterquality,
         ec1_water_doc_L2.csv = doc_flag_doc,
         ec1_water_orp_L2.csv = orp_flag_waterquality,
         ec1_water_ph_L2.csv = ph_flag_waterquality,
         ec1_water_salinity_L2.csv = sal_flag_waterquality,
         ec1_water_tdn_L2.csv = tdn_flag_tdn,
         ec1_water_tss_L2.csv = tss_flag_tss,
         ec1_soil_bulk_density_L2.csv = bulk_density_flag_bulkdensity,
         ec1_soil_cond_L2.csv = specific_conductance_flag_soilphspc,
         ec1_soil_ph_L2.csv = ph_flag_soilphspc,
         ec1_soil_tc_L2.csv = tc_flag_tctn,
         ec1_soil_tn_L2.csv = tn_flag_tctn,
         ec1_soil_gwc_L2.csv = gwc_flag_soil_gwc,
         ec1_sediment_gwc_L2.csv = gwc_flag_sediment_gwc,
         ec1_soil_iron_L2.csv = notes_iron,
         ec1_soil_cations_L2.csv = notes_flags_cations,
         ec1_soil_visual_metrics_L2.csv = notes_soil_vizmetrics,
         ec1_soil_mineralogy_L2.csv = notes_mineralogy,
         ec1_soil_texture_L2.csv = notes_texture,
         ec1_soil_wrc_L2.csv = notes_wrc) %>%
  pivot_longer(cols = starts_with("ec1"), names_to = "analyte", values_to = "status") %>%
  left_join(analyte_meta, by = "analyte") %>% 
  mutate(status = case_when(grepl("soil", analyte) & transect_location %in% c("water", "sediment") ~ "not applicable", #not sure what we want to call something like this
                            grepl("water", analyte) & transect_location != "water" ~ "not applicable",
                            grepl("sediment", analyte) & transect_location != "sediment" ~ "not applicable",
                            TRUE ~ status)) %>% 
  left_join(metadata_collected, by = c("campaign", "kit_id", "transect_location", "sample_method")) %>% #we need to take into account sample_method, need to know which analytes used which method
  mutate(status = case_when(collected == TRUE & is.na(status) ~ "data available",
                            TRUE ~ status),
         transect_location = factor(transect_location, levels = c("upland", "transition", "wetland", "sediment", "water"))) %>% 
  select(-sample_type, -sample_method, -collected, -notes) %>% 
  distinct() %>% 
  pivot_wider(names_from = analyte, values_from = status) %>% 
  arrange(kit_id, transect_location)

sample_catalog %>% write.csv(paste0("./ec1_sample_catalog.csv"), row.names = FALSE)

# V3 Package Directory
directory <- "https://drive.google.com/drive/u/1/folders/1hB3S06ZVovPvzU-6Yp9VIOzQxuPigubG"

drive_upload("./ec1_sample_catalog.csv", name= "ec1_sample_catalog.csv", path = directory)

file.remove("./ec1_sample_catalog.csv")



