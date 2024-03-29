## This script imports raw data for cations measured using an ICP.
## Data are read in from Google Drive
## 
## Created: 2023-12-07
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

## Set GDrive URL for CEC raw data files
directory = "https://drive.google.com/drive/u/1/folders/1VoHcMv0QrIIMW4_G4tLYhvrnuXfo0aNU"

#
# 3. Download data -------------------------------------------------------------

## Create function: 
import_data = function(directory){
  
  ## a. Create a list of files to download
  files <- 
    drive_ls(directory) %>% 
    filter(grepl("_icp_results", name))
  
  ## b. Download files to local (don't worry, we'll delete em in a sec)
  lapply(files$id, drive_download, overwrite = TRUE)
  
  
  ## c. pull a list of file names
  ## then read all files and combine
  
  filePaths <- files$name
  dat <- 
    do.call(rbind, lapply(filePaths, function(path){
      # then add a new column `source` to denote the file name
      df <- readxl::read_excel(path, skip = 3)
      #  df <- read.delim(path, skip = 2)
      df[["source"]] <- rep(path, nrow(df))
      df}))
  
  ## d. delete the temporary files
  file.remove(c(files$name))  
  
  ## e. output
  dat
}

## import the raw data files
raw_data = import_data(directory)

## load sample weights
sample_weights = read_sheet("15yM3DPcsUxh2fEvZrvcNkfu2etDxdwj5eXbVSRGRzV4", sheet = "sample_key")

# load sample key
sample_key = read.csv("Data/EC1_Metadata_KitLevel.csv")

#
# 4. Process data ---------------------------------------------------------
## processing involves:
## 1. applying dilution correction
## 2. normalizing to soil mass (ug/g)
## 3. applying flags for analytes that were below detect
## 4. calculating CEC (in meq/100g)

## 4a. normalize to soil mass ----
processed_data = 
  raw_data %>% 
  mutate_at(vars(-c(Sample)), as.numeric) %>% 
  mutate(dilution_factor = mL_final/mL_of_sample,
         dilution_factor = round(dilution_factor, 2)) %>% 
  # select only the cations (base cations + Al)
  dplyr::select(Sample, Ca, Mg, Na, K, Al, dilution_factor) %>% 
  force()

# clean up the samples
# normalize to soil mass
samples = 
  processed_data %>% 
  mutate(Sample = as.numeric(Sample)) %>% 
  filter(!is.na(Sample)) %>% 
  mutate(Sample = str_pad(Sample, 3, "0", side = "left"),
         Sample = paste0("EC1_ICP_", Sample)) %>% 
  rename(analysis_ID = Sample) %>% 
  pivot_longer(cols = -c(analysis_ID, dilution_factor), names_to = "element", values_to = "mgL") %>% 
  filter(!is.na(mgL)) %>% 
  # because all the NAs were replaced by 0, switch back to NA
  mutate(mgL = na_if(mgL, 0)) %>% 
  mutate(mgL_corr = mgL * dilution_factor) %>% 
  left_join(sample_weights) %>% 
  dplyr::select(-Sample, -note) %>% 
  # ug/g = (mg/L * vol/wt)/1000
  mutate(mgL_corr = round(mgL_corr, 2),
         mgg = mgL * (vol_extractant_mL / wt_soil_g) * (1/1000),
         ug_g = mgg * 1000, 
         ug_g = signif(ug_g, 5)
  ) %>% 
  mutate(skip = (analysis_ID == "EC1_ICP_137" & element == "K")) %>% 
  filter(!skip) %>% 
  dplyr::select(analysis_ID, kit_id, transect, element, mgL, ug_g) 

#
## 4b. set flags ----
# set a flag for all the elements that are below detection
# some samples have one, some samples have multiple. 
# setting individual flags and then concatenate using `toString()`
# later, add this to the data
samples_flag = 
  samples %>% 
  mutate(flag = case_when(is.na(mgL) ~ element)) %>% 
  dplyr::select(analysis_ID, flag) %>% 
  drop_na() %>% 
  group_by(analysis_ID) %>% 
  dplyr::summarise(flag = toString(flag),
                   flag = paste(flag, "below detect")) %>% 
  ungroup()

#
## 4c. CEC calculations ----
## CEC is calculated in terms of meq/100g
## for this we need charge and atomic weight per cation 

charges = 
  tribble(
    ~element, ~charge, ~atomic_wt,
    "Na", 1, 23,
    "K", 1, 39,
    "Ca", 2, 40,
    "Mg", 2, 24,
    "Al", 3, 27
  )

samples_meq = 
  samples %>% 
  dplyr::select(analysis_ID, kit_id, transect, element, ug_g) %>% 
  left_join(charges) %>% 
  mutate(
    meq_100g = ug_g * charge/atomic_wt * 100/1000,
    meq_100g = round(meq_100g, 2)) %>% 
  filter(!is.na(meq_100g)) %>% 
  group_by(analysis_ID) %>% 
  pivot_longer(cols = c(ug_g, meq_100g)) %>% 
  mutate(element = paste0(element, "_", name)) 

# calculate CEC as sum of all extractable cations
cec = 
  samples_meq %>% 
  filter(name == "meq_100g") %>% 
  group_by(analysis_ID) %>% 
  dplyr::summarise(CEC_meq_100g = sum(value),
                   CEC_meq_100g = round(CEC_meq_100g, 2))

# now combine cations and cec
cations_and_cec = 
  samples_meq %>% 
  dplyr::select(-c(charge, atomic_wt, name)) %>% 
  pivot_wider(names_from = "element", values_from = "value") %>% 
  rename(transect_location = transect) %>%
  mutate(campaign = "EC1") %>% 
  dplyr::select(campaign, analysis_ID, kit_id, transect_location, ends_with("meq_100g")) %>% 
  left_join(cec) %>% 
  left_join(samples_flag) %>% 
  ungroup() %>% 
  dplyr::select(-analysis_ID)

# 11. Clean data  --------------------------------------------------------------

cations_and_cec %>% 
  # switch wetland and transition names due to a...
  # ...sampling error: wetland soil was sampled and put into a jar labeled "transition" incorrectly
  mutate(transect_location = case_when(kit_id == "K046" & transect_location == "transition" ~ "wetland", 
                                       kit_id == "K046" & transect_location == "wetland" ~ "transition", 
                                       TRUE ~ transect_location)) -> data_clean

# 12. Check with Metadata for missing samples  ---------------------------------

source("./Processing_Scripts/Metadata_kit_list.R")

metadata_collected %>%
  filter(sample_method == "jar") -> meta_filter

data_clean %>% 
  full_join(meta_filter, by = c("campaign", "kit_id", "transect_location")) %>% 
  mutate(notes = case_when(kit_id == "K050" & transect_location == "upland" ~ "not enough material for extraction",
                           kit_id == "K024" & transect_location == "wetland" ~ "sample compromised",
                           collected == FALSE ~ "sample not collected",
                  TRUE ~ notes),
         notes_flags = case_when(is.na(notes) ~ flag,
                           TRUE ~ notes)) -> full

nums <- sapply(full, is.numeric)           # identify numeric columns
full[!is.na(full$notes), which(nums)] <- NA  # set compromised kits to NA

full %>% select(campaign, kit_id, transect_location, contains("meq_100"), notes_flags) -> cations_l1

#
# 13. Write L0B data -----------------------------------------------------------
write_csv(cations_l1, paste0("~/Documents/ec1_soil_cations_L1_", Sys.Date(), ".csv"))

## extras ----
# testing
# join sample key
cations_and_cec %>% 
  pivot_longer(cols = -c(kit_id, transect, flag)) %>% 
  left_join(sample_key %>% dplyr::select(kit_id, region)) %>% 
  ggplot(aes(x = name, y = value, color = region))+
  geom_jitter(width = 0.2)+
  facet_wrap(~region, scales = "free")
