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
  dplyr::select(-starts_with("..."), -starts_with("mL"), -S, -source) %>% 
  rename(S = `S (corrected)`) %>% 
  force()

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
  mutate(mgL_corr = round(mgL_corr, 2),
         mgg = mgL * (vol_extractant_mL / wt_soil_g) * (1/1000),
         ug_g = mgg * 1000, 
         ug_g = signif(ug_g, 5),
         #ug_g = formatC(ug_g, digits = 5)
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
## 4c. finalize dataset for ug/g ----
samples_wide = 
  samples %>% 
  dplyr::select(-mgL) %>% 
  mutate(element = paste0(element, "_ug_g")) %>% 
  pivot_wider(names_from = "element", values_from = "ug_g") %>% 
  left_join(samples_flag) %>% 
  dplyr::select(-analysis_ID)

#
## 4b. CEC calculations ----
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
  #drop_na() %>% 
  mutate(#ion = paste0(ion, "_meq_100g"), 
    meq_100g = ug_g * charge/atomic_wt * 100/1000,
    meq_100g = round(meq_100g, 2)) %>% 
  filter(!is.na(meq_100g)) %>% 
  group_by(analysis_ID) %>% 
  #dplyr::mutate(CEC_meq_100g = sum(meq_100g)) %>% 
  pivot_longer(cols = c(ug_g, meq_100g)) %>% 
  mutate(element = paste0(element, "_", name)) 

cec = 
  samples_meq %>% 
  filter(name == "meq_100g") %>% 
  group_by(analysis_ID) %>% 
  dplyr::summarise(CEC_meq_100g = sum(value))

cations_and_cec = 
  samples_meq %>% 
  dplyr::select(-c(charge, atomic_wt, name)) %>% 
  pivot_wider(names_from = "element", values_from = "value") %>% 
  dplyr::select(analysis_ID, kit_id, transect, ends_with("meq_100g")) %>% 
  left_join(cec) %>% 
  left_join(samples_flag)


#
# 11. Write L0B data -----------------------------------------------------------

write_csv(samples_wide, paste0("Data/Processed/EC1_Soil_ICP_ugg_L0B_", Sys.Date(), ".csv"))
write_csv(cations_and_cec, paste0("Data/Processed/EC1_Soil_ICP_CEC_L0B_", Sys.Date(), ".csv"))




# testing
# join sample key
samples %>% 
  left_join(sample_key %>% dplyr::select(kit_id, region)) %>% 
  ggplot(aes(x = region, y = ug_g, color = region))+
  geom_jitter()+
  facet_wrap(~element, scales = "free")
