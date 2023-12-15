## This script imports raw data for iron measured colorimetrically by the ferrozine method
## Fe was extracted using HCl and then measured on a 96-well plate reader, with ferrozine as color reagent.
## Data are read in from Google Drive
## 
## Created: 2023-12-15
## Kaizad F. Patel
##

# 1. Setup ---------------------------------------------------------------------

# load packages
require(pacman)
pacman::p_load(tidyverse, # keep things tidy
               janitor, # useful for simplifying column names
               googlesheets4, # read_sheet 
               googledrive # drive_ functions
               )

## Set theme
theme_set(theme_bw())

## Set GDrive URL for plate reader raw data files
directory = "https://drive.google.com/drive/u/1/folders/1Q5xiW8xSZ5TUs5_kXyDV16CBGqCqx-of"

#
# 2. Download data --------------------------------------------------------
## there are three parts to the data:
## 1. sample weights
## 2. plate map/layout of samples
## 3. absorbance data from the plate reader

# download the sample weights
weights = googlesheets4::read_sheet("1hvORzjON18xEKG-vrBzZG0_GjX_-fHRXC40S5T23Kxs", 
                                    sheet = "weights", col_types = "c") 
  
# download the plate map
ferrozine_map = googlesheets4::read_sheet("1hvORzjON18xEKG-vrBzZG0_GjX_-fHRXC40S5T23Kxs", sheet = "plate_map") %>% mutate_all(as.character)
ferrozine_map[ferrozine_map == "NULL"] <- NA

# download the data files
## the files are stored as csv files on GoogleDrive, so we download and combine them
import_data = function(directory){
  
  ## a. Create a list of files to download
  files <- 
    drive_ls(directory) %>% 
    filter(grepl("csv", name))
  
  ## b. Download files to local (don't worry, we'll delete em in a sec)
  lapply(files$id, drive_download, overwrite = TRUE)
  
  ## c. pull a list of file names
  ## then read all files and combine
  
  filePaths <- files$name
  dat <- 
    do.call(bind_rows, lapply(filePaths, function(path){
      df <- read.csv(path, skip = 24, header = TRUE) %>% mutate_all(as.character) %>% janitor::clean_names()
      df = df %>% mutate(source = basename(path))
      df}))
  
  ## d. delete the temporary files
  file.remove(c(files$name))  
  
  ## e. output
  dat
}

## import the raw data files
raw_data = import_data(directory)

#

# 3. Process data ---------------------------------------------------------

map_processed = 
  ferrozine_map %>% 
  fill(plate, analysis, notes) %>% 
  mutate(plate = parse_number(plate)) %>% 
  pivot_longer(-c(plate, notes, letter, analysis), names_to = "number", values_to = "sample_label") %>% 
  drop_na() %>% 
  filter(letter != "letter") %>% 
  mutate(sample_type = case_when(grepl("ppm", sample_label) ~ "standard",
                                 grepl("blank", sample_label, ignore.case = T) ~ "blank",
                                 grepl("K[0-9]", sample_label) ~"sample"),
         well_position = paste0(letter, number)) %>% 
  rename(tray_number = plate) %>% 
  dplyr::select(tray_number, well_position, sample_label, sample_type, notes) %>% 
  mutate(standard_ppm = case_when(sample_type == "standard" ~ parse_number(sample_label)))

data_processed = 
  ferrozine_data %>% 
  mutate_all(na_if,"") %>% 
  dplyr::select(-x) %>% 
  fill(x_1) %>% 
  filter(x_2 == "562") %>% 
  dplyr::select(-x_2) %>% 
  pivot_longer(-c(source, x_1), values_to = "absorbance_562") %>% 
  mutate(name = str_remove(name, "x"),
         well_position = paste0(x_1, name),
         tray_number = str_extract(source, "tray|plate[1-9]+"),
         tray_number = parse_number(tray_number),
         absorbance_562 = as.numeric(absorbance_562)) %>% 
  dplyr::select(tray_number, well_position, absorbance_562) %>% 
  right_join(map_processed, by = c("tray_number", "well_position")) %>% 
  filter(!notes %in% "skip")

#
# 4. Apply calibrations ---------------------------------------------------
calibrate_ferrozine_data = function(data_processed){
  standards = 
    data_processed %>% 
    filter(grepl("standard", sample_type)) %>% 
    dplyr::select(tray_number, absorbance_562, standard_ppm) %>% 
    mutate(standard_ppm = as.numeric(standard_ppm))
  
  standards %>% 
    ggplot(aes(x = standard_ppm, y = absorbance_562, color = as.character(tray_number)))+
    geom_point()+
    geom_smooth(method = "lm", se = F)+
    facet_wrap(~tray_number)
  
  calibration_coef = 
    standards %>% 
    filter(tray_number == 2) %>% 
    dplyr::summarize(slope = lm(absorbance_562 ~ standard_ppm)$coefficients["standard_ppm"], 
                     intercept = lm(absorbance_562 ~ standard_ppm)$coefficients["(Intercept)"])
  
  # y = mx + c
  # abs = m*ppm + c
  # ppm = abs-c/m
  
  data_processed %>% 
    cross_join(calibration_coef) %>% 
    mutate(ppm_calculated = ((absorbance_562 - intercept) / slope))
  
}

samples = 
  calibrate_ferrozine_data(data_processed) %>% 
  filter(sample_type == "sample") %>% 
  dplyr::select(sample_label, ppm_calculated, notes) %>% 
  mutate(dilution = case_when(grepl("diluted 2x", notes) ~ 2,
                              TRUE ~ 1),
         # all samples were diluted 2x with HCl, and then some were diluted again
         # also apply the reduction efficiency correction (90 %)
         ppm_corrected = ppm_calculated * 2 * dilution * 100/90,
         ppm_corrected = round(ppm_corrected, 2)) 

# freeze-dried samples were used, so no need for a moisture correction
samples2 = 
  samples %>% 
  dplyr::select(sample_label, ppm_corrected) %>% 
  separate(sample_label, sep = "_", into = c("kit_id", "transect")) %>% 
  mutate(transect = case_match(transect, "U" ~ "upland", "T" ~ "transition", "W" ~ "wetland")) %>% 
  left_join(weights) %>% 
  mutate(ppm_corrected = as.numeric(ppm_corrected),
         weight_g = as.numeric(weight_g),
         HCl_mL = as.numeric(HCl_mL),
         Fe_ug_g = ppm_corrected * ((HCl_mL)/weight_g),
         Fe_ug_g = round(Fe_ug_g, 2)) %>% 
  dplyr::select(kit_id, transect, Fe_ug_g) %>% 
  mutate(transect = factor(transect, levels = c("upland", "transition", "wetland"))) %>% 
  arrange(kit_id, transect)

#
# 5. Export L0B data ------------------------------------------------------
write_csv(samples2, paste0("Data/Processed/EC1_Soil_iron_ferrozine_", Sys.Date(), ".csv"))


