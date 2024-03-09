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
                                    sheet = "weights", col_types = "c") %>% 
  filter(!is.na(weight_g))
  
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
  fill(date, plate, analysis, notes) %>% 
  mutate(plate = parse_number(plate),
         date = ymd(date)) %>% 
  pivot_longer(-c(date, plate, notes, dilution, letter, analysis), names_to = "number", values_to = "sample_label") %>% 
  drop_na() %>% 
  filter(letter != "letter") %>% 
  mutate(sample_type = case_when(grepl("mM", sample_label) ~ "standard",
                                 grepl("blank", sample_label, ignore.case = T) ~ "blank",
                                 grepl("K[0-9]", sample_label) ~"sample"),
         well_position = paste0(letter, number)) %>% 
  rename(tray_number = plate) %>% 
  dplyr::select(date, tray_number, well_position, sample_label, sample_type, dilution) %>% 
#  mutate(standard_mM = case_when(sample_type == "standard" ~ parse_number(sample_label))) %>% 
  force()

data_processed = 
  raw_data %>% 
  mutate_all(na_if,"") %>% 
  dplyr::select(-x) %>% 
  fill(x_1) %>% 
  filter(x_2 == "562") %>% 
  dplyr::select(-x_2) %>% 
  pivot_longer(-c(source, x_1), values_to = "absorbance_562") %>% 
  mutate(name = str_remove(name, "x"),
         well_position = paste0(x_1, name),
         date = str_extract(source, "[0-9]{8}"),
         date = ymd(date),
         tray_number = str_extract(source, "tray|plate[1-9]+"),
         tray_number = parse_number(tray_number),
         absorbance_562 = as.numeric(absorbance_562)) %>% 
  dplyr::select(date, tray_number, well_position, absorbance_562) %>% 
  right_join(map_processed, by = c("date", "tray_number", "well_position")) 

#
# 4. Apply calibrations ---------------------------------------------------

calibrate_ferrozine_data = function(data_processed){
  # now do the calibrations
  # standards are in mM
  # molecular formula for FAS = (NH₄)₂Fe(SO₄)₂·6H₂O
  # so 1 M FAS = 1M Fe
  # 1 mM FAS = 1 * 55.85 mg Fe in 1 L solution = 55.85 mg Fe in 1 L solution
  # therefore 1 mM = 55.85 mg/L or 55.85 ppm
  
  standards = 
    data_processed %>% 
    filter(sample_type == "standard") %>% 
    mutate(standard_mM = parse_number(sample_label),
           standard_type = str_extract(sample_label, "FAS|FeCl3"),
           standard_ppm =  case_when(standard_type == "FAS" ~ standard_mM * 55.85)) %>% 
    #dplyr::select(date, tray, absorbance_562, standard_ppm) %>% 
    mutate(standard_ppm = as.numeric(standard_ppm)) %>% 
    dplyr::filter(standard_type == "FAS") %>% 
    # the curve is linear only below 2 mM
    filter(standard_mM <= 2)
  
  gg_calibration = 
    standards %>% 
    filter(standard_mM <= 2) %>% 
    ggplot(aes(x = standard_ppm, y = absorbance_562, color = as.character(tray_number)))+
    geom_point()+
    geom_smooth(method = "lm", se = F)+
    facet_wrap(~date + tray_number)
  
  calibration_coef = 
    standards %>% 
    # 2022-12-22 tray 1 cc is bad, so drop
    filter(!(date == "2022-12-22" & tray_number == 1)) %>% 
    drop_na() %>% 
    dplyr::group_by(date) %>% 
    dplyr::summarize(slope = lm(absorbance_562 ~ standard_ppm)$coefficients["standard_ppm"], 
                     intercept = lm(absorbance_562 ~ standard_ppm)$coefficients["(Intercept)"])
  
  # y = mx + c
  # abs = m*ppm + c
  # ppm = abs-c/m
  
  data_calibrated = 
    data_processed %>% 
    left_join(calibration_coef, by = c("date")) %>% 
    mutate(ppm_calculated = ((absorbance_562 - intercept) / slope))
  
  list(calibration_coef = calibration_coef,
       data_calibrated = data_calibrated,
       gg_calibration = gg_calibration)
}

calibration_curves = calibrate_ferrozine_data(data_processed)$gg_calibration


samples = 
  calibrate_ferrozine_data(data_processed)$data_calibrated %>% 
  filter(sample_type == "sample") %>% 
  dplyr::select(sample_label, ppm_calculated, dilution, date) %>% 
  mutate(dilution = as.numeric(dilution),
         # also apply the reduction efficiency correction (90 %)
         ppm_corrected = ppm_calculated * dilution * 100/90,
         ppm_corrected = round(ppm_corrected, 2)) 


# freeze-dried samples were used, so no need for a moisture correction
samples2 = 
  samples %>% 
  dplyr::select(sample_label, ppm_corrected) %>% 
  separate(sample_label, sep = "_", into = c("kit_id", "transect_location")) %>% 
  mutate(transect_location = case_match(transect_location, "U" ~ "upland", "T" ~ "transition", "W" ~ "wetland")) %>% 
  left_join(weights) %>% 
  mutate(ppm_corrected = as.numeric(ppm_corrected),
         weight_g = as.numeric(weight_g),
         HCl_mL = as.numeric(HCl_mL),
         Fe_ug_g = ppm_corrected * ((HCl_mL)/weight_g),
         Fe_ug_g = round(Fe_ug_g, 2)) %>% 
  dplyr::select(kit_id, transect_location, Fe_ug_g) %>% 
  mutate(transect_location = factor(transect_location, levels = c("upland", "transition", "wetland"))) %>% 
  arrange(kit_id, transect_location)

#
# 5. Export L0B data ------------------------------------------------------
write_csv(samples2, paste0("Data/Processed/EC1_Soil_iron_ferrozine_", Sys.Date(), ".csv"))






## extras ----
# load sample key
sample_key = read.csv("Data/EC1_Metadata_KitLevel.csv")

samples2 %>% 
  left_join(sample_key %>% dplyr::select(kit_id, region)) %>% 
  ggplot(aes(x = transect_location, y = Fe_ug_g, color = region))+
  geom_jitter(width = 0.1)
