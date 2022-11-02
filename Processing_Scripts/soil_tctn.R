
## This is a data processing script for EXCHANGE, a sub-project of THE DOE-funded 
## COMPASS project (https://compass.pnnl.gov/). 
##
## This script imports raw data for Total Carbon and Total Nitrogen measured using
## an Elemental Analyzer at MCRL
## and exports clean, Level 1 QC'ed data.
## Data are read in from the COMPASS Google Drive.
## 
## Created: 2022-04-01
## Stephanie Pennington
##
# ############# #
# ############# #

#
# 1. Setup ---------------------------------------------------------------------
cat("Setup")

# load packages
require(pacman)
pacman::p_load(cowsay,
               ggplot2,
               dplyr,
               readr,
               tidyr,
               googlesheets4, # read_sheet 
               googledrive,
               stringr,
               lubridate) # drive_upload

## Welcome
say("Welcome to EXCHANGE!", by = "random")

## URL for data
folder_path <- "https://drive.google.com/drive/folders/1OVhQADClTIcfMtbJenoWfCD8fnODx_it" 


## Define constants
f1_min <- 0
f1_max <- 100

## Define analyte
var <- "TC/TN"

# Create function to use in lapply that reads in a google sheet 
read_tctn <- function(x) {
  df <- read_delim(file = x, skip = 7, delim = "\t")
}



# 3. Import data ---------------------------------------------------------------

## Create a list of files to download
files <- drive_ls(directory) %>% 
  filter(grepl("_Summary_", name))

## Download files to local (don't worry, we'll delete em in a sec)
lapply(files$name, drive_download, overwrite = TRUE)

# 3. Import data ---------------------------------------------------------------

## Read in data, filter to EC1 samples, and add sample name
npoc_raw <- files$name %>% 
  map(read_data) %>% 
  bind_rows() 

## Clean up local (delete downloaded files)
file.remove(c(files$name))

map_dfr(file_name,readtext)

#
# 2. Import data ---------------------------------------------------------------
cat("Importing", var, "data...")

## read in raw data
all_files <- drive_ls(path = folder_path, pattern = "2022")
gsheet_files <- all_files[endsWith(all_files$name, "2022.txt"),1]

## Download files to local (don't worry, we'll delete em in a sec)
lapply(all_files$name, drive_download, overwrite = TRUE)

lapply(gsheet_files$name, read_tctn) %>% 
  bind_rows() -> data_raw

file.remove(c(files$name))
#
# 3. Process data --------------------------------------------------------------
cat("Processing", var, "data...")

# Process LOD data
lod %>% 
  rename(instrument_id = "...1", sample_id = "...3",
               total_nitrogen_mg = "Weight\n[mg]...8",
               total_carbon_mg = "Weight\n[mg]...15") %>% 
  left_join(key, by = c("instrument_id" = "Original Instrument ID")) %>% 
  separate(sample_id, into = c("type", "weight", "run"), sep = "_") %>% 
  select(type, weight, run, total_nitrogen_mg, total_carbon_mg) %>% 
  filter(weight == "0.1", type == "ACN") %>% 
  mutate(total_nitrogen_mg = as.numeric(total_nitrogen_mg),
         total_carbon_mg = as.numeric(total_carbon_mg)) %>% 
  summarise(lod_tn_average = mean(total_nitrogen_mg),
            lod_tn_sd = sd(total_nitrogen_mg),
            lod_tc_average = mean(total_carbon_mg),
            lod_tc_sd = sd(total_carbon_mg)) -> lod_processed

data_raw %>% 
  rename(instrument_id = "...1", sample_id = "...3",
         total_nitrogen_perc = "Weight\n[%]...9",
         total_nitrogen_mg = "Weight\n[mg]...8",
         total_carbon_perc = "Weight\n[%]...16",
         total_carbon_mg = "Weight\n[mg]...15") %>% 
  select(instrument_id, sample_id, total_nitrogen_perc, total_nitrogen_mg,
         total_carbon_perc, total_carbon_mg) %>% 
  filter(str_detect(instrument_id, "^EC1")) %>% # filter out blanks
  left_join(key, by = c("instrument_id" = "Original Instrument ID")) %>% 
  separate('Reassigned Sample ID', into = c("campaign", "kit_id","transect_location", 
                                            "acidification", "set", "run"), sep = "_") %>% 
  mutate(transect_location = case_when(transect_location == "WET" ~ "Wetland",
                                       transect_location == "TRANS" ~ "Transition",
                                       transect_location == "UPL" ~ "Upland"),
         acidification = case_when(acidification == "UnAc" ~ FALSE)) %>% 
  separate(instrument_id, into = c("one", "two", "three", "Month", "Day"), sep = "_") %>% 
  mutate(Year = "2022", date_ran = make_date(day = Day, month = Month, year = Year)) %>% 
  bind_cols(lod_processed) -> data_intermediate

data_intermediate %>% 
  group_by(kit_id, transect_location, set) %>% 
  summarise(total_nitrogen_perc = mean(as.numeric(total_nitrogen_perc)),
         total_carbon_perc = mean(as.numeric(total_carbon_perc)),
         total_nitrogen_mg = mean(as.numeric(total_nitrogen_mg)),
         total_carbon_mg = mean(as.numeric(total_carbon_mg)),
         total_nitrogen_perc_min = min(total_nitrogen_perc),
         total_carbon_perc_min = min(total_carbon_perc),
         total_nitrogen_perc_max = max(total_nitrogen_perc),
         total_carbon_perc_max = max(total_carbon_perc)) -> means_mins

data_intermediate %>% 
  distinct(kit_id, transect_location, .keep_all = TRUE) %>% 
  select(campaign, kit_id, transect_location, set, acidification, lod_tc_average, 
         lod_tc_sd, lod_tn_average, lod_tn_sd, date_ran) %>% 
  right_join(means_mins, by = c("kit_id", "transect_location", "set")) -> data_processed

#
# 4. Apply QC flags ------------------------------------------------------------
cat("Applying flags to", var, "data...")

data_qc <- function(data) {
  data %>% 
    mutate(  #a = round(a, n_sig_figs),
           tn_flag_1 = ifelse(total_nitrogen_perc < f1_min | total_nitrogen_perc > f1_max, T, F),
           tc_flag_1 = ifelse(total_carbon_perc < f1_min | total_carbon_perc > f1_max, T, F),
           tn_flag_2 = ifelse(total_nitrogen_mg < (lod_tn_sd * 3), T, F),
           tc_flag_2 = ifelse(total_carbon_mg < (lod_tc_sd * 3), T, F),
           tn_flag_3 = ifelse(total_nitrogen_perc_min < (0.5 * total_nitrogen_perc) |
                              total_nitrogen_perc_max > (0.5 * total_nitrogen_perc), T, F),
           tc_flag_3 = ifelse(total_carbon_perc_min < (0.5 * total_carbon_perc) |
                              total_carbon_perc_max > (0.5 * total_carbon_perc), T, F)
           )
}

data_qc <- data_qc(data_processed)

data_qc %>% 
  pivot_longer(cols = starts_with("tn"), names_to = "tn_flag",
               values_to = "tn_vals") %>% 
  filter(tn_vals == TRUE) %>% select(-tn_vals) %>%  
  group_by(kit_id, transect_location) %>%
  mutate(tn_flag = case_when(tn_flag == "tn_flag_1" ~ "outside range",
                             tn_flag == "tn_flag_2" ~ "below detect",
                             tn_flag == "tn_flag_3" ~ "rep outlier")) %>% 
  summarise(tn_flag = toString(tn_flag)) -> tn_flags
  

data_qc %>% 
  pivot_longer(cols = starts_with("tc"), names_to = "tc_flag",
               values_to = "tc_vals") %>% 
  filter(tc_vals == TRUE) %>% select(-tc_vals) %>% 
  group_by(kit_id, transect_location) %>%
    mutate(tc_flag = case_when(tc_flag == "tc_flag_1" ~ "outside range",
                               tc_flag == "tc_flag_2" ~ "below detect",
                               tc_flag == "tc_flag_3" ~ "rep outlier")) %>% 
    summarise(tc_flag = toString(tc_flag)) %>% 
    left_join(tn_flags, by = c("kit_id", "transect_location")) -> flags
  
  
  data_qc %>% 
    left_join(flags, by = c("kit_id", "transect_location")) %>% 
    select(campaign, kit_id, transect_location, acidification, total_nitrogen_perc, 
         total_carbon_perc, tn_flag, tc_flag) %>% 
    mutate(total_carbon_perc = round(total_carbon_perc, 2),
           total_nitrogen_perc = round(total_nitrogen_perc, 2)) %>% 
    rename(tc_perc = total_carbon_perc,
           tn_perc = total_nitrogen_perc)-> data_clean

#
# 5. Write cleaned data to drive -----------------------------------------------

## We should add Sys.date or hardcode date so we know when the L0B was born
## The file written out should be named following 
## [Campaign]_[Analyte]_[QC_level]_[Date_of_creation_YYYYMMDD].csv
#drive_upload(media = data_clean, path = data_path)

write_csv(data_clean, "Data/Processed/EC1_Soil_TCTN_L0B_20220602.csv")

