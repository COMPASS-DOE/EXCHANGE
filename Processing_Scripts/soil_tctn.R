
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
               lubridate,
               EnvStats) # drive_upload

## Welcome
say("Welcome to EXCHANGE!", by = "random")

## URL for data
directory <- "https://drive.google.com/drive/folders/1OVhQADClTIcfMtbJenoWfCD8fnODx_it" 

## Define constants
f1_min <- 0
f1_max <- 100
standard_c <- 71.09
standard_c_prop <- standard_c * 0.05
standard_n <- 10.36
standard_n_prop <- standard_n * 0.05

## Define analyte
var <- "TC/TN"

#
# 3. Import data ---------------------------------------------------------------

import_data = function(directory){
  
  ## a. Create a list of files to download
  files <- 
    drive_ls(directory) %>% 
    filter(grepl(c("EC1_"), name))
  
  ## b. Download files to local (don't worry, we'll delete em in a sec)
  lapply(files$id, drive_download, overwrite = TRUE)
  
  ## c. pull a list of file names, then read all files and combine
  
  filePaths <- files$name
  
  ## this is tricky because the headers are at variable locations across the files
  ## e.g. some files have header on line 5, some on line 20, etc.
  ## To get around this mess, set the column names first, so all the files have the same column names.
  ## Then remove the unnecessary rows
  ## PS: this only works because the column positions are consistent across all files!
  ## Dummy columns "aa", "bb", "zz" were set because those will be removed later anyway. 
  dat <- 
    do.call(bind_rows, lapply(filePaths, function(path){
      df <- read.delim(path, 
                       sep = "\t", 
                       header = FALSE,
                       col.names = c("aa", "bb", "sample", "sample_id", "sample_wt_mg", 
                                     "nitrogen_retention_min", "nitrogen_response", "nitrogen_wt_mg", "nitrogen_weight_percent", "nitrogen_peak_type", "nitrogen_element_name", "nitrogen_carbon_response_ratio",
                                     "carbon_retention_min", "carbon_response", "carbon_wt_mg", "carbon_weight_percent", "carbon_peak_type", "carbon_element_name", "carbon_carbon_response_ratio", "zz"),
                       na = "")
      #  df <- read.delim(path, skip = 2)
      df[["source"]] <- rep(path, nrow(df))
      df}))
  
  dat <- 
    dat %>% 
    filter(!is.na(aa)) %>% 
    filter(!is.na(carbon_element_name))
  
  ## d. delete the temporary files
  file.remove(c(files$name))  
  
  ## e. output
  dat
}

data_raw = import_data(directory)

data_raw[data_raw == "N/A"] <- NA # replace "N/A" with NA

data_raw %>% 
  mutate(
    date_run = str_extract(source, "[0-9]{1}_[0-9]{2}_[0-9]{4}|[0-9]{2}_[0-9]{2}_[0-9]{4}"),
    date_run = mdy(str_replace_all(date_run, fixed("_"), "-")),
    month = as.character(month(date_run, label = TRUE, abbr = FALSE)),
    month_groups = ifelse(month == "September", "August", month),
    nitrogen_weight_percent = as.numeric(nitrogen_weight_percent),
    carbon_weight_percent = as.numeric(carbon_weight_percent),
    nitrogen_response = as.numeric(nitrogen_response),
    carbon_response = as.numeric(carbon_response),
    nitrogen_wt_mg = as.numeric(nitrogen_wt_mg),
    carbon_wt_mg = as.numeric(carbon_wt_mg),
    sample_wt_mg = as.numeric(sample_wt_mg)) %>% 
  separate(source, into = c("one", "two", "three", "four", "rep")) %>% 
  select(-one, -two, -three, -four) -> data_primitive

#
# 3. Process data --------------------------------------------------------------
  #extract response values
  ##get range of responses from samples in each run to bound curve
  #take cal curve for each run date
  ##Filter to standards that are reading a weight percentage of C and N 
  #that are in line with what the standard should be reporting
  ##Check out the curve with all good points as is, no averaging by level 
  # calculate standard curves in R 
  ##recalculate all sample values based on responses 

cat("Calcuating", var, "data from cal curves...")

#Step 1. filter nitrogen_response and carbon_response and date_ran in standards = Standards Dataframe
# standards start with STD in sample column

data_primitive %>% filter(grepl("STD", sample)) -> standards_df

#Step 1b. filter nitrogen_response and carbon_response and date_ran in checks = Checks Dataframe

data_primitive %>% filter(grepl("CK", sample)) %>% 
  mutate(type = str_extract(sample_id, "CK_[A-Z]*"),
         type_perc_C = case_when(type == "CK_AN" ~ 71.09,
                                 type == "CK_AP" ~ 70.56,
                                 type == "CK_C" ~ 4.4),
         type_perc_N = case_when(type == "CK_AN" ~ 10.36,
                                 type == "CK_AP" ~ 4.84,
                                 type == "CK_N" ~ 6.06)) -> checks_df

#Step 2. filter nitrogen_response and carbon_response and date_ran in samples = Sample Dataframe

data_primitive %>% filter(grepl("EC1", sample)) -> samples_df

#Step 3. Reverse-calculate sample weights using Acetanilide fraction, these will be used to predict sample weights

standards_df %>% mutate(
  reverse_C_wt = sample_wt_mg * 0.7109,
  reverse_N_wt = sample_wt_mg * 0.1036
) -> reverse_standards


#Step 4. Calculate sample weights based on the reverse standards using the 'EnvStats' package

curve_fit_N <- function(x) {
  
  standards <- filter(reverse_standards, date_run == x)
  data <- filter(samples_df, date_run == x)
  
  calibrate(nitrogen_response ~ reverse_N_wt, standards, max.order = 3) -> w
  as.data.frame(inversePredictCalibrate(w, data$nitrogen_response)) %>% 
    mutate(date_run = x) %>% 
    rename(nitrogen_response = obs.y, predict_N_wt = pred.x)
  
}

curve_fit_C <- function(x) {
  
  standards <- filter(reverse_standards, date_run == x)
  data <- filter(samples_df, date_run == x)
  
  calibrate(carbon_response ~ reverse_C_wt, standards, max.order = 3) -> w
  as.data.frame(inversePredictCalibrate(w, data$carbon_response)) %>% 
    mutate(date_run = x) %>% 
    rename(carbon_response = obs.y, predict_C_wt = pred.x)
  
}

d_groups <- unique(reverse_standards$date_run) #grab dates run

lapply(d_groups, curve_fit_C) %>% bind_rows() -> C_reverse
lapply(d_groups, curve_fit_N) %>% bind_rows() -> N_reverse

samples_df %>% 
  left_join(N_reverse, by = c("nitrogen_response", "date_run")) %>% 
  left_join(C_reverse, by = c("carbon_response", "date_run")) -> reverse_joined

reverse_joined %>% 
  separate(sample_id, into = c("campaign", "kit_id", "transect_location", "extra_rep")) %>% 
  mutate(extra_rep = case_when(extra_rep == "REP4" ~ "R4",
                               TRUE ~ extra_rep),
         extra_rep = toupper(extra_rep),
         rep = case_when(!is.na(extra_rep) ~ extra_rep,
                         TRUE ~ rep)) %>%
  select(campaign, kit_id, transect_location, date_run, rep, sample_wt_mg, predict_N_wt, nitrogen_response, predict_C_wt, carbon_response) %>% 
  # round to 3 dec places (SP)
  mutate(carbon_weight_perc = round((predict_C_wt / sample_wt_mg) * 100, 3),
         nitrogen_weight_perc = round((predict_N_wt / sample_wt_mg) * 100, 3),
         carbon_weight_mg = round(predict_C_wt, 3),
         nitrogen_weight_mg = round(predict_N_wt, 3),
         transect_location = case_when(transect_location == "UPL" ~ "upland",
                                       transect_location == "TRANS" ~ "transition",
                                       transect_location == "WET" ~ "wetland")) -> tctn_df

## ----- NEED TO RESOLVE / CLEAN BELOW CODE -----

# 4. Apply QC flags ------------------------------------------------------------
#TO DO:
#Flag 2:
  #below_detect rename to "outside cal curve"

cat("Applying flags to", var, "data...")

# Calculate minimum response standards for each date_run (flag 2)
reverse_standards %>% 
  group_by(date_run) %>% 
  summarise(min_c = min(reverse_C_wt),
            min_n = min(reverse_N_wt)) -> mins

# Calculate median predicted percentages by kit_id, transect_location (flag 3)
tctn_df %>% 
  group_by(kit_id, transect_location) %>% 
  summarise(median_c = median(carbon_weight_perc),
            median_n = median(nitrogen_weight_perc)) %>% 
  mutate(range_c = median_c * 0.10,
         range_n = median_n * 0.10) -> medians

### @Steph: We need to drop the outlier here ###
data_qc <- function(df){

  df %>% 
    left_join(mins, by = "date_run") %>% 
    left_join(medians, by = c("kit_id", "transect_location")) %>% 
    mutate(
           tn_flag_1 = ifelse(nitrogen_weight_perc < f1_min | nitrogen_weight_perc > f1_max, T, F),
           tc_flag_1 = ifelse(carbon_weight_perc < f1_min | carbon_weight_perc > f1_max, T, F)) %>% 
    group_by(date_run) %>% 
    mutate(
           tn_flag_2 = ifelse(nitrogen_weight_mg < min_n, T, F),
           tc_flag_2 = ifelse(carbon_weight_mg < min_c, T, F)) %>% 
    ungroup() %>% 
    group_by(kit_id, transect_location) %>% 
    mutate(
           tn_flag_3 = ifelse(nitrogen_weight_perc < (median_n - range_n) | nitrogen_weight_perc > (median_n + range_n), T, F),
           tc_flag_3 = ifelse(carbon_weight_perc < (median_c - range_c) | carbon_weight_perc > (median_c + range_c), T, F)
           # TC_FLAG_2 = VALUE < MIN(STANDARD RESPONSE FOR C/N FROM DATE_RUN), TRUE
           # tn_flag_3 = ifelse(VALUE < (MEDIAN * 0.10)
           #                    VALUE > (MEDIAN * 0.10), FLAG TRUE
           ) %>% 
    ungroup()
}

data_qc <- data_qc(tctn_df)

data_qc %>% 
  pivot_longer(cols = starts_with("tn"), names_to = "tn_flag",
               values_to = "tn_vals") %>% 
  filter(tn_vals == TRUE) %>% select(-tn_vals) %>%  
  group_by(kit_id, transect_location, rep) %>%
  mutate(tn_flag = case_when(tn_flag == "tn_flag_1" ~ "outside range",
                             tn_flag == "tn_flag_2" ~ "below detect",
                             tn_flag == "tn_flag_3" ~ "rep outlier")) %>% 
  summarise(tn_flag = toString(tn_flag)) -> tn_flags
  

data_qc %>% 
  pivot_longer(cols = starts_with("tc"), names_to = "tc_flag",
               values_to = "tc_vals") %>% 
  filter(tc_vals == TRUE) %>% select(-tc_vals) %>% 
  group_by(kit_id, transect_location, rep) %>%
    mutate(tc_flag = case_when(tc_flag == "tc_flag_1" ~ "outside range",
                               tc_flag == "tc_flag_2" ~ "below detect",
                               tc_flag == "tc_flag_3" ~ "rep outlier")) %>% 
    summarise(tc_flag = toString(tc_flag)) %>% 
    left_join(tn_flags, by = c("kit_id", "transect_location", "rep")) -> flags
  
  
data_qc %>% 
    left_join(flags, by = c("kit_id", "transect_location", "rep")) %>% 
    select(campaign, kit_id, transect_location, rep, nitrogen_weight_perc, 
           carbon_weight_perc, tn_flag, tc_flag) %>% 
    rename(tc_perc = carbon_weight_perc,
           tn_perc = nitrogen_weight_perc)-> data_clean

#
# 5. Write cleaned data to drive -----------------------------------------------
  
## The file written out should be named following 
## [Campaign]_[Analyte]_[QC_level]_[Date_of_creation_YYYYMMDD].csv
#drive_upload(media = data_clean, path = data_path)

#rite_csv(data_clean, paste0("Data/Processed/EC1_Soil_TCTN_L0B_",Sys.Date(),".csv"))
 