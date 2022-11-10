
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
directory <- "https://drive.google.com/drive/folders/1OVhQADClTIcfMtbJenoWfCD8fnODx_it" 


## Define constants
f1_min <- 0
f1_max <- 100

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

data_raw = data_raw %>% mutate(date_run = str_extract(source, "[0-9]{8}"))


#
# 3. Process data --------------------------------------------------------------
#TO DO:
  #extract response values
  ##get range of responses from samples in each run to bound curve
  #take cal curve for each run date
  ##Filter to standards that are reading a weight percentage of C and N 
  #that are in line with what the standard should be reporting
  ##Check out the curve with all good points as is, no averaging by level 
  # calculate standard curves in R 
  ##recalculate all sample values based on responses 

cat("Processing", var, "data...")

## I dont think we need these lines now- AMP ##
# # Process LOD data
# lod %>% 
#   rename(instrument_id = "...1", sample_id = "...3",
#                total_nitrogen_mg = "Weight\n[mg]...8",
#                total_carbon_mg = "Weight\n[mg]...15") %>% 
#   left_join(key, by = c("instrument_id" = "Original Instrument ID")) %>% 
#   separate(sample_id, into = c("type", "weight", "run"), sep = "_") %>% 
#   select(type, weight, run, total_nitrogen_mg, total_carbon_mg) %>% 
#   filter(weight == "0.1", type == "ACN") %>% 
#   mutate(total_nitrogen_mg = as.numeric(total_nitrogen_mg),
#          total_carbon_mg = as.numeric(total_carbon_mg)) %>% 
#   summarise(lod_tn_average = mean(total_nitrogen_mg),
#             lod_tn_sd = sd(total_nitrogen_mg),
#             lod_tc_average = mean(total_carbon_mg),
#             lod_tc_sd = sd(total_carbon_mg)) -> lod_processed

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

## This is where I would first filter to standards and samples dataframes 

cat("Calcuating", var, "data from cal curves...")

#Step 1. filter nitrogen_response and carbon_response and date_ran in standards = Standards Dataframe
#Step 2. filter nitrogen_response and carbon_response and date_ran in samples = Sample Dataframe
#Step 3. filtering samples outside of range between the response values for 0.1mg to 6mg in Standards Dataframe per date_ran
#Step 4. get range of responses from samples in each run (date_ran)
#Step 5. Filter Standards Dataframe that are reading a weight percentage of C and N that are in line with what the standard should be reporting +/- 5% (Acetanilide_C_per = 71.09, Acetanilide_N_per = 10.36)
#Step 6. make a linear plot to visually confirm the curve per date_ran is reasonable with good points as is (at min 3 points per curve, ideally as many as possible), no averaging by curve/weight level needed (if curves look wonky, need to average and/or drop points dates)
#Step 7. Steph talk to Allison & Khadijah
#Step 8. Response ratios of good/bad curves if needed (@amyerspigg)
#Step 9. Re-calculate standard curves in R using package EnvStats calibrate function:
      #curve_fit_N = calibrate(nitrogen_response ~ nitrogen_wt_mg, Standards Dataframe, max.order = 2)
#Step 10. Recalculate all sample values based on responses in R using package EnvStats inversePredictCalibrate:
  #Sample Dataframe$nitrogen_wt_mg = inversePredictCalibrate(curve_fit_N, Sample Dataframe$nitrogen_response)
#Step 11. Using nitrogen_wt_mg , carbon_wt_mg columns from the re-calculation above, then calculate weight percent: 
        # nitrogen_wt_mg /sample_wt_mg x 100 = nitrogen_weight_percent
        # carbon_wt_mg /sample_wt_mg x 100 = carbon_weight_percent
#
# 4. Apply QC flags ------------------------------------------------------------
#TO DO:
#Flag 2:
  #below_detect rename to "outside cal curve"

cat("Applying flags to", var, "data...")

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

data_qc <- function(data) {
  data %>% 
    mutate(  #a = round(a, n_sig_figs),
           tn_flag_1 = ifelse(total_nitrogen_perc < f1_min | total_nitrogen_perc > f1_max, T, F),
           tc_flag_1 = ifelse(total_carbon_perc < f1_min | total_carbon_perc > f1_max, T, F),
           tn_flag_3 = ifelse(total_nitrogen_perc_min < (0.05 * total_nitrogen_perc) |
                              total_nitrogen_perc_max > (0.05 * total_nitrogen_perc), T, F),
           tc_flag_3 = ifelse(total_carbon_perc_min < (0.05 * total_carbon_perc) |
                              total_carbon_perc_max > (0.05 * total_carbon_perc), T, F)
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
  
## The file written out should be named following 
## [Campaign]_[Analyte]_[QC_level]_[Date_of_creation_YYYYMMDD].csv
#drive_upload(media = data_clean, path = data_path)

write_csv(data_clean, paste0("Data/Processed/EC1_Soil_TCTN_L0B_",Sys.Date(),".csv"))
 