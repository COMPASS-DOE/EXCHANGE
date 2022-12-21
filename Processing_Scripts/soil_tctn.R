
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

data_raw = data_raw %>% mutate(
  date_run = str_extract(source, "[0-9]{1}_[0-9]{2}_[0-9]{4}|[0-9]{2}_[0-9]{2}_[0-9]{4}"),
  date_run = mdy(str_replace_all(date_run, fixed("_"), "-")))


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

data_raw %>% 
  filter(grepl("STD", sample)) %>% 
  mutate(nitrogen_weight_percent = as.numeric(nitrogen_weight_percent),
         carbon_weight_percent = as.numeric(carbon_weight_percent),
         nitrogen_response = as.numeric(nitrogen_response),
         carbon_response = as.numeric(carbon_response),
         nitrogen_wt_mg = as.numeric(nitrogen_wt_mg),
         carbon_wt_mg = as.numeric(carbon_wt_mg)) -> standards_df

#Step 1b. filter nitrogen_response and carbon_response and date_ran in checks = Checks Dataframe

data_raw %>% 
  filter(grepl("STD", sample)) %>% 
  mutate(nitrogen_weight_percent = as.numeric(nitrogen_weight_percent),
         carbon_weight_percent = as.numeric(carbon_weight_percent),
         nitrogen_response = as.numeric(nitrogen_response),
         carbon_response = as.numeric(carbon_response),
         nitrogen_wt_mg = as.numeric(nitrogen_wt_mg),
         carbon_wt_mg = as.numeric(carbon_wt_mg)) -> checks_df

#Step 2. filter nitrogen_response and carbon_response and date_ran in samples = Sample Dataframe

data_raw  %>% 
  filter(grepl("EC1", sample)) %>% 
  mutate(nitrogen_weight_percent = as.numeric(nitrogen_weight_percent),
         carbon_weight_percent = as.numeric(carbon_weight_percent),
         nitrogen_response = as.numeric(nitrogen_response),
         carbon_response = as.numeric(carbon_response)) -> samples_df

#Step 3. filtering samples outside of range between the response values for 0.1mg to 6mg in Standards Dataframe per date_ran

standards_df %>% 
  rename(n_response_standards = nitrogen_response, 
         c_response_standards = carbon_response) %>% 
  group_by(date_run) %>% 
  summarise(min_n = min(n_response_standards),
            max_n = max(n_response_standards),
            min_c = min(c_response_standards),
            max_c = max(c_response_standards)) -> standards_range

samples_df %>% 
  left_join(standards_range, by = "date_run") %>% 
  filter(nitrogen_response < max_n, nitrogen_response > min_n,
         carbon_response < max_c, carbon_response > min_c) %>% 
  mutate(month = as.character(month(date_run, label = TRUE, abbr = FALSE))) -> samples_filtered

checks_df %>% 
  left_join(standards_range, by = "date_run") %>% 
  filter(nitrogen_response < max_n, nitrogen_response > min_n,
         carbon_response < max_c, carbon_response > min_c) %>% 
  mutate(month = as.character(month(date_run, label = TRUE, abbr = FALSE))) -> checks_filtered

nrow(data_raw) - nrow(samples_filtered) -> row_change
cat(row_change, "rows were dropped")

#Step 4. get range of responses from samples in each run (date_ran)

summary(samples_filtered)

#Step 5. Filter Standards Dataframe that are reading a weight percentage of C and N that are in line 
# with what the standard should be reporting +/- 5% (Acetanilide_C_per = 71.09, Acetanilide_N_per = 10.36)

# standard_c = 71.09, standard_c_prop = 3.5545
# standard_n = 10.36, standard_n_prop = 0.518
standards_df %>% 
  filter(carbon_weight_percent < standard_c + standard_c_prop,
         carbon_weight_percent > standard_c - standard_c_prop,
         nitrogen_weight_percent > standard_n - standard_n_prop,
         nitrogen_weight_percent < standard_n + standard_n_prop) %>% 
  mutate(month = as.character(month(date_run, label = TRUE, abbr = FALSE)),
       month_groups = ifelse(month == "September", "August", month)) %>% 
  group_by(month) -> t

nrow(standards_df) - nrow(t) -> row_change_2
cat(row_change_2, "rows were dropped")

#Step 6. make a linear plot to visually confirm the curve per date_ran is reasonable with good points as is 
# (at min 3 points per curve, ideally as many as possible), no averaging by curve/weight level needed (if curves look wonky, 
# need to average and/or drop points dates) x = weight, y = response
t %>% 
  summarise(n = n()) -> p

mon <- unique(t$month)

r2 <- function(mon) {
  
  t <- filter(t, month == mon)
  m <- lm(nitrogen_response ~ nitrogen_wt_mg, data = t)
  data.frame(month = mon, type = "N", r2 = summary(m)$r.squared)
}

lapply(mon, r2) -> y

t %>% 
  ggplot(aes(x = nitrogen_wt_mg, y = nitrogen_response)) + 
  geom_point(aes(color = as.factor(date_run))) + 
  geom_smooth(aes(group = month), method = "lm") +
  geom_text(data = p, aes(label = paste("n =", n)), x = 0.1, y = 1000) +
  facet_wrap(~month) +
  theme_linedraw() +
  labs(title = "Nitrogen Standards", x = "Nitrogen Weight (mg)", y = "Nitrogen Response")

t %>% 
  ggplot(aes(x = carbon_wt_mg, y = carbon_response)) + 
  geom_point(aes(color = as.factor(date_run))) + 
  geom_text(data = p, aes(label = paste("n =", n)), x = 0.5, y = 12000) +
  facet_wrap(~month) +
  theme_linedraw() +
  labs(title = "Carbon Standards", x = "Carbon Weight (mg)", y = "Carbon Response")

t_groups <- unique(t$month)

curve_fit_N <- function(x) {
  
  standards <- filter(t, month == x)
  samples <- filter(samples_filtered, month == x)

#samples
  calibrate(nitrogen_response ~ nitrogen_wt_mg, standards, max.order = 2) -> c
  as.data.frame(inversePredictCalibrate(c, samples$nitrogen_response)) %>% 
    mutate(month = x) %>% 
    rename(nitrogen_response = obs.y, predict_N_wt = pred.x)
  
}

curve_fit_N_ck <- function(x) {
  
  standards <- filter(t, month == x)
  checks <- filter(checks_filtered, month == x)
  
  #checks
  calibrate(nitrogen_response ~ nitrogen_wt_mg, standards, max.order = 2) -> w
  as.data.frame(inversePredictCalibrate(w, checks$nitrogen_response)) %>% 
    mutate(month = x) %>% 
    rename(nitrogen_response = obs.y, predict_N_wt = pred.x)
  
}

curve_fit_C <- function(x) {
  
  standards <- filter(t, month == x)
  samples <- filter(samples_filtered, month == x)
  
#samples
    calibrate(carbon_response ~ carbon_wt_mg, standards, max.order = 2) -> c
  as.data.frame(inversePredictCalibrate(c, samples$carbon_response)) %>% 
    mutate(month = x) %>% 
    rename(carbon_response = obs.y, predict_C_wt = pred.x)
  
}

curve_fit_C_ck <- function(x) {
  
  standards <- filter(t, month == x)
  checks <- filter(checks_filtered, month == x)

  #checks
  calibrate(carbon_response ~ carbon_wt_mg, checks, max.order = 2) -> w
  as.data.frame(inversePredictCalibrate(w, checks$carbon_response)) %>% 
    mutate(month = x) %>% 
    rename(carbon_response = obs.y, predict_C_wt = pred.x)
  
}

lapply(t_groups, curve_fit_N) %>% bind_rows() -> N_vals
lapply(t_groups, curve_fit_C) %>% bind_rows() -> C_vals

lapply(t_groups, curve_fit_N_ck) %>% bind_rows() -> N_chks
lapply(t_groups, curve_fit_C_ck) %>% bind_rows() -> C_chks

samples_filtered %>% 
  left_join(N_vals, by = c("nitrogen_response", "month")) %>% 
  left_join(C_vals, by = c("carbon_response", "month")) -> samples_joined

checks_filtered %>% 
  left_join(N_chks, by = c("nitrogen_response", "month")) %>% 
  left_join(C_chks, by = c("carbon_response", "month")) -> checks_joined

#Step 11. Using nitrogen_wt_mg , carbon_wt_mg columns from the re-calculation above, then calculate weight percent: 
        # nitrogen_wt_mg /sample_wt_mg x 100 = nitrogen_weight_percent
        # carbon_wt_mg /sample_wt_mg x 100 = carbon_weight_percent

samples_joined %>% 
  ungroup() %>% 
  mutate(new_nitrogen_weight_percent = predict_N_wt / as.numeric(sample_wt_mg) * 100,
         new_carbon_weight_percent = predict_C_wt  /as.numeric(sample_wt_mg) * 100) -> full_samples

checks_joined %>% 
  ungroup() %>% 
  mutate(new_nitrogen_weight_percent = predict_N_wt / as.numeric(sample_wt_mg) * 100,
         new_carbon_weight_percent = predict_C_wt  /as.numeric(sample_wt_mg) * 100) -> full_checks

ggplot(full_samples, aes(x = month, y = new_nitrogen_weight_percent)) + geom_point() + theme_minimal()
ggplot(full_samples, aes(x = month, y = new_carbon_weight_percent)) + geom_point() + theme_minimal()

ggplot(full_checks, aes(x = month, y = new_nitrogen_weight_percent)) + geom_point() + theme_minimal()
ggplot(full_checks, aes(x = month, y = new_carbon_weight_percent)) + geom_point() + theme_minimal()


full_samples %>% 
  separate(sample_id, sep = "_", into = c("campaign", "kit_id", "transect_location", "misc")) %>% 
  mutate(transect_location = case_when(transect_location == "WET" ~ "Wetland",
                                       transect_location == "TRANS" ~ "Transition",
                                       transect_location == "UPL" ~ "Upland")) %>% 
  select(campaign, kit_id, transect_location, new_nitrogen_weight_percent, new_carbon_weight_percent) %>%
  rename(total_nitrogen_perc = new_nitrogen_weight_percent, total_carbon_perc = new_carbon_weight_percent) -> df

# Check with Check Standards:
checks_df %>% 
  filter(carbon_weight_percent < standard_c + standard_c_prop,
         carbon_weight_percent > standard_c - standard_c_prop,
         nitrogen_weight_percent > standard_n - standard_n_prop,
         nitrogen_weight_percent < standard_n + standard_n_prop) %>% 
  mutate(month = as.character(month(date_run, label = TRUE, abbr = FALSE)),
         month_groups = ifelse(month == "September", "August", month)) %>% 
  group_by(month) -> q


# 4. Apply QC flags ------------------------------------------------------------
#TO DO:
#Flag 2:
  #below_detect rename to "outside cal curve"

cat("Applying flags to", var, "data...")

df %>% 
   group_by(kit_id, transect_location) %>% 
   summarise(total_nitrogen_perc = mean(as.numeric(total_nitrogen_perc)),
             total_carbon_perc = mean(as.numeric(total_carbon_perc)),
            # total_nitrogen_mg = mean(as.numeric(total_nitrogen_mg)),
            # total_carbon_mg = mean(as.numeric(total_carbon_mg)),
             total_nitrogen_perc_min = min(total_nitrogen_perc),
             total_carbon_perc_min = min(total_carbon_perc),
             total_nitrogen_perc_max = max(total_nitrogen_perc),
             total_carbon_perc_max = max(total_carbon_perc)) -> means_mins
 
 df %>% 
   distinct(kit_id, transect_location, .keep_all = TRUE) %>% 
#   select(campaign, kit_id, transect_location, set, acidification, lod_tc_average, 
#          lod_tc_sd, lod_tn_average, lod_tn_sd, date_ran) %>% 
   right_join(means_mins, by = c("kit_id", "transect_location")) -> data_processed

### @Steph: We need to drop the outlier here ###
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

data_qc <- data_qc(means_mins)

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
 