## This script imports raw data for NPOC and TDN measured using a Shimadzu TOC-L
## at PNNL MCRL and exports clean, Level 0B QC'ed data. 
## Data are read in from Google Drive
## 
## Created: 2022-01-15 (Updated 2022-03-30 with Opal Otenburg)
## Peter Regier
## Updated: 2023-02-23 by Julia McElhinny
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

## Set GDrive URL for NPOC raw data files
directory = "https://drive.google.com/drive/folders/1Mkg5UCEzt9ifKCGyr-Kn9a3LL8XuGzBa"


# 2. Create Functions ----------------------------------------------------------

## Create a function to read in data
read_data <- function(data){
  # First, scrape date from filename
  date <- substr(data, 1, 8)
  # Second, read in data
  read_delim(file = data, skip = 10, delim = "\t") %>% 
    rename(sample_name = `Sample Name`, 
           npoc_raw = `Result(NPOC)`, 
           tdn_raw = `Result(TN)`) %>% 
    select(sample_name, npoc_raw, tdn_raw) %>% 
    mutate(date = date)
}


## create a function to read in the readmes to double check any actions
read_mes <- function(readme){
  # First, scrape date from filename
  date <- str_extract(readme, "[0-9]{8}")
  # Second, read in Read Me
  readxl::read_excel(path = readme, sheet = 1) %>% 
    rename(sample_name = `Sample Name`,
           vial = `Vial`,
           action = `Action`,
           notes = `Notes`) %>% 
    mutate(date = date)
}


# 3. Download data -------------------------------------------------------------

## Create a list of raw NPOC/TDN data files to download
files <- drive_ls(directory) %>% 
  filter(grepl("_Summary_", name))

## Download these files to local (don't worry, we'll delete em in a sec)
lapply(files$name, drive_download, overwrite = TRUE)

## create a list of the raw readmes to download
readmes <- drive_ls(directory) %>%
  filter(grepl("Readme", name))

## download the readmes
lapply(readmes$name, drive_download, overwrite = TRUE)

## Get full file name of the TOC-L LOD information
LOD_file <- drive_ls(directory) %>%
  filter(grepl("MCRL_LOD", name))

## Download LOD file
drive_download(LOD_file$name, overwrite = TRUE)


# 4. Read in data --------------------------------------------------------------

## Read in NPOC/TDN data from downloaded files
npoc_raw <- files$name %>% 
  map(read_data) %>% 
  bind_rows() 

## read in the readmes
readmes_all <- readmes$name %>%
  map(read_mes) %>%
  bind_rows()

## Read in LOD data file
LOD <- readxl::read_excel(LOD_file$name, col_names = TRUE)

## Clean up local (delete downloaded files)
file.remove(c(files$name, LOD_file$name, readmes$name))


# 5. Set up LOD and Join with Raw Data -----------------------------------------

## edit column names of LOD data frame for easier reference
colnames(LOD) <- c("Date_LOD_run", "LOD_Start_Date", "LOD_End_Date", "LOD_NPOC", "LOD_TN", 
                   "LOD_DIC", "LOD_TSS", "Instrument_Notes")

## make date columns numeric for math later
LOD$Date_LOD_run <- as.numeric(LOD$Date_LOD_run)
npoc_raw$date <- as.numeric(npoc_raw$date)

## create a blank list to populate
raw_data_LOD_list <- list()

## for loop to determine which LOD the instrument runs fall within
## this is done by calculating which LOD period start date is closest to the date of the instrument run
for (a in 1:length(unique(npoc_raw$date))) {
  
  ## pull focus date
  focus_date <- unique(npoc_raw$date)[a]
  
  ## filter LOD dataset to just the dates that are less than the desired date
  ## also calculate difference between each of the LOD start dates and the desired date
  target_LOD <- LOD %>%
    filter(Date_LOD_run < focus_date) %>%
    mutate(difference = Date_LOD_run - focus_date) %>%
    filter(difference == max(difference))
  
  ## pull the LOD run start date that is closest to the instrument run date
  target_LOD_date <- target_LOD$Date_LOD_run[1]
  
  ## filter the blanks dataset to all the ones with the focus date
  ## and then add the LOD date as a new column for later merging
  npoc_raw_LOD <- npoc_raw %>%
    filter(date == focus_date) %>%
    mutate(LOD_date = target_LOD_date)
  
  ## populate the empty list
  raw_data_LOD_list[[a]] <- npoc_raw_LOD
  
}

## bind all rows together
raw_data_LOD <- bind_rows(raw_data_LOD_list)

## merge TN and NPOC LOD values to edit blanks that fall below these cutoffs
raw_data_LOD <- left_join(raw_data_LOD, select(LOD, Date_LOD_run, LOD_NPOC, LOD_TN),
                                by = c("LOD_date" = "Date_LOD_run"))


# 6. Data Frame Edits/Set-Up ---------------------------------------------------

## Filter raw data to just blanks
blanks <- raw_data_LOD %>% 
  filter(grepl("^Blank", sample_name))

## Filter raw data to just EXCHANGE samples based on "EC1_K" name pattern
samples <- raw_data_LOD %>% 
  filter(grepl("EC1_K", sample_name))

## Pull information about the calibration curve in another data frame
cal_curve <- raw_data_LOD %>%
  filter(grepl("STD_", sample_name)) %>%
  group_by(date) %>%
  # pull the value of the top point of the calibration curve for TDN and for NPOC by instrument run date
  summarize(npoc_calib_upper_limit = sum(npoc_raw, na.rm = TRUE),
            tdn_calib_upper_limit = sum(tdn_raw, na.rm = TRUE)) %>%
  # replace 0 values with NA to indicate that cal curve was not run for a specific date
  mutate(npoc_calib_upper_limit = ifelse(npoc_calib_upper_limit == 0,
                                     NA,
                                     npoc_calib_upper_limit),
         tdn_calib_upper_limit = ifelse(tdn_calib_upper_limit == 0,
                                    NA,
                                    tdn_calib_upper_limit)) 

## edit readmes to notes about sample re-runs/replacement
readmes_filtered <- readmes_all %>%
  # pull rows that have action items
  filter(!is.na(action)) %>%
  # filter to just EXCHANGE samples
  filter(grepl("EC1_K", sample_name)) %>%
  select(sample_name, action, date) %>%
  mutate(date = as.numeric(date))


# 7. Calculate Blanks per Instrument Run (after LOD correction) ----------------

## if blanks are below the LOD for a given instrument run, set those to the LOD
blanks_LOD_corrected <- blanks %>%
  mutate(npoc_raw = ifelse(npoc_raw < LOD_NPOC,
                           LOD_NPOC,
                           npoc_raw),
         tdn_raw = ifelse(tdn_raw < LOD_TN,
                          LOD_TN,
                          tdn_raw)) %>%
  select(sample_name, npoc_raw, tdn_raw, date)
  
## calculate blanks to use for blank correction by instrument run
blanks_final <- blanks_LOD_corrected %>% 
  group_by(date) %>% 
  summarize(npoc_blank = round(mean(npoc_raw, na.rm = TRUE), 4), 
            tdn_blank = round(mean(tdn_raw, na.rm = TRUE), 4),
            npoc_blank_SD = sd(npoc_raw, na.rm = TRUE),
            tdn_blank_SD = sd(tdn_raw, na.rm = TRUE))


# 8. Blank Correct Raw Data ----------------------------------------------------

samples_blank_corrected <- samples %>% 
  mutate(campaign = "EC1", 
         kit_id = substr(sample_name, 5, 9), 
         transect_location = "water") %>% 
  # join blank information with dataset
  inner_join(select(blanks_final, date, npoc_blank, tdn_blank), by = "date") %>% 
  # calculate blank corrected npoc/tdn values
  mutate(npoc_mgl = npoc_raw - npoc_blank, 
         tdn_mgl = tdn_raw - tdn_blank) %>%
  # simplify data frame
  select(campaign, kit_id, transect_location, sample_name, date, npoc_mgl, tdn_mgl, LOD_NPOC, LOD_TN)


# 8b. Check Data Against ReadMe Action Items -----------------------------------

## the readme files contain "Action" column - some samples needed TDN values
## replaced from a subsequent instrument run for just TN
samples_readme_action <- samples_blank_corrected %>%
  left_join(readmes_filtered, by = c("sample_name", "date")) %>%
  # if the action reads "Replace TN", make tdn_mgl NA
  mutate(tdn_mgl = ifelse(grepl("Replace TN", action),
                          NA,
                          tdn_mgl)) %>%
  select(-action)


# 8c. Flag Data Against LOD and Cal Curve Upper Limits -------------------------

## join cal curve data frame and compare values to LOD and upper limits to add flags
npoc_bc_flagged <- samples_readme_action %>% 
  ## First, round each parameter to proper significant figures
  mutate(npoc_mgl = round(npoc_mgl, 2), 
         tdn_mgl = round(tdn_mgl, 3)) %>% 
  ## join with the cal curve information
  left_join(cal_curve, by = "date") %>%
  ## Second, add flags for outside LOD (LOD info is still part of this data frame from previous edits)
  mutate(npoc_flag = ifelse(npoc_mgl > npoc_calib_upper_limit, "npoc outside calibration curve range", NA), #per cal curve upper limit
       tdn_flag = ifelse(tdn_mgl > tdn_calib_upper_limit, "tdn outside calibration curve range", NA),
       npoc_lod_flag = ifelse(npoc_mgl < LOD_NPOC, "npoc below limit of detection", NA),
       tdn_lod_flag = ifelse(tdn_mgl < LOD_TN, "tdn below limit of detection", NA)) %>%
  ## combine flags into one column since one sample will not be below the LOD and above the cal curve at the same time
  mutate(npoc_flag = ifelse(!is.na(npoc_lod_flag), npoc_lod_flag, npoc_flag),
         tdn_flag = ifelse(!is.na(tdn_lod_flag), tdn_lod_flag, tdn_flag)) %>%
  select(campaign:tdn_mgl, npoc_flag, tdn_flag)

  
# 9. Clean data ----------------------------------------------------------------

## Helper function to calculate mean if numeric, otherwise preserve the value of
## the first observation (needed to preserve dates, which are different for duplicated kits)
mean_if_numeric <- function(x){
  ifelse(is.numeric(x), mean(x, na.rm = TRUE), first(x))
}

## Another step before finalizing is taking care of pesky duplicates from reruns
npoc_duplicates_removed <- npoc_bc_flagged %>%  
  ## make sure date is back to a character variable so no math is done on the date
  mutate(date = as.character(date)) %>%
  group_by(kit_id) %>% 
  summarize(across(everything(), .f = mean_if_numeric))

## Finalize Dataset
npoc <- npoc_duplicates_removed %>% 
  select(date, campaign, kit_id, transect_location, npoc_mgl, tdn_mgl, contains("_flag"))

# 10. Check with Metadata for missing:

source("./Processing_Scripts/Metadata_kit_list.R")

metadata_collected %>% 
  filter(sample_method == "vial_40ml") -> meta_filter

npoc %>% 
  full_join(meta_filter, by = c("campaign", "kit_id", "transect_location")) -> full_npoc_tdn

full_npoc_tdn %>% 
  mutate(npoc_flag = case_when(!is.na(notes) ~ notes,
                               TRUE ~ npoc_flag)) %>% 
  select(campaign, kit_id, transect_location, npoc_mgl, npoc_flag) -> full_npoc

full_npoc_tdn %>% 
  mutate(tdn_flag = case_when(!is.na(notes) ~ notes,
                               TRUE ~ tdn_flag)) %>% 
  select(campaign, kit_id, transect_location, tdn_mgl, tdn_flag) -> full_tdn

# 11. Write L1 data -----------------------------------------------------------

write_csv(full_npoc, paste0("Data/Processed/ec1_water_npoc_L1_", Sys.Date(), ".csv"))
write_csv(full_tdn, paste0("Data/Processed/ec1_water_tdn_L1_", Sys.Date(), ".csv"))

L1directory = "https://drive.google.com/drive/folders/1yhukHvW4kCp6mN2jvcqmtq3XA5niKVR3"

drive_upload(media = paste0("./ec1_water_npoc_L1_", Sys.Date(), ".csv"), name= paste0("ec1_water_npoc_L1_", Sys.Date(), ".csv"), path = L1directory )
drive_upload(media = paste0("./ec1_water_tdn_L1_", Sys.Date(), ".csv"), name= paste0("ec1_water_tdn_L1_", Sys.Date(), ".csv"), path = L1directory )

file.remove(paste0("./ec1_water_npoc_L1_", Sys.Date(), ".csv"))
file.remove(paste0("./ec1_water_tdn_L1_", Sys.Date(), ".csv"))
