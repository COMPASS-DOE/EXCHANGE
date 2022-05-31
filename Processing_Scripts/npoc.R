## This script imports raw data for NPOC and TDN measured using a Shimadzu TOC-L
## at PNNL MCRL and exports clean, Level 0B QC'ed data. 
## Data are read in from Google Drive
## 
## Created: 2022-01-15 (Updated 2022-03-30 with OO)
## Peter Regier
##
# #############
# #############

### QUESTION for team: We are setting blanks to 0 when the blank is less than LOD
### Does that make sense? Setting blanks to 0 when they are not? 


# 1. Setup ---------------------------------------------------------------------

# load packages
require(pacman)
pacman::p_load(tidyverse, # keep things tidy
               janitor, # useful for simplifying column names
               googlesheets4, # read_sheet 
               googledrive) # drive_ functions

## Set theme
theme_set(theme_bw())

## Set LOD (for all data after 10/11/2021)
## If any data were before 10/11/21, lod_npoc = 0.27, lod_tn = 0.070
lod_npoc <- 0.076
lod_tn <- 0.014

## Set GDrive URL for NPOC raw data files
directory = "https://drive.google.com/drive/folders/1Mkg5UCEzt9ifKCGyr-Kn9a3LL8XuGzBa"

# 2. Functions -----------------------------------------------------------------

## Create a function to read in data
read_data <- function(data){
  # First, scrape date from filename
  date <- substr(data, 1, 8)
  # Second, read in data
  read_delim(file = data, skip = 10, delim = "\t") %>% 
    rename(sample_name = `Sample Name`, 
           npoc_raw = `Result(NPOC)`, 
           tn_raw = `Result(TN)`) %>% 
    select(sample_name, npoc_raw, tn_raw) %>% 
    mutate(date = date)
}


# 3. Import data ---------------------------------------------------------------

## Create a list of files to download
files <- drive_ls(directory) %>% 
  filter(grepl("_Summary_", name))

## Download files to local (don't worry, we'll delete em in a sec)
lapply(files$name, drive_download, overwrite = TRUE)

## Read in data, filter to EC1 samples, and add sample name
npoc_raw <- files$name %>% 
  map(read_data) %>% 
  bind_rows() 

## Clean up local (delete downloaded files)
file.remove(c(files$name))


# 4. Calculate blanks and add to data ------------------------------------------

blanks <- npoc_raw %>% 
  filter(grepl("^Blank", sample_name)) %>% 
  group_by(date) %>% 
  summarize(npoc_blank_raw = round(mean(npoc_raw[!is.na(npoc_raw)]), 2), 
         tn_blank_raw = round(mean(tn_raw[!is.na(tn_raw)]), 2)) %>% 
  mutate(npoc_blank = ifelse(npoc_blank_raw > lod_npoc, npoc_blank_raw, 0), 
         tn_blank = ifelse(tn_blank_raw > lod_tn, tn_blank_raw, 0)) %>% 
  select(date, npoc_blank, tn_blank)


# 5. Add blanks data -----------------------------------------------------------

npoc_blank_corrected <- npoc_raw %>% 
  filter(grepl("EC1_K", sample_name)) %>% # filter to EC1 samples only
  mutate(campaign = "EC1", 
         kit_id = substr(sample_name, 5, 9), 
         transect_location = "water") %>% 
  inner_join(blanks, by = "date") %>% 
  mutate(npoc_mgl = npoc_raw - npoc_blank, 
         tn_mgl = tn_raw - tn_blank)


# 6. Clean data ----------------------------------------------------------------

## Helper function to calculate mean if numeric, otherwise first (needed to 
## preserve dates, which are different for duplicated kits)
mean_if_numeric <- function(x){
  ifelse(is.numeric(x), mean(x, na.rm = TRUE), first(x))
}

## Another step before finalizing is taking care of pesky duplicates from reruns
npoc_duplicates_removed <- npoc_blank_corrected %>% 
  select(campaign, transect_location, kit_id, date, npoc_mgl, tn_mgl, npoc_blank, tn_blank) %>% 
  group_by(kit_id) %>% 
  summarize(across(everything(), .f = mean_if_numeric))

## The last step is flagging data
npoc_raw_flags <- npoc_duplicates_removed %>% 
  ## First, round each parameter to proper significant figures
  mutate(npoc_mgl = round(npoc_mgl, 2), 
         tn_mgl = round(tn_mgl, 3)) %>% 
  ## Second, add flags for outside LOD
  mutate(`npoc outside range` = ifelse(npoc_mgl < lod_npoc | npoc_mgl > 30, T, F), #per cal curve upper limit
         `tn outside range` = ifelse(tn_mgl < lod_tn | tn_mgl > 3, T, F), 
         `npoc blank below LOD` = ifelse(npoc_blank == 0, T, F), 
         `tn blank below LOD` = ifelse(tn_blank == 0, T, F))

## gather both npoc-relevant flags into a single column
npoc_flags <- npoc_raw_flags %>% 
  pivot_longer(cols = c(`npoc outside range`, `npoc blank below LOD`), 
               names_to = "npoc_flag", values_to = "vals") %>% 
  filter(vals == TRUE) %>% select(-vals) %>% 
  group_by(kit_id) %>% 
  summarize(npoc_flag = toString(npoc_flag))

## gather both npoc-relevant flags into a single column
tn_flags <- npoc_raw_flags %>% 
  pivot_longer(cols = c(`tn outside range`, `tn blank below LOD`), 
               names_to = "tn_flag", values_to = "vals") %>% 
  filter(vals == TRUE) %>% select(-vals) %>% 
  group_by(kit_id) %>% 
  summarize(tn_flag = toString(tn_flag))

npoc <- npoc_raw_flags %>% 
  left_join(npoc_flags, by = "kit_id") %>% 
  left_join(tn_flags, by = "kit_id") %>% 
  select(date, campaign, kit_id, transect_location, npoc_mgl, tn_mgl, contains("_flag"))


# 7. Write data ----------------------------------------------------------------
date_updated <- "20220531"

write_csv(npoc, paste0("Data/Processed/EC1_NPOC_TN_L0B_", date_updated, ".csv"))



