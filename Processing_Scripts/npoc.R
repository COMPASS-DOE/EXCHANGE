## This script imports raw data for NPOC and TDN measured using a Shimadzu TOC-L
## at PNNL MCRL and exports clean, Level 1 QC'ed data. 
## Data are read in from Google Sheets.
## 
## Created: 2022-01-15 (Updated 2022-03-24)
## Peter Regier
##
# #############
# #############

# 220202 - need to add in LODs (from Sammi), then get blank values for each
# run (average across the full run, don't use condblanks), then test if 
# blanks are higher than LOD, and if they are, blank-correct. This should all
# be done in a single function probably. Next up is flagging, and not sure
# what we need to flag for, but maybe something related to LOD?

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


# 2. Import data ---------------------------------------------------------------

## First, authorize drive access
drive_auth()

## Takes forever to find, looks through all GDrive files - would like to figure
## out a better alternative....
file_names <- drive_find(pattern = "_Summary_")

## Download the files from drive then read in one by one to a list
file_list <- list() # Initialize a list to fill with the for loop
for(i in 1:nrow(file_names)){
  path <- drive_download(file_names$name[i], overwrite = T) # this downloads files
  # to your local environment (based on project I think)
  date <- substr(file_names$name[i], 1, 8) # scrape the data from the file
  file_list[[i]] <- read_delim(paste0("./", path$path), skip = 10, delim = "\t") %>% 
    rename(sample_name = `Sample Name`, 
           npoc_raw = `Result(NPOC)`, 
           tn_raw = `Result(TN)`) %>% 
    select(sample_name, npoc_raw, tn_raw) %>% 
    mutate(date = date)
}


# 3. Set up functions ----------------------------------------------------------

## This function calculates blanks for each run as the mean
calculate_blanks <- function(file){ 
  x <- file %>% filter(grepl("Blank", sample_name))
  date <- first(x$date)
  npoc_blank = round(mean(x$npoc_raw[!is.na(x$npoc_raw)]), 2)
  tn_blank = round(mean(x$tn_raw[!is.na(x$tn_raw)]), 3)
  
  return(c(date = date, 
           npoc_blank_raw = npoc_blank, 
           tn_blank_raw = tn_blank))
}

## This function pulls only EC1 files to keep
pull_ec1_data <- function(file){
  file %>% filter(grepl("EC1_K", sample_name)) %>% 
    mutate(sample_name = substr(sample_name, 5, 9))
}

## This function blank-corrects NPOC and TN
blank_correct <- function(file){
    date = first(file$date)
    npoc_blank = blanks$npoc_blank[which(blanks$date == date)]
    tn_blank = blanks$tn_blank[which(blanks$date == date)]
    
    file %>% 
      mutate(npoc_blank = npoc_blank, 
             tn_blank = tn_blank, 
             npoc_mgl = npoc_raw - npoc_blank, 
             tn_mgl = tn_raw - tn_blank)
}


# 4. Blank-correct the data ----------------------------------------------------

## Create a final blanks dataset
blanks <- bind_rows(lapply(file_list, calculate_blanks)) %>%
  mutate(npoc_blank_raw = as.numeric(npoc_blank_raw), 
         tn_blank_raw = as.numeric(tn_blank_raw)) %>% 
  mutate(npoc_blank = ifelse(npoc_blank_raw > lod_npoc, npoc_blank_raw, 0), 
         tn_blank = ifelse(tn_blank_raw > lod_tn, tn_blank_raw, 0))
  
df <- lapply(file_list, pull_ec1_data) %>% 
  lapply(., blank_correct) %>% 
  bind_rows()

write_csv(df, "data/220217_npoc_provisional.csv")


### Pick up here, figure out how to blank-correct - this also needs to take into
### account the readme (probably) to figure out which of the TNs actually need
### to be scraped because they're sus














## CondBlank = initial blanks
## STD_ are standards for curve - can ignore
## 

## First: blank-correct each chunk of samples with blank run AFTER samples
## Second: need to use CKSTD to set data quality


## Goals before Workshop 2.2
## 1. Blank-correct
## 2. Merge with metdata and plot boxplots by region





