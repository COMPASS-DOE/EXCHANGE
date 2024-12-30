## This is a data processing script for EXCHANGE, a sub-project of THE DOE-funded 
## COMPASS project (https://compass.pnnl.gov/). 
##
## This script imports raw data for Total Organic Carbon measured using
## a Vario IsotopeCube (Elementar) at PNNL-Richland
## and exports clean, Level 1 QCd data.
## Data are read in from the COMPASS Google Drive.
## 
## Created: 2024-11-14
## Kaizad Patel
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
directory <- "https://drive.google.com/drive/folders/15lH7R488uPxzmxVDVH7myrvVFs3xWoOs" 

## Define constants
F1_MIN <- 0
F1_MAX <- 100

## Define analyte
var <- "TOC"

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
      df <- read.csv(path, na = "")
      #  df <- read.delim(path, skip = 2)
      df[["source"]] <- rep(path, nrow(df))
      df}))
  
#  dat <- 
#    dat %>% 
#    filter(!is.na(aa)) %>% 
#    filter(!is.na(carbon_element_name))
  
  ## d. delete the temporary files
  file.remove(c(files$name))  
  
  ## e. output
  dat
}

data_raw = import_data(directory)

#
# 3. Process data --------------------------------------------------------------

data_primitive = 
  data_raw %>% 
  janitor::clean_names() %>% 
  mutate(
    date_run = str_extract(source, "[0-9]{8}"),
    date_run = ymd(date_run)) %>% 
  rename(toc_percent = c) %>% 
  filter(!grepl("skip", memo, ignore.case = T))

samples = 
  data_primitive %>% 
  filter(grepl("K", name)) %>% 
  mutate(name = toupper(name)) %>% 
  separate(name, sep = "_", into = c("kit_id", "transect_location"), remove = F) %>% 
  mutate(transect_location = case_match(transect_location, "U" ~ "Upland", "T" ~ "Transition", "W" ~ "Wetland"))


samples_clean =
  samples %>% 
  dplyr::select(kit_id, transect_location, toc_percent) %>% 
  group_by(kit_id, transect_location) %>% 
  dplyr::mutate(mean = mean(toc_percent),
                   n = n(),
                   sd = sd(toc_percent),
                   cv = 100 * sd/mean)







samples %>% 
  ggplot(aes(x = transect_location, y = toc_percent))+
  geom_jitter(width = 0.2)+
  facet_wrap(~kit_id)




data_primitive %>% 
  filter(!memo %in% "skip") %>% 
  filter(grepl("_", name)) %>% 
  ggplot(aes(x = c_factor))+
  geom_density()+
  xlim(0.5, 1.5)


samples %>% 
  ggplot(aes(x = transect_location, y = toc_percent))+
  geom_boxplot(fill = NA, width = 0.25)+
  geom_jitter(width = 0.1, size = 2)+
  theme_bw()

