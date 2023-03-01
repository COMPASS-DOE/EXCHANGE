## EXCHANGE Metadata List 

## This is a data processing script for EXCHANGE, a sub-project of the DOE-funded 
## COMPASS project (https://compass.pnnl.gov/). 


#This Kit imports metadata from the consortium and outputs: 
#Kit ID, Sample Type (Water, Soil, Sediment), Transect Location, Collected (T/F)

## Data are read in from the COMPASS Google Drive.
## 
## Created: 2023-02-27
## Allison Myers-Pigg

# 1. Setup ---------------------------------------------------------------------
cat("Setup")

## Define 
var <- "metadata"

# load packages
require(pacman)
pacman::p_load(cowsay,
               tidyverse,
               googlesheets4, # read_sheet 
               googledrive # drive_upload
)

## Welcome
say("Welcome to EXCHANGE!", by = "random")

#Set Directory 
metadata_directory = "https://drive.google.com/drive/folders/1IQUq_sD-Jama7ajaZl1zW_9zlWfyCohn"

## Next, list all files in the directory, filter out the Kit Level file and snag the file name
metadata_file <- drive_ls(metadata_directory) %>%
  filter(grepl("KitLevel", name)) %>%
  pull(name)


## Now download that file to your local (an annoying googledrive requirement, we will delete this in a minute)
drive_download(metadata_file, overwrite = T)

# make a dataframe
metadata_collected_raw <- read_csv(metadata_file) 

#delete file on the local
file.remove(metadata_file)

# 2. Import metadata ----------------------------------------------------------

cat("Importing", var, "...")

## Finally, read those data in and format so we can set up a list of all samples received
metadata_collected <- metadata_collected_raw %>%
  select(kit_id, samples_collected) %>%
  mutate(Water = ifelse(str_detect(samples_collected, "Water"), T, F),
         Sediment = ifelse(str_detect(samples_collected, "Sediment"), T, F),
         Wetland = ifelse(str_detect(samples_collected, "Wetland"), T, F),
         Transition = ifelse(str_detect(samples_collected, "Transition"), T, F),
         Upland = ifelse(str_detect(samples_collected, "Upland"), T, F)) %>%
  pivot_longer(cols = c(Water, Sediment, Wetland, Transition, Upland),
               names_to = "transect_location", values_to = "collected") %>%
  mutate(sample_type= case_when(str_detect(transect_location, "Upland") ~ "Soil",
                                str_detect(transect_location, "Wetland") ~ "Soil",
                                str_detect(transect_location, "Transition") ~ "Soil",
                                str_detect(transect_location, "Water") ~ "Water",
                                str_detect(transect_location, "Sediment") ~ "Sediment"),
         campaign = "EC1") %>%
select(-samples_collected) %>%
select(campaign, kit_id, transect_location, sample_type, collected)

# 3. Export cleaned metadata --------------------------------------------------
cat("Exporting", var, "...")

#Write to GoogleDrive

#https://googledrive.tidyverse.org/reference/drive_upload.html