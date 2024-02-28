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
metadata_directory = "https://drive.google.com/drive/u/1/folders/1xy9nsTpbiSk-KXrcDEeFQ7gg4350DuH-"

## Next, list all files in the directory, filter out the Kit Level file and snag the file name
metadata_file <- drive_ls(metadata_directory) %>%
  filter(grepl("kitlevel", name)) %>%
  pull(name)


## Now download that file to your local (an annoying googledrive requirement, we will delete this in a minute)
drive_download(metadata_file, overwrite = T)

# make a dataframe
metadata_collected_raw <- read_csv(metadata_file) 

sample_kit <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/18JlGJzeQaqtkTPJyuzsAZWB8fuhcyDZ8ZekGUlanCQ8/edit#gid=549612035")

#delete file on the local
file.remove(metadata_file)

# 2. Import metadata ----------------------------------------------------------

cat("Importing", var, "...")

removed_kits <- tibble(kit_id = c("K001", "K007"))

## Finally, read those data in and format so we can set up a list of all samples received
metadata_collected <- metadata_collected_raw %>%
  bind_rows(removed_kits) %>% 
  select(kit_id, samples_collected) %>%
  mutate(Water = ifelse(str_detect(samples_collected, "Water"), T, F),
         Sediment = ifelse(str_detect(samples_collected, "Sediment"), T, F),
         Wetland = ifelse(str_detect(samples_collected, "Wetland"), T, F),
         Transition = ifelse(str_detect(samples_collected, "Transition"), T, F),
         Upland = ifelse(str_detect(samples_collected, "Upland"), T, F)) %>%
  pivot_longer(cols = c(Water, Sediment, Wetland, Transition, Upland),
               names_to = "transect_location", values_to = "collected") %>%
  mutate(sample_type= case_when(str_detect(transect_location, "Upland") ~ "soil",
                                str_detect(transect_location, "Wetland") ~ "soil",
                                str_detect(transect_location, "Transition") ~ "soil",
                                str_detect(transect_location, "Water") ~ "water",
                                str_detect(transect_location, "Sediment") ~ "sediment"),
         campaign = "EC1",
         transect_location = tolower(transect_location)) %>%
select(-samples_collected) %>%
select(campaign, kit_id, transect_location, sample_type, collected) %>% 
  left_join(sample_kit, by = c("transect_location", "sample_type")) %>% 
  # next, we need to make manual edits by sample method based on kit tracking sheet https://docs.google.com/spreadsheets/d/19F1oS-DBvxQlU1EYXtciYkhu6TNA7fuCQ0Ts2NRWQlk/edit?usp=sharing
  mutate(collected = case_when(kit_id == "K014" & transect_location == "wetland" & sample_method == "jar" ~ TRUE, # collected sample for jar wetland
                               kit_id == "K052" & transect_location == "wetland" & sample_method %in% c("jar", "bag") ~ FALSE, # did not sample wetland jar or bag
                               kit_id == "K060" & !transect_location == "sediment" & sample_method %in% c("jar", "bag") ~ FALSE, # did not sample jar or bag did not sample hyprop for transition or upland
                               kit_id == "K060" & transect_location %in% c("transition","upland") & sample_method %in% c("hyprop") ~ FALSE, # did not sample hyprop for transition or upland
                               kit_id == "K029" & sample_method %in% c("bag") ~ FALSE, # did not sample bags, put the jars inside the bags instead
                               kit_id == "K056" & sample_method %in% c("bag") ~ FALSE, # did not sample bags, put the jars inside the bags instead
                               kit_id == "K051" & transect_location == "sediment" & sample_method %in% c("jar", "bag") ~ FALSE, # did not sample sediment jar or bag
                               kit_id == "K058" & sample_method %in% c("bag") ~ FALSE, # did not sample bags, put the jars inside the bags instead
                               TRUE ~ collected),
         notes = case_when(kit_id == "K001" ~ "kit compromised",
                           kit_id == "K007" ~ "kit compromised",
                           kit_id == "K010" & sample_type == "sediment" & sample_method == "jar" ~ "sample compromised",
                           kit_id == "K018" & sample_type %in% c("sediment", "soil") ~ "sample compromised",
                           kit_id == "K014" & sample_method %in% c("vial_15ml", "vial_40ml") ~ "sample compromised in shipment",
                           kit_id == "K027" & sample_method %in% c("vial_40ml", "bottle_1l", "jar") ~ "sample compromised",
                           kit_id == "K057" & sample_method %in% c("vial_15ml", "vial_40ml") ~ "sample compromised in shipment")) %>% 
  arrange(kit_id)

# 3. Export cleaned metadata --------------------------------------------------
cat("Exporting", var, "...")

#Write to GoogleDrive

#https://googledrive.tidyverse.org/reference/drive_upload.html