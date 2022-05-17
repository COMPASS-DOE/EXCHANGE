## This is a data processing script for EXCHANGE, a sub-project of THE DOE-funded 
## COMPASS project (https://compass.pnnl.gov/). 
##
## This script imports raw data for oxygen concentrations measured during soil and
## sediment incubations conducted at MCRL. Incubations were conducted with seawater
## for 24 hours to measure the rate at which soils and sediments consumed oxygen.
##
## Data are read in from a local drive, then exported as clean, Level 0B QC'ed data. 
##
###### # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
## IMPORTANT: THESE DATA ARE PROCESSED USING ASSUMPTIONS/THRESHOLDS/DECISIONS
## THAT ARE UNDER DISCUSSION AND ARE SUBJECT TO CHANGE DRAMATICALLY AS THESE 
## DATA MATURE. PLEASE TREAT THIS SCRIPT HAS VERY PRELIMINARY!
###### # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
## 
## Created: 2022-05-17
## Peter Regier
##
# ############# #
# ############# #

#
# 1. Setup environment ---------------------------------------------------------
cat("Setup")

## Load packages
require(pacman)

p_load(cowsay, 
       tidyverse, 
       magrittr, # 'set_columns'
       parsedate, # 'parse_date'
       tictoc) # time long operations

## Welcome
say("Welcome to EXCHANGE!", by = "random")

## Define constants
o2_min = 0 ## O2 concentrations cannot be less than 0
o2_max = 10 ## define upper limit: seawater was not super-saturated
rate_max = 50 ## because observed oxygen rates seem high, and are highly
## responsive to changes in cleaning/calculation steps, setting a max rate
anoxia_threshold = 0.1 ## define threshold for anoxia to trim end of exps

## Define analyte
var <- "o2"

## Define local location where data are stored
local_path = "/Users/regi350/OneDrive - PNNL/Documents/projects/compass/exchange_soil_gas_paper/data/raw/firesting/"

#
# 2. Functions -----------------------------------------------------------------

## Function to read in data from one run (one folder)
import_firesting_data <- function(folder) {
  
  ## Grab kit name, which is stored in the folder name
  kit_id <- str_match(folder, "[K][0-9]{3}")[,1]
  
  ## First, set up 4 filepaths for the four channels (each is a dataset)
  ## For these exps: Ch1 = sediment, Ch2 = upland, Ch3 = wetland, Ch4 = transition
  sed_filepath <- paste0(local_path, folder, "/ChannelData/Firesting O2 (4 Channels)_(A Ch.1)_Oxygen.txt")
  up_filepath <- paste0(local_path, folder, "/ChannelData/Firesting O2 (4 Channels)_(A Ch.2)_Oxygen.txt")
  wet_filepath <- paste0(local_path, folder, "/ChannelData/Firesting O2 (4 Channels)_(A Ch.3)_Oxygen.txt")
  tr_filepath <- paste0(local_path, folder, "/ChannelData/Firesting O2 (4 Channels)_(A Ch.4)_Oxygen.txt")
  
  ## Next, create four dataframes for the four channels - this is done separately
  ## for each type of soil/sediment
  sed_data <- read_delim(sed_filepath, delim = "\t", skip = 25,  col_names = F) %>% 
    magrittr::set_colnames(firesting_names) %>% 
    mutate(datetime = parsedate::parse_date(paste(date, time))) %>% 
    dplyr::select(datetime, dt_s, temp_c, do_mgl_raw) %>% 
    mutate(site = "Sediment", 
           kit_id = kit_id)
  
  up_data <- read_delim(up_filepath, delim = "\t", skip = 25,  col_names = F) %>% 
    magrittr::set_colnames(firesting_names) %>% 
    mutate(datetime = parsedate::parse_date(paste(date, time))) %>% 
    dplyr::select(datetime, dt_s, temp_c, do_mgl_raw) %>% 
    mutate(site = "Upland", 
           kit_id = kit_id)
  
  wet_data <- read_delim(wet_filepath, delim = "\t", skip = 25,  col_names = F) %>% 
    magrittr::set_colnames(firesting_names) %>% 
    mutate(datetime = parsedate::parse_date(paste(date, time))) %>% 
    dplyr::select(datetime, dt_s, temp_c, do_mgl_raw) %>% 
    mutate(site = "Wetland", 
           kit_id = kit_id)
  
  tr_data <- read_delim(tr_filepath, delim = "\t", skip = 25,  col_names = F) %>% 
    magrittr::set_colnames(firesting_names) %>% 
    mutate(datetime = parsedate::parse_date(paste(date, time))) %>% 
    dplyr::select(datetime, dt_s, temp_c, do_mgl_raw) %>% 
    mutate(site = "Transition", 
           kit_id = kit_id)
  
  ## Finally, combine into a single dataframe
  df <- bind_rows(sed_data, up_data, wet_data, tr_data)
  
  ## Print the first lines to make sure it looks right
  print(head(df))
  
  return(df)
}


# 3. Import data ---------------------------------------------------------------
## tell me what you're doing
cat("Importing", var, "data...")

## Set raw firesting data column names
firesting_names <- c("date", "time", "dt_s", "do_mgl_raw", "dphi", 
                     "intensity_mv", "light_mv", "do_status", "temp_date", "temp_time", 
                     "temp_dt_s", "temp_c", "temp_status", "press_date", "press_time", 
                     "press_dt_s", "pressure_mbar", "press_status")

## Create a list of folders (each is a separate experiment) to import
folders <- list.files(local_path)

## Import all data and merge
firesting_raw <- map(folders, import_firesting_data) %>% 
  bind_rows()


# 4. Clean up data -------------------------------------------------------------

## Do initial cleaning using multiple filtering steps (see comments below). The
## anoxia threshold is currently set at 0.2 mg/L
firesting_clean_initial <- firesting_raw %>% 
  filter(do_mgl_raw >= o2_min & do_mgl_raw <= o2_max) %>% ## filter out values outside limits
  filter(dt_s < 60 * 60 * 24) %>%  ## filter out data collected after 24 hours
  group_by(kit_id, site) %>% 
  filter(dt_s <= dt_s[which.min(do_mgl_raw)]) %>% ## remove values after min DO
  filter(dt_s >= dt_s[which.max(do_mgl_raw)]) %>% ## remove values before max DO
  filter(do_mgl_raw > anoxia_threshold) ## remove values below anoxia threshold

## Removing deadspace before sensors are inserted is a little more complicated, 
## so it gets its own step here. This is a workaround, but feeding mutate() with 
##remove_plateau() after group_by() got complicated... 
##More elegant solutions always appreciated!
split_groups <- group_split(firesting_clean_initial %>% 
                              mutate(d_do = lead(do_mgl_raw) - do_mgl_raw))

## This function removes plateaus at the beginning of each experiment which occur
## because all sensors start logging at the same time, but each sensor is 
## inserted into the vial individually, resulting in lags. Thus, plateaus are
## artifacts of our experimental design. I clean them out by identifying the 
## point where the difference between two adjacent points is greater than a
## threshold (0.02 mg/L here, based on visual examination of several exps). Then,
## because low variance occurs at the end of each experiment as well, we find the
## first time-point where plateau conditions stop, and trim datasets based on 
## that time-point. 
remove_plateau <- function(data){
  
  ## Set a threshold for minimum change between adjacent measurements to 
  ## algorithmically determine when experiments start
  d_threshold = 0.02
  
  ## Calculate the start time of the experiment (end of plateau)
  start_time <- data %>% 
    filter(abs(d_do) > d_threshold) %>% ## find values with change > threshold 
    slice(1) %>% ## pull the first row with a large change (i.e., start of exp)
    pull(dt_s) ## pull that time-stamp
  
  ## This cleans up one exp (group #21) that returned a logical...
  start_time = ifelse(is.logical(start_time), 1, start_time)
  
  ## Add a column for DO with the plateau removed
  data %>% 
    mutate(do_mgl = ifelse(dt_s >= start_time, do_mgl_raw, NA))
}

## Clean each group, and return the final dataset with 
firesting_clean <- dataset_list %>% 
  map(remove_plateau) %>%
  bind_rows()



# 5. Calculate rates -----------------------------------------------------------

## Calculate rates as (oxygen_max - oxygen_min) / (t_max - t_min)
firesting_rates_raw <- firesting_clean %>% 
  drop_na() %>% ## remove rows NA'd by remove_plateau()
  rename("transect_location" = site) %>%
  group_by(kit_id, transect_location) %>% 
  dplyr::summarize(max_do = max(do_mgl, na.rm = T),
                   min_do = min(do_mgl, na.rm = T), 
                   min_dt = min(dt_s, na.rm = T), 
                   max_dt = max(dt_s, na.rm = T)) %>% 
  mutate(campaign = "EC1", 
         delta_do_s = (max_do - min_do) / (max_dt - min_dt), 
         delta_do = max_do - min_do, 
         delta_do_hr = delta_do_s * 60 * 60) %>% 
  dplyr::select(campaign, kit_id, transect_location, delta_do_hr)


# 6. Clean rates data ----------------------------------------------------------

clean_data <- function(data) {
  data %>% 
    mutate(delta_do_hr_flag = ifelse(delta_do_hr > rate_max, "TRUE", NA))
}

firesting_rates <- clean_data(firesting_rates_raw)

# 6. Write out data ------------------------------------------------------------

date_updated <- "20220517"

## Write out dataset used to calculate rates
#write_csv(firesting_clean, "data/firesting_full_dataset.csv")

## Write out dataset with calculated rates
write_csv(firesting_rates, paste0("Data/Processed/EC1_OxygenDrawdown_L0B_", date_updated, "_PRELIMINARY.csv"))

