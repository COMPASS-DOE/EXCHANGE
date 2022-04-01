########################### #
########################### #

## EXCHANGE EC1
## PROCESS IONS DATA

# This script will import cations and anions data
# Cations = c("Lithium", "Sodium", "Ammonium", "Potassium", "Magnesium", "Calcium")
# Anions = c("Nitrite", "Nitrate")

# The input data are in shitty, non-tidy format, with multi-line headers and multiple chunks of data per dataframe.  
# Use this script to assign the ions and turn it into tidy format, and then process and clean the dataframe

# KFP, 2022-02-20

########################### #
########################### #

# Step 1. load packages ----
library(tidyverse)

# Step 2. function to import and tidy files ----

# this function will do the initial work to make the dataframe tidy
# input parameters include (a) the dataframe being cleaned and (b) the ions in question.
# NOTE: You must include ALL the ions reported in the file

assign_ions = function(FILEPATH, PATTERN, IONS){
  
  # a. read and combine files ----
  
  # pull a list of file names in the target folder with the target pattern
  # then read all files and combine
  filePaths <- list.files(path = FILEPATH, pattern = PATTERN, full.names = TRUE)
  # filePaths <- list.files(path = FILEPATH, pattern = c(".xls"), full.names = TRUE)
  
  dat <- do.call(rbind, lapply(filePaths, function(path) {
    # then add a new column `source` to denote the file name
    df <- readxl::read_xls(path, skip = 2)
    df[["source"]] <- rep(path, nrow(df))
    df}))
  
  # b. start processing the ions data ----
  
  # identify the rows that contain ions names
  label_rows = which(grepl(paste(IONS, collapse = "|"), dat$Time))
  
  # make this a dataframe/tibble
  label_rows_df = 
    label_rows %>% 
    as_tibble() %>%
    rename(Row_number = value) %>% 
    mutate(label = TRUE, 
           Row_number = as.character(Row_number))
  
  # now join this to the dataframe
  data_new = 
    dat %>% 
    rownames_to_column("Row_number") %>% 
    left_join(label_rows_df) %>% 
    mutate(Ion = case_when(label ~ Amount)) %>% 
    # ^ this pulls the Ion name only for certain rows
    # use fill() to down-fill the values
    # it will down-fill until it hits the next non-empty cell
    # therefore, make sure to include ALL ion names in the IONS parameter
    fill(Ion) %>% 
    dplyr::select(-Row_number, -label)
  
  # create header by collapsing the header + first row
  new_header = data_new %>% 
    colnames() %>% 
    paste0(data_new[1,]) %>% 
    str_remove_all("NA")
  # the "source" column has "source" with a lot more crap
  # use grepl to replace that full value with just "source"
  new_header = replace(new_header, grep("source", new_header), "source")
  
  # set column names for the dataframe
  names(data_new) <- new_header  
  
  # preliminary processing to make it tidy
  data_new_processed = 
    data_new %>% 
    filter(!is.na(`No.`)) %>% 
    mutate_at(vars(-Name, -Ion, -source), as.numeric) %>% 
    mutate(date_run = str_extract(source, "[0-9]{8}"),
           date_run = lubridate::as_date(date_run)) %>% 
    dplyr::select(Name, Amount, Ion, date_run) %>% 
    force()
  
  data_new_processed
  
}


#
# Step 3. Run the function ------------------------------------------------

# this is the general format to run the function:
# assign_ions(FILEPATH = , # folder/location where all the files are stored
#             PATTERN = , # the pattern used to ID the target files, 
#                         # e.g. "Anion_UV" for nitrite/nitrite, "Cation", or just ".xls" for all .xls files
#             IONS = ) # list of ions present in the file
#                      # this could be c("Nitrite", "Nitrate"), etc. or just use `all_ions` below for the full list

all_ions = c("Lithium", "Sodium", "Ammonium", "Potassium", "Magnesium", "Calcium", "Nitrite", "Nitrate",
             "Chloride", "Bromide", "Sulfate", "Phosphate", "Fluoride")

data_ions_assigned = assign_ions(FILEPATH = "data/ions/ions data without dilution correction", 
                                    PATTERN = ".xls",
                                    IONS = all_ions)





