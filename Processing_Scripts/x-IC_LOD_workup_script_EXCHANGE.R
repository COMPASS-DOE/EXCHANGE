################# IC LOD Workup Script ##################
### By Opal & Allison 
## Last Updated: Oct 2022

#Steps: 
#This will calculate run specific LODs:
# 1.	Establish Constants & Load Packages 
# 2.  Import blank concentrations
# 3.  Calculate averages and SDs of blank concentrations per run 
# 4.	Import slope of the standard curve for each ion – slope is the signal response  
# 5.	Calculate LOD and LOQ per run 

# Definitions & Calculations:

# 1. LOD = limit of detection calculated from Harris et al & Harvey et al # Need to link DOIs still #
# 2. (SA)DL= detection limit of analyte (signal)
# 3. Sreag = average blank signal
# 4. z = confidence interval (3 = 99%)
# 5. σreag = standard deviation of blank signals

# (SA)DL = Sreag + zσreag
# [LOD] = (SA)DL / m 

#1. Establish Constants & Load Packages :
z <- 3 # z = confidence interval (3 = 99%)

require(pacman)
pacman::p_load(cowsay,
               tidyverse,
               googlesheets4, # read_sheet 
               googledrive # drive_upload
)

#### 2.  Import blank concentrations ####

#2a. confirm working directory is ../GitHub/EXCHANGE#
getwd()

#2b. Import data 
#Need:
## URL for data
# data_path = "xxxx" 

## Define analyte
var <- "ions"

cat("Importing", var, "data...")

# `import_data`: this function will import all xls files in the target directpry and combine them
# input parameters are (a) FILEPATH, the target directory with the raw data files

import_data = function(directory){
  
  ## a. Create a list of files to download
  files <- 
    drive_ls(directory) %>% 
    filter(grepl("_Data_Raw_", name))
  
  ## b. Download files to local (don't worry, we'll delete em in a sec)
  lapply(files$id, drive_download, overwrite = TRUE)
  
  
  ## c. pull a list of file names
  ## then read all files and combine
  
  filePaths <- files$id
  dat <- 
    do.call(rbind, lapply(filePaths, function(path){
      # then add a new column `source` to denote the file name
      df <- readxl::read_excel(path, skip = 2)
      #  df <- read.delim(path, skip = 2)
      df[["source"]] <- rep(path, nrow(df))
      df}))
  
  ## d. delete the temporary files
  file.remove(c(files$id))  
  
  ## e. output
  dat
}

# Now, Import all the ions data:
directory = "https://drive.google.com/drive/u/1/folders/1tp6X9bJYFjsc9fda5R8aGBhiNMUfpJsN"

raw_data = import_data(directory)

#2d. Process BLANKS
process_blanks = function(raw_data, IONS){
  
  # The input data are in shitty, non-tidy format, with multi-line headers and multiple chunks of data per dataframe.  
  # This function assigns the ions and turns it into tidy format, then cleans/processes the dataframe
  
  # a. assign ions ----
  
  # identify the rows that contain ions names
  label_rows = which(grepl(paste(IONS, collapse = "|"), raw_data$Time))
  
  # make this a dataframe/tibble
  label_rows_df = 
    label_rows %>% 
    as_tibble() %>%
    rename(Row_number = value) %>% 
    mutate(label = TRUE, 
           Row_number = as.character(Row_number))
  
  # now join this to the dataframe
  data_new = 
    raw_data %>% 
    rownames_to_column("Row_number") %>% 
    left_join(label_rows_df) %>% 
    mutate(Ion = case_when(label ~ Area)) %>% 
    # ^ this pulls the Ion name only for certain rows
    # use fill() to down-fill the values
    # it will down-fill until it hits the next non-empty cell
    # therefore, make sure to include ALL ion names in the IONS parameter
    fill(Ion) %>% 
    dplyr::select(-Row_number, -label)
  
  # the dataframe now has all the ions assigned to each row
  # but it is still horribly untidy
  
  # b. clean the dataframe -----
  
  # preliminary processing to make it tidy
  blanks_new_processed = 
    data_new %>% 
    filter(!is.na(`No.`), 
           grepl("Blank", Name)) %>% 
    filter(!grepl("CondBlank", Name)) %>% #Remove conditioning blanks 
    filter(!Name %in% c("Blank1", "Blank2", "Blank3", "Blank4")) %>% #filtering out carry over blanks 
    mutate_at(vars(-Name, -Ion, -source), as.numeric) %>% 
    # pull the date run from the long `source` column
    mutate(date_run = str_extract(source, "[0-9]{8}"),
           date_run = lubridate::as_date(date_run)) %>%
    dplyr::select(Name, Area, Ion, date_run) %>% 
    mutate(Ion = str_remove_all(Ion, "_UV")) %>% 
    force()
  
  blanks_new_processed
  
}

#assign ions of interest: 
all_ions = c("Lithium", "Sodium", "Ammonium", "Potassium", "Magnesium", "Calcium", "Nitrite", "Nitrate",
             "Chloride", "Bromide", "Sulfate", "Phosphate", "Fluoride")

#Now run function: 
blanks_ions_processed = process_blanks(raw_data, IONS = all_ions)

#### 3.  Calculate averages and SDs of blank concentrations per run ####
Sreag.avg <- aggregate(Area ~ Ion + date_run, data = blanks_ions_processed, mean)

σreag.sd <- aggregate(Area ~ Ion + date_run, data = blanks_ions_processed, sd)  

colnames(Sreag.avg)[colnames(Sreag.avg) == 'Area'] <- 'Sreag'
colnames(σreag.sd)[colnames(σreag.sd) == 'Area'] <- 'σreag'

SADL_cal <- dplyr::left_join(Sreag.avg,σreag.sd, by=c("Ion","date_run"))

SADL_cal[is.na(SADL_cal)] = 0

SADL_cal <- SADL_cal %>% dplyr::mutate(SADL = Sreag + (z*σreag))

#### 4.	Import slope of the standard curve for each ion – slope is the signal response   #####

### Function to import slopes data:

import_slopes = function(directory){
  
  ## a. Create a list of files to download
  files <- 
    drive_ls(directory) %>% 
    filter(grepl("_Slope_", name))
  
  ## b. Download files to local (don't worry, we'll delete em in a sec)
  lapply(files$id, drive_download, overwrite = TRUE)
  
  
  ## c. pull a list of file names
  ## then read all files and combine
  
  filePaths <- files$id
  dat <- do.call(rbind, lapply(filePaths, function(path) {
    # then add a new column `source` to denote the file name
    df <- readxl::read_xls(path, skip = 7)
    df[["source"]] <- rep(path, nrow(df))
    df}))
  
  ## d. delete the temporary files
  file.remove(c(files$id))  
  
  ## e. output
  dat
}

#import slopes:
raw_slopes = import_data(slope.directory)
slope.directory = "https://drive.google.com/drive/u/1/folders/1pf5oxzg15uB0twTl76sEGD0Ayan8ckBy"

assign_slopes = function(raw_slopes, IONS){
  # identify the rows that contain ions names
  label_rows = which(grepl(paste(IONS, collapse = "|"), dat$`Peak Name`))
  
  # make this a dataframe/tibble
  label_rows_df = 
    label_rows %>% 
    as_tibble() %>%
    dplyr::rename(Row_number = value)  %>%
    mutate(label = TRUE, 
          Row_number = as.character(Row_number))
  
  # now join this to the dataframe
  slope_new <-  dat %>% 
    tibble::rownames_to_column("Row_number") %>% 
   right_join(label_rows_df) %>% 
   dplyr::select(-Row_number, -label)
  
  # preliminary processing to make it tidy
  slope_new_processed = 
    slope_new %>% 
    mutate_at(vars(-'Peak Name', -source, -Cal.Type, -Points, -Offset, -Slope, -Curve, -Coeff.Det., -Eval.Type), as.numeric) %>% 
    mutate(Date_Run.1 = str_extract(source, "[0-9]{8}"),
           Date_Run = lubridate::as_date(Date_Run.1)) %>% 
    dplyr::select('Peak Name', Slope, Date_Run) %>% 
    force()
  
  slope_new_processed
  
}

#output from slope file needs to be Ion, Slope, Date

m.run = assign_slopes(raw_slopes,IONS = all_ions)

colnames(m.run)[colnames(m.run) == 'Peak Name'] <- 'Ion'


#LOD.run = SADL_cal / m.run 

data_m_sadl <- dplyr::left_join(SADL_cal,m.run, by=c("Ion","Date_Run"))
data_m_sadl <- data_m_sadl %>% filter(!is.na(Slope)) %>% 
  mutate(LOD.run = SADL / as.numeric(Slope))

save(data_m_sadl, file="LODs_byrun_COMPASS.rda")

