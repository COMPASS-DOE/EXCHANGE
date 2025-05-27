## EXCHANGE-IONS
##
## This is a data processing script for EXCHANGE, a sub-project of the DOE-funded 
## COMPASS project (https://compass.pnnl.gov/). 
##
## This script imports raw data for ions, processes the data,
## and exports clean, Level 1 QCd data.
## 
## Major cations and anions were all measured using ion chromatography 
## and detected via conductivity, except for nitrate and nitrite, 
## which were detected via UV absorbance 
## (Wilson et al., 2011, https://doi.org/10.1093/chrsci/49.8.596) 
## on ThermoFisher Dionex ICS-6000 HPIC DP System at MCRL.
##

## Ions measured include: "Lithium", "Sodium", "Ammonium", "Potassium", 
## "Magnesium", "Calcium", "Nitrite", "Nitrate",
## "Chloride", "Bromide", "Sulfate", "Phosphate", "Fluoride"
##
## Data are read in from the COMPASS Google Drive.
## 
## Created: 2022-02-20
## Kaizad F. Patel
## Updated by AMP & SP 2023-02-21
## V2 by  AMP & KFP 2023-12-20
# ############# #
# ############# #

# 1. Setup ---------------------------------------------------------------------
cat("Setup")

# load packages
require(pacman)
pacman::p_load(cowsay,
               tidyverse,
               googlesheets4, # read_sheet 
               googledrive # drive_upload
)

## Welcome
say("Welcome to EXCHANGE!", by = "random")

## URL for data (Google)
directory = "https://drive.google.com/drive/u/1/folders/1tp6X9bJYFjsc9fda5R8aGBhiNMUfpJsN"

## Define analyte
var <- "ions"

#
# 2. Import data ----------------------------------------------------------

cat("Importing", var, "data...")

# Import files from Google Drive

#Create function: 
import_data = function(directory){
  
  ## a. Create a list of files to download
  files <- 
    drive_ls(directory) %>% 
    filter(grepl("_Data_Raw_", name))
  
  ## b. Download files to local (don't worry, we'll delete em in a sec)
  lapply(files$id, drive_download, overwrite = TRUE)
  
  
  ## c. pull a list of file names
  ## then read all files and combine
  
  filePaths <- files$name
  dat <- 
    do.call(rbind, lapply(filePaths, function(path){
      # then add a new column `source` to denote the file name
      df <- readxl::read_excel(path, skip = 2)
      df[["source"]] <- rep(path, nrow(df))
      df}))
  
  ## d. delete the temporary files
  file.remove(c(files$name))  
  
  ## e. output
  dat
}

## import the raw data files
raw_data = import_data(directory)

#convert text nas to real NAs
raw_data[raw_data == "n.a."] <- NA

## import the dilutions key
dilutions_key_wide = read_sheet("1ekMFJrzE_1dAzfFuLrDfrRUzp2b66WAgE4LRNZEYGy0")
run_log_withdates = read_sheet("https://docs.google.com/spreadsheets/d/1hazGv03RroD2tKmZgwE6DOmCYDRqMtMl6C7U6TLQ1go/edit?usp=sharing")

## readme files set directory
directory_readme = "https://drive.google.com/drive/u/1/folders/1PAa6Gtnthn9gUw5QhiyLOSLsBN1j7fpc"

#Create function: 
import_readme = function(directory_readme){
  
  ## a. Create a list of files to download
  files <- 
    drive_ls(directory_readme) %>% 
    filter(grepl("_Readme_", name))
  
  ## b. Download files to local (don't worry, we'll delete em in a sec)
  lapply(files$id, drive_download, overwrite = TRUE)
  
  
  ## c. pull a list of file names
  ## then read all files and combine
  
  filePaths <- files$name
  dat <- 
    do.call(dplyr::bind_rows, lapply(filePaths, function(path){
      # then add a new column `source` to denote the file name
      df <- readxl::read_excel(path)
      df[["source"]] <- rep(path, nrow(df))
      df}))
  
  ## d. delete the temporary files
  file.remove(c(files$name))  
  
  ## e. output
  dat
}

#import read mes:
readme_data = import_readme(directory_readme)

#

# 3. Process data ---------------------------------------------------------

#Create Function: 
# `process_dilutions_data`: this function will make longform and clean the dilutions map/key
# input parameters are (a) the dilutions key (wideform)
process_dilutions_data = function(dilutions_key_wide){
  dilutions_key_wide %>% 
    pivot_longer(cols = ends_with("_dilution"),
                 names_to = "Ion",
                 values_to = "dilution") %>% 
    mutate(Ion = str_remove(Ion, "_dilution"))
}

#Work on incorporating dilutions: 
dilutions_key_nodate = process_dilutions_data(dilutions_key_wide)

run_log_long = run_log_withdates %>% pivot_longer(cols= -"kit_id", names_sep = "_", names_to = c(".value", "run")) %>% na.omit()

dilutions_key = left_join(dilutions_key_nodate, run_log_long, by = c("kit_id","dilution"))  %>%
  mutate(date_run = lubridate::ymd(rundate)) %>% select(-c(run,rundate))

#Create Function:
# `process_data`: this function will assign ions and tidy the dataframe
# input parameters are (a) the dataframe being cleaned and (b) the ions in question.

process_data = function(raw_data, readme_data, IONS){
  
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
    left_join(label_rows_df, by = "Row_number") %>% 
    mutate(Ion = case_when(label ~ Amount),
           flag = case_when(is.na(Amount)  ~ "below instrument detection")) %>% 
    # ^ this pulls the Ion name only for certain rows
    # use fill() to down-fill the values
    # it will down-fill until it hits the next non-empty cell
    # therefore, make sure to include ALL ion names in the IONS parameter
    fill(Ion) %>% 
    dplyr::select(-Row_number, -label)
  
  # the dataframe now has all the ions assigned to each row
  # but it is still horribly untidy
  
  # b. clean the dataframe -----
  
  ### create header by collapsing the header + first row
  
  # preliminary processing to make it tidy
  data_new_processed = 
    data_new %>% 
    filter(!is.na(`No.`)) %>% 
    mutate_at(vars(-Name, -Ion, -source, -flag), as.numeric) %>% 
    # pull the date run from the long `source` column
    mutate(date_run = str_extract(source, "[0-9]{8}"),
           date_run = lubridate::as_date(date_run)) %>% 
    dplyr::select(Name, Amount, Area, Ion, date_run, flag) %>% 
    mutate(Ion = str_remove_all(Ion, "_UV"),
           Ion = tolower(Ion)) %>% 
    force()
  
  # now, format the readme data so we can join with the processed file
  readme_data2 = 
    readme_data %>% 
    rename(Name = `Sample Name`) %>% 
    mutate(date_run = str_extract(source, "[0-9]{8}"),
           date_run = lubridate::as_date(date_run)) %>% 
    dplyr::select(date_run, Name, Action, Dilution) %>% 
    force()
  
  data_new_processed_readme = 
    data_new_processed %>% 
    left_join(readme_data2, by = c("Name", "date_run")) %>% 
    mutate(REMOVE = case_when(Action == "Omit_cations" & Ion %in% c("lithium", "sodium", "ammonium",
                                                                    "potassium", "magnesium", "calcium") ~ TRUE,
                              Action == "Omit_anions" & Ion %in% c("chloride", "bromide", "sulfate", "phosphate", "fluoride") ~ TRUE,
                              Action == "Omit_UV" & Ion %in% c("nitrite", "nitrate") ~ TRUE,
                              Action == "Omit" ~ TRUE
    ),
    Action_original = Action,
    Action = case_when(str_detect(Action_original, "correct") ~ "blank correct")
    ) %>% 
    group_by(date_run) %>%
    mutate(Action = case_when(Action == "blank correct" ~ "blank correct")) %>%
    fill(Action, .direction = c("updown")) %>%
    #  filter(!Action %in% "Omit") %>%
    filter(is.na(REMOVE), Name != "Name") %>% 
    dplyr::select(-REMOVE, -Action_original)
  
  data_new_processed_readme
  # This file has all the processed data for all the samples/standards/blanks run on the machine.
  # Some samples were run multiple times, at varying dilutions
  # We will use the Readme file to map these dilutions later, 
  # and then pick only the dilutions we want.
  # --> see the `do_corrections()` function
  
}

# Now, run the function
# set ions of interest
all_ions = c("Lithium", "Sodium", "Ammonium", "Potassium", "Magnesium", "Calcium", "Nitrite", "Nitrate",
             "Chloride", "Bromide", "Sulfate", "Phosphate", "Fluoride")

data_ions_processed = process_data(raw_data, readme_data, IONS = all_ions)

#

# 4. Apply QC flags ------------------------------------------------------------

# Steps are: Calculate LODs, Apply Flags, do dilution/blank corrections 

## 4a. Calculate LODs ----------------------------------------------------------

#Create Function:
calculate_lods = function(data_ions_processed, z, IONS, directory_slope){
  
  # This function will calculate the Limits of Detection (LOD) for each analyte, for each run
  
  # Definitions & Calculations:
  # 1. LOD = limit of detection 
  # calculated from Harris et al.  ("Quantitative Chemical Analysis" book) 
  # & Harvey et al. ("Modern Analytical Chemistry 2.0") 
  # 2. (SA)DL= Signal Analyte Detection Limit
  # 3. mean = average blank signal
  # 4. z = confidence interval (3 = 99%)
  # 5. sd = standard deviation of blank signals
  # 6. m = slope of calibration curve
  
  # (SA)DL = mean + sd
  # [LOD] = (SA)DL / m 
  
  # get blanks from `data_ions_processed`
  blanks = 
    data_ions_processed %>% 
    filter(grepl("blank", Name, ignore.case = TRUE)) %>% 
    filter(!grepl("CondBlank", Name)) %>% #Remove conditioning blanks 
    filter(!Name %in% c("Blank1", "Blank2", "Blank3", "Blank4")) %>% #remove carryover blanks 
    force()
  
  # calculate averages and SD for blank areas
  blanks_summary = 
    blanks %>% 
    group_by(Ion, date_run) %>% 
    dplyr::summarise(mean = mean(Area, na.rm = TRUE),
                     sd = sd(Area, na.rm = TRUE)) %>% 
    replace(is.na(.), 0) %>% 
    mutate(SADL = mean + (z * sd))
  
  # import slope of the standard curve for each ion
  import_slopes = function(directory_slope){
    
    ## a. Create a list of files to download
    files <- 
      drive_ls(directory_slope) %>% 
      filter(grepl("_Slope_", name))
    
    ## b. Download files to local (don't worry, we'll delete em in a sec)
    lapply(files$id, drive_download, overwrite = TRUE)
    
    ## c. pull a list of file names
    ## then read all files and combine
    
    filePaths <- files$name
    dat <- do.call(rbind, lapply(filePaths, function(path) {
      # then add a new column `source` to denote the file name
      df <- readxl::read_xls(path, skip = 7)
      df[["source"]] <- rep(path, nrow(df))
      df}))
    
    ## d. delete the temporary files
    file.remove(c(files$name))  
    
    ## e. output
    dat
  }
  raw_slopes = import_slopes(directory_slope)
  
  assign_slopes = function(raw_slopes, IONS){
    # identify the rows that contain ions names
    label_rows = which(grepl(paste(IONS, collapse = "|"), raw_slopes$`Peak Name`))
    
    # make this a dataframe/tibble
    label_rows_df = 
      label_rows %>% 
      as_tibble() %>%
      dplyr::rename(Row_number = value)  %>%
      mutate(label = TRUE, 
             Row_number = as.character(Row_number))
    
    # now join this to the dataframe
    slope_new <-  raw_slopes %>% 
      tibble::rownames_to_column("Row_number") %>% 
      right_join(label_rows_df) %>% 
      dplyr::select(-Row_number, -label)
    
    # preliminary processing to make it tidy
    slope_new_processed = 
      slope_new %>% 
      mutate_at(vars(-'Peak Name', -source, -Cal.Type, -Points, -Offset, -Slope, -Curve, -Coeff.Det., -Eval.Type), as.numeric) %>% 
      mutate(Date_Run.1 = str_extract(source, "[0-9]{8}"),
             date_run = lubridate::as_date(Date_Run.1)) %>% 
      dplyr::select('Peak Name', Slope, date_run) %>% 
      force()
    
    slope_new_processed
    
  }
  
  #output from slope file needs to be Ion, Slope, Date
  m.run = assign_slopes(raw_slopes,IONS = all_ions) %>% 
    rename(Ion = `Peak Name`) %>% 
    mutate(Ion = tolower(Ion),
           Ion = str_remove(Ion, "_uv"))
  
  #LOD.run = SADL_cal / m.run 
  
  data_m_sadl <- 
    m.run %>% 
    left_join(blanks_summary, by = c("Ion","date_run")) %>% 
    filter(!is.na(Slope)) %>% 
    mutate(LOD_ppm = SADL / as.numeric(Slope)) %>% 
    dplyr::select(Ion, date_run, LOD_ppm)
  
  data_m_sadl
  #save(data_m_sadl, file="LODs_byrun_COMPASS.rda")
  
  
}

#Calculate LODs: 
ions_lods = calculate_lods(data_ions_processed, 
                           z = 3, 
                           IONS = all_ions,
                           directory_slope = "https://drive.google.com/drive/u/1/folders/1pf5oxzg15uB0twTl76sEGD0Ayan8ckBy")


## 4b. `apply_qc_flags`: applying QC flags -------------------------------------
## apply flags to data points below the Limit of Detection and above Calibration


#Create Function: 
apply_qc_flags = function(data_ions_processed, QC_DATA){
  # we will apply two flags before blank corrections: (1) LOD, (2) above cal-curve
  
  data_ions_standards = 
    data_ions_processed %>% 
    filter(grepl("A-", Name) | grepl("C-", Name)) %>% 
    filter(!grepl("CK", Name)) %>%
    filter(!is.na(Amount)) %>% #getting rid of NAs in the curves, because they aren't used for the curve calculations 
    group_by(Ion, date_run, Action) %>% 
    dplyr::summarise(calib_min = min(Amount),
                     calib_max = max(Amount))
  data_ions_qc = 
    data_ions_processed %>% 
    left_join(QC_DATA %>% dplyr::select(Ion, date_run, LOD_ppm)) %>%
    left_join(data_ions_standards) %>% 
    mutate(flag = case_when(Amount  < LOD_ppm ~ "below limit of detection",
                            Amount  > calib_max ~ "above calibration",
                            TRUE ~ flag)) %>% 
    rename(ppm = Amount) %>% 
    dplyr::select(Name, date_run, Ion, ppm, flag, Action, Dilution)
  
}

#Run Function: 
data_ions_qc = apply_qc_flags(data_ions_processed, QC_DATA = ions_lods)

#1) group by sample, analyte, flag ; count of rows 
#2) look at how many of the repeats were flagged or not
#3) of the non-flagged repeats; examine this data frame to look at the dilution amount and the comparability across dilutions to determine best dilution to use



# kp testing for calibration ranges/flags in data ----
# 2024-01-22

  
disentangle_flags = function(){
  
  data_ions_qc_samples =
    data_ions_qc %>% 
    filter(grepl("EC1", Name)) %>% 
    arrange(Name, Ion, date_run) %>% 
    mutate(flag = case_when(is.na(flag) ~ "no flag", TRUE ~ flag))
  
  plot_flags_by_ion = function(){
    # plotting sample name ~ concentration, so we can see how many times each sample was run (which dilutions), and if they were flagged
    
    data_ions_qc_samples %>% 
      filter(Ion == "chloride") %>% 
      ggplot(aes(y = Name, x = ppm, color = as.character(Dilution), shape = flag))+
      geom_point(size = 4)+
      scale_shape_manual(values = c(21, 22, 18))+
      labs(title = "chloride")
    
    data_ions_qc_samples %>% 
      filter(Ion == "sodium") %>% 
      ggplot(aes(y = Name, x = ppm, color = as.character(Dilution), shape = flag))+
      geom_point(size = 4)+
      scale_shape_manual(values = c(21, 22, 18))+
      labs(title = "sodium")
    
    data_ions_qc_samples %>% 
      filter(Ion == "calcium") %>% 
      ggplot(aes(y = Name, x = ppm, color = as.character(Dilution), shape = flag))+
      geom_point(size = 4)+
      scale_shape_manual(values = c(21, 22, 18))+
      labs(title = "calcium")
    
    data_ions_qc_samples %>% 
      filter(Ion == "sulfate") %>% 
      ggplot(aes(y = Name, x = ppm, color = as.character(Dilution), shape = flag))+
      geom_point(size = 4)+
      scale_shape_manual(values = c(21, 22, 18))+
      labs(title = "sulfate")
    
    data_ions_qc_samples %>% 
      filter(Ion == "phosphate") %>% 
      ggplot(aes(y = Name, x = ppm, color = as.character(Dilution), shape = flag))+
      geom_point(size = 4)+
      scale_shape_manual(values = c(21, 22, 18))+
      labs(title = "phosphate")
    
    data_ions_qc_samples %>% 
      filter(Ion == "nitrate") %>% 
      ggplot(aes(y = Name, x = ppm, color = as.character(Dilution), shape = flag))+
      geom_point(size = 4)+
      scale_shape_manual(values = c(21, 22, 18))+
      labs(title = "nitrate")
    
  }
  
  
  # 1. identify samples+ions that had no flags
  # in case of multiple dilutions, keep the lowest dilution
  # in case of multiple dates, keep the most recent date
  # NO NEED TO RE-RUN ANY OF THESE SAMPLES
  
  no_flag = 
    data_ions_qc_samples %>% 
    filter(flag == "no flag")
  
  lowest_dilution = 
    no_flag %>% 
    arrange(Name, Ion) %>% 
    group_by(Name, Ion) %>% 
    dplyr::mutate(LOWEST_DILUTION = Dilution == min(Dilution)) %>% 
    filter(LOWEST_DILUTION) %>% 
    group_by(Name, Ion) %>% 
    dplyr::mutate(LATEST_DATE = date_run == max(date_run)) %>% 
    filter(LATEST_DATE) 
  
  # 2. identify samples+ions that had flags across all dilutions
  # SOME OF THESE WILL NEED TO BE RE-RUN
  # most of the flags were "below detection"
  # where a 1x dilution was "below detection", we can't re-run - report as below detection
  # where higher dilutions were "below detection", we may want to re-run at 1x dilution

  all_flags = 
    data_ions_qc_samples %>% 
    arrange(Name, Ion) %>% 
    filter(flag != "no flag") %>% 
    left_join(lowest_dilution %>% dplyr::select(Name, Ion) %>% mutate(NO_FLAG = TRUE)) %>% 
    filter(is.na(NO_FLAG))
  
  all_flags_wide = 
    all_flags %>% 
    mutate(Dilution = as.numeric(Dilution),
           label = paste(ppm, flag, date_run)) %>% 
    arrange(Dilution) %>% 
    ungroup() %>% 
    dplyr::select(Name, Ion, Dilution, label) %>%
    group_by(Name, Ion, Dilution) %>% 
    dplyr::summarise(label = str_c(label, collapse = "/ ")) %>% 
    mutate(Dilution = as.numeric(Dilution)) %>% 
    arrange(Dilution) %>% 
    pivot_wider(names_from = "Dilution", values_from = "label") %>% 
    arrange(Name, Ion)

  all_flags_wide %>% write.csv("EC1_water_ions_all_flags.csv", row.names = F, na = "")
  
  
  needs_rerun_at_no_dilution = 
    all_flags_wide %>% 
    filter(is.na(`1`)) %>% 
    ungroup() %>% 
    distinct(Name)
  
  # making sure we have a full sample list
  sample_list = 
    no_flag %>% 
    bind_rows(all_flags) %>% 
    ungroup %>% 
    distinct(Name)
  
  }