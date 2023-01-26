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
##
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

# `import_data`: this function will import all xls files in the target directpry and combine them
# input parameters are (a) FILEPATH, the target directory with the raw data files

import_data_OLD = function(FILEPATH){
  ## THIS WAS THE OLD FUNCTION,
  ## pulling files stored locally
  ## Replaced by the function below
  ## KFP 2022-10-04
  
  # pull a list of file names in the target folder with the target pattern
  # then read all files and combine
  
  filePaths <- list.files(path = FILEPATH, pattern = ".xls", full.names = TRUE)
  
  # dat <- 
    do.call(rbind, lapply(filePaths, function(path){
      # then add a new column `source` to denote the file name
      df <- readxl::read_excel(path, skip = 2)
    #  df <- read.delim(path, skip = 2)
      df[["source"]] <- rep(path, nrow(df))
      df}))
  
}

# Now, run this function
# raw_data <- import_data(FILEPATH = "data/ions/ions_data_without_dilution_correction")

# Import the Limits of Detection (LOD)
# ions_lods = read.csv("data/LODs/ions_LODs_2020_Oct_2022_April_COMPASS_Only.csv")
## not doing this here - LOD function is below

# Import files from Google Drive

## import the raw data files
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
      #  df <- read.delim(path, skip = 2)
      df[["source"]] <- rep(path, nrow(df))
      df}))

  ## d. delete the temporary files
  file.remove(c(files$name))  
  
  ## e. output
  dat
}

raw_data = import_data(directory)
raw_data[raw_data == "n.a."] <- NA

## import the dilutions key
dilutions_key_wide = read_sheet("1ekMFJrzE_1dAzfFuLrDfrRUzp2b66WAgE4LRNZEYGy0")
run_log_withdates = read_sheet("https://docs.google.com/spreadsheets/d/1hazGv03RroD2tKmZgwE6DOmCYDRqMtMl6C7U6TLQ1go/edit?usp=sharing")

## import the readme files
directory_readme = "https://drive.google.com/drive/u/1/folders/1PAa6Gtnthn9gUw5QhiyLOSLsBN1j7fpc"

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
      #  df <- read.delim(path, skip = 2)
      df[["source"]] <- rep(path, nrow(df))
      df}))
  
  ## d. delete the temporary files
  file.remove(c(files$name))  
  
  ## e. output
  dat
}

readme_data = import_readme(directory_readme)
readme_data %>% write_csv("TEMP-ions_readme_compiled_2022-10-12.csv")

#
# 3. Process data ---------------------------------------------------------

# `process_dilutions_data`: this function will make longform and clean the dilutions map/key
# input parameters are (a) the dilutions key (wideform)
process_dilutions_data = function(dilutions_key_wide){
  dilutions_key_wide %>% 
    pivot_longer(cols = ends_with("_dilution"),
                 names_to = "Ion",
                 values_to = "dilution") %>% 
    mutate(Ion = str_remove(Ion, "_dilution"))
}

dilutions_key_nodate = process_dilutions_data(dilutions_key_wide)

run_log_long = run_log_withdates %>% pivot_longer(cols= -"kit_id", names_sep = "_", names_to = c(".value", "run")) %>% na.omit()

dilutions_key = left_join(dilutions_key_nodate, run_log_long, by = c("kit_id","dilution"))  %>%
  mutate(date_run = lubridate::ymd(rundate)) %>% select(-c(run,rundate))

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
    mutate(Ion = case_when(label ~ Amount)) %>% 
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
      ### new_header = 
      ###   data_new %>% 
      ###   colnames() %>% 
      ###   paste0(data_new[1,]) %>% 
      ###   str_remove_all("NA")
      ### # the "source" column has "source" with a lot more crap
      ### # use grepl to replace that full value with just "source"
      ### new_header = replace(new_header, grep("source", new_header), "source")
      ##
      ### # set column names for the dataframe
      ### names(data_new) <- new_header  
  
  # preliminary processing to make it tidy
  data_new_processed = 
    data_new %>% 
    filter(!is.na(`No.`)) %>% 
    mutate_at(vars(-Name, -Ion, -source), as.numeric) %>% 
    # pull the date run from the long `source` column
    mutate(date_run = str_extract(source, "[0-9]{8}"),
           date_run = lubridate::as_date(date_run)) %>% 
    dplyr::select(Name, Amount, Area, Ion, date_run) %>% 
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
    )) %>% 
    #  filter(!Action %in% "Omit") %>%
    filter(is.na(REMOVE)) %>% 
    dplyr::select(-REMOVE)
 
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



## 4a. Calculate LODs ----------------------------------------------------------

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
    mutate(Ion = tolower(Ion))
  
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

ions_lods = calculate_lods(data_ions_processed, 
                          z = 3, 
                          IONS = all_ions,
                          directory_slope = "https://drive.google.com/drive/u/1/folders/1pf5oxzg15uB0twTl76sEGD0Ayan8ckBy")


## 4b. `apply_qc_flags`: applying QC flags -------------------------------------
## apply flags to data points below the Limit of Detection and above Calibration

apply_qc_flags = function(data_ions_processed, QC_DATA){
  # we will apply two flags: (1) LOD, (2) above cal-curve
  
  data_ions_standards = 
    data_ions_processed %>% 
    filter(grepl("A-", Name) | grepl("C-", Name)) %>% 
    filter(!grepl("CK", Name)) %>%
    filter(!is.na(Amount)) %>% 
    group_by(Ion, date_run, Action, Dilution) %>% 
    dplyr::summarise(calib_min = min(Amount),
                     calib_max = max(Amount))
  data_ions_qc = 
    data_ions_processed %>% 
    left_join(QC_DATA %>% dplyr::select(Ion, date_run, LOD_ppm)) %>%
    left_join(data_ions_standards) %>% 
    mutate(flag = case_when(Amount  < LOD_ppm ~ "below detect",
                            Amount  > calib_max ~ "above calibration")) %>% 
    rename(ppm = Amount) %>% 
    dplyr::select(Name, date_run, Ion, ppm, flag, Action, Dilution) %>% 
    filter(ppm >= 0)
  
}

data_ions_qc = apply_qc_flags(data_ions_processed, QC_DATA = ions_lods)


#
# 5. Do dilution/blank corrections ----------------------------------------

# `do_corrections`: this function will apply blank and dilution corrections
# input parameters are:
#  (a) the processed_data dataframe with qc flags, 
#  (b) compiled readme file, which contains data for dilutions, etc.,
#  (c) and the dilutions key, which tells us which dilutions we want to keep for each sample/ion

do_corrections = function(data_ions_qc, dilutions_key){
  
  # 1. blank corrections ----
  samples_and_blanks = 
    data_ions_qc %>% 
    filter(grepl("EC1_", Name) | grepl("Blank", Name)) %>% 
    filter(!Name %in% c("Blank1", "Blank2", "Blank3", "Blank4")) %>% 
    filter(!grepl("CondBlank", Name)) %>% 
    # remove NA amounts
    filter(!is.na(ppm)) %>% 
    # assign sample or blank
    mutate(sample_type = case_when(grepl("Blank", Name) ~ "Blank",
                                   grepl("EC1_", Name) ~ "Sample")) 
  
  blank_mean = 
    samples_and_blanks %>% 
    filter(sample_type == "Blank") %>% 
    group_by(Ion, date_run) %>% 
    dplyr::summarise(blank_mean_ppm = mean(ppm))
    
  samples_blank_corrected = 
    samples_and_blanks %>% 
    filter(sample_type == "Sample") %>% 
    left_join(blank_mean, by = c("Ion", "date_run")) %>% 
    mutate(blank_mean_ppm = replace_na(blank_mean_ppm,0)) %>% 
    mutate(Amount_bl_corrected = ppm - blank_mean_ppm)
  
  # #
  # # 2. dilution correction ----
   options(scipen = 50)
  # isolate the ones run at the same dilution level multiple times
   samples_dilution_corrected_problem_children = 
     samples_blank_corrected %>% 
  #   # bring in the dilutions key to determine which dilutions to keep for which ion
     left_join(dilutions_key, by = c("Name" = "kit_id", "Ion" = "Ion", "Dilution" = "dilution", "date_run")) %>% 
     group_by(Name, Ion, Dilution) %>% 
     filter(n()>1, is.na(flag)) %>%
     mutate(max = max(Amount_bl_corrected),
            min = min(Amount_bl_corrected),
            mean = mean(Amount_bl_corrected),
            percent_diff = ((((max - min) / mean) * 100 )),
            check = if_else(percent_diff > 5 , "FLAG", as.character(mean)),
            keep = date_run == max(date_run),
            value = if_else(check == "FLAG" & keep == "TRUE", Amount_bl_corrected, mean )) #might need a case_when here. it seems to be pulling the wrong things when first is false and second is true
   
   samples_dilution_corrected_unique = 
     samples_blank_corrected %>% 
     #   # bring in the dilutions key to determine which dilutions to keep for which ion
     right_join(dilutions_key, by = c("Name" = "kit_id", "Ion" = "Ion", "Dilution" = "dilution", "date_run")) %>% 
     group_by(Name, Ion, Dilution) %>% 
     filter(n()==1) 
   
   
  #   dplyr::mutate(keep = date_run == max(date_run)) %>% 
  #   filter(keep) %>% 
  #   dplyr::select(-keep) %>% 
     
     
  #   # do the dilution correction
  #   mutate(Amount_bl_dil_corrected = Amount_bl_corrected * Dilution) %>% 
  #   mutate(Amount_bl_dil_corrected = as.numeric(Amount_bl_dil_corrected),
  #          Amount_bl_dil_corrected = round(Amount_bl_dil_corrected, 3)) %>% 
  #   dplyr::select(Name, date_run, Ion, Amount_bl_dil_corrected, flag, Dilution) %>% 
  #   filter(Amount_bl_dil_corrected > 0)
  # 
  # 
  # samples_dilution_corrected_ALLDILUTIONS = 
  #   samples_blank_corrected %>% 
  #   # bring in the dilutions key to determine which dilutions to keep
  #   left_join(dilutions_key, by = c("Name" = "kit_id", "Ion" = "Ion", "Dilution" = "dilution")) %>% 
  #   # do the dilution correction
  #   mutate(Amount_bl_dil_corrected = Amount_bl_corrected * Dilution) %>% 
  #   mutate(Amount_bl_dil_corrected = as.numeric(Amount_bl_dil_corrected),
  #          Amount_bl_dil_corrected = round(Amount_bl_dil_corrected, 3)) %>% 
  #   dplyr::select(Name, date_run, Ion, Amount_bl_dil_corrected, flag, Action, Dilution, keep) %>% 
  #   filter(Amount_bl_dil_corrected > 0))
  # 
  # 
  # 
  # list(samples_dilution_corrected = samples_dilution_corrected,
  #      samples_dilution_corrected_ALLDILUTIONS = samples_dilution_corrected_ALLDILUTIONS
  # )
  
}

data_ions_corrected = do_corrections(data_ions_qc, dilutions_key)

$samples_dilution_corrected
data_ions_corrected_all_dilutions = do_corrections(data_ions_qc, dilutions_key)$samples_dilution_corrected_ALLDILUTIONS

# 6. final formatting ----------------------------------------------------

# `format_df`: format the dataframe to a more legible format in wideform, with a flag column for each ion

format_df = function(data_ions_corrected){
  
  data_ions_corrected %>% 
    ungroup() %>% 
    rename(ppm = Amount_bl_dil_corrected) %>% 
    mutate(ppm = as.character(ppm),
           Dilution = as.character(Dilution)) %>% 
    pivot_longer(-c(Name, date_run, Ion)) %>% 
    mutate(name2 = paste0(Ion, "_", name)) %>% 
    dplyr::select(-Ion, -name) %>% 
    distinct %>% 
    pivot_wider(names_from = "name2", values_from = "value") %>% 
    separate(Name, sep = "_", into = c("campaign", "kit_id")) %>% 
    mutate(transect_location = "Water") %>% 
    dplyr::select(campaign, kit_id, transect_location, everything()) %>% 
    mutate(across(ends_with("_ppm"), as.numeric)) %>% 
    janitor::clean_names() %>% 
    arrange(kit_id)
  
  
}

data_ions_final = format_df(data_ions_corrected) # this includes only the results for the selected (correct) dilutions
data_ions_final_all_dilutions = format_df(data_ions_corrected_all_dilutions) # this includes results for all dilutions (including the ones we want to exclude)


#
# 5. Export cleaned data --------------------------------------------------

data_ions_final %>% write.csv("Data/Processed/L0B/EC1_Water_Ions_L0B_20221202_WITH_dilutions.csv", row.names = FALSE)
data_ions_final_all_dilutions %>% write.csv("Data/Processed/L0B/EC1_Water_Ions_L0B_20221202_WITH_ALL_dilutions.csv", row.names = FALSE)
  
#
# xx. other functions -----------------------------------------------------

check_cal_curve_values = function(){
  
  ## (side code) the lower end of cal curves is generally NA. 
  ## check if samples have values below the non-NA cal curves.
  
  # first, pull out all the standards
  data_standards = 
    data_ions_processed %>% 
    filter(grepl("A-", Name) | grepl("C-", Name)) %>% 
    filter(!grepl("CK", Name)) %>%
    filter(!is.na(Amount)) %>% 
    group_by(Ion, date_run) %>% 
    dplyr::summarise(amount_min = min(Amount))
  
  data_samples = 
    data_ions_processed %>% 
    filter(grepl("EC1_", Name))  %>% 
    dplyr::select(Name, Ion, date_run, Amount) %>% 
    left_join(data_standards) %>% 
    mutate(less_than_cal = Amount < amount_min)
}


# Plottings 
a = data_ions_final_all_dilutions %>%
  ggplot() +
  geom_point(aes(kit_id, as.numeric(nitrate_ppm)))

  ggplotly(a)
  
#QC stuff
data_ions_qc %>% write.csv("TEMP-EC1-ions-not-dilution-corrected_2022-10-12.csv", row.names = FALSE)
file.remove("TEMP-ions_readme_compiled_2022-10-12.csv")

file.remove("TEMP-EC1-ions-not-dilution-corrected_2022-10-12.csv")
