################# IC LOD Workup Script ##################
### By Opal & Allison 
## Last Updated: May 2022

#Steps: 
# 1.	Develop a script to calculate the slope of the standard curve for each ion – slope is the signal response 
# 2.	Need to collate each blank run – need the signal response for each ion
# 3.	Need to average blank concentrations and SDs of blank concentrations 
# 4.	Need to calculate average slope of each ion 
# 5.	Calculate LOD and LOQ

# LOD = limit of detection calculated from Harris et al & Harvey et al # Need to link DOIs still #

# (SA)DL = Sreag + zσreag
#[LOD] = (SA)DL / m 
#(SA)DL= detection limit of analyte (signal)
# Sreag = average blank signal
# z = confidence interval (3 = 99%)
z <- 3
#σreag = standard deviation of blank signals

#### Set Maintenance Period ####

#Opal to make spreadsheet of maintenance periods 
#Allison to write this part of the code 

#### Calculate Blank Signal Average & Standard Deviation ####
library(tidyverse)
setuser <- "/Users/myer056/OneDrive - PNNL/"
setwd(paste0(setuser,"/Documents/GitHub/EXCHANGE/Data/LODs/"))

Blankfile <- read.csv(paste0(setuser,"/Data Generation and Files/Raw_Instrument_Data/IC-6000 MCRL/Data for LOD calculations/Blank_Output_complete_2022-05-04.csv"))
Blankfile <- Blankfile %>% mutate(Date_Run.1 = as.character(Date_Run), Date_Run = lubridate::as_date(Date_Run.1, format = "%Y%m%d")) %>% select(-Date_Run.1)

Sreag.avg <- aggregate(Area ~ Analyte + Date_Run, data = Blankfile, mean)
σreag.sd <- aggregate(Area ~ Analyte + Date_Run, data = Blankfile, sd)

colnames(Sreag.avg)[colnames(Sreag.avg) == 'Area'] <- 'Sreag'
colnames(σreag.sd)[colnames(σreag.sd) == 'Area'] <- 'σreag'

SADL_cal <- dplyr::left_join(Sreag.avg,σreag.sd, by=c("Analyte","Date_Run"))

SADL_cal <- SADL_cal %>% dplyr::mutate(SADL = Sreag + (z*σreag))

#### Bring in slope data #####

###need to assign ions - function modified for slopes from EXCHANGE (https://github.com/COMPASS-DOE/EXCHANGE/blob/kaizad/Processing_Scripts/ions.R)###
assign_ions = function(FILEPATH, PATTERN, IONS){
  
  # a. read and combine files ----
  
  # pull a list of file names in the target folder with the target pattern
  # then read all files and combine
  filePaths <- list.files(path = FILEPATH, pattern = PATTERN, full.names = TRUE)
   #filePaths <- list.files(path= "/Users/myer056/OneDrive - PNNL/Data Generation and Files/Raw_Instrument_Data/IC-6000 MCRL/Data for LOD calculations/Slopes", pattern = c("RC2"), full.names = TRUE)
   #IONS = all_ions
  
  #to troubleshoot: dplyr::bind_rows binds all rows, you can see which ones are not matching
  dat <- do.call(rbind, lapply(filePaths, function(path) {
    # then add a new column `source` to denote the file name
    df <- readxl::read_xls(path, skip = 7)
    df[["source"]] <- rep(path, nrow(df))
    df}))
  
  # b. start processing the ions data ----
  
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
  data_new <-  dat %>% 
    tibble::rownames_to_column("Row_number") %>% 
   right_join(label_rows_df) %>% 
   # mutate(Ion = case_when(label ~ Slope)) %>% 
    # ^ this pulls the Ion name only for certain rows
    # use fill() to down-fill the values
    # it will down-fill until it hits the next non-empty cell
    # therefore, make sure to include ALL ion names in the IONS parameter
   # tidyr::fill(Ion) %>% 
   dplyr::select(-Row_number, -label)
  
  # create header by collapsing the header + first row
 # new_header = data_new %>% 
 #   colnames() %>% 
 #   paste0(data_new[1,]) %>% 
 #   str_remove_all("NA")
  # the "source" column has "source" with a lot more crap
  # use grepl to replace that full value with just "source"
 # new_header = replace(new_header, grep("source", new_header), "source")
  
  # set column names for the dataframe
#  names(data_new) <- new_header  
  
  # preliminary processing to make it tidy
  data_new_processed = 
    data_new %>% 
    #filter(!is.na(`No.`)) %>% 
    mutate_at(vars(-'Peak Name', -source, -Cal.Type, -Points, -Offset, -Slope, -Curve, -Coeff.Det., -Eval.Type), as.numeric) %>% 
    mutate(Date_Run.1 = str_extract(source, "[0-9]{8}"),
           Date_Run = lubridate::as_date(Date_Run.1)) %>% 
    dplyr::select('Peak Name', Slope, Date_Run) %>% 
    force()
  
  data_new_processed
  
}

#output from slope file needs to be Analyte, Slope, Date


all_ions = c("Lithium", "Sodium", "Ammonium", "Potassium", "Magnesium", "Calcium", "Nitrite", "Nitrate",
             "Chloride", "Bromide", "Sulfate", "Phosphate", "Fluoride")

### NEED TO CHANGE FOR CAMPAIGN SPECIFIC CALCULATIONS! IF YOU WANT ALL THEN CHANGE PATTERN TO .xls

m.run = assign_ions(FILEPATH = "/Users/myer056/OneDrive - PNNL/Data Generation and Files/Raw_Instrument_Data/IC-6000 MCRL/Data for LOD calculations/Slopes", 
                                 PATTERN = "COMPASS",
                                 IONS = all_ions)

colnames(m.run)[colnames(m.run) == 'Peak Name'] <- 'Analyte'


#LOD.run = SADL_cal / m.run 

data_m_sadl <- dplyr::left_join(SADL_cal,m.run, by=c("Analyte","Date_Run"))
data_m_sadl <- data_m_sadl %>% filter(!is.na(Slope)) %>% 
  mutate(LOD.run = SADL / as.numeric(Slope))

save(data_m_sadl, file="Prelim_all_LODs_byrun_COMPASS.rda")

#Maintenance Periods# 

maintenance_dates <- readxl::read_excel(paste0(setuser,"/Documents/GitHub/EXCHANGE/Data/LODs/Maintenance_Dates_IC.xlsx"))

dates = 
  maintenance_dates %>%
  distinct(Date) %>% # remove duplicates
  add_row(Date = as.Date("2020-10-01")) %>% # add a "first date for the runs"
  #add_row(Date=as.Date("2022-02-20")) %>%
  add_row(Date = as.Date(Sys.Date())) %>% # set current date as the "final date for runs"
  arrange(Date) %>% 
  mutate(Beginning = Date,
         Ending = lead(Beginning)) %>% # create columns denoting date range
  drop_na() %>% 
  rownames_to_column("group") # assign a group to each date range, easiest to just use row numbers

data_bins = 
  data_m_sadl %>% 
  dplyr::select(Analyte, Date_Run, LOD.run)

lod_dates = 
  merge(data_bins, dates) %>% # this will merge all rows in file1 with all rows in file2
  subset(Date_Run >= Beginning & Date_Run <= Ending)  # keep only rows where dates fall within the range

lod_final.1 = aggregate(LOD.run ~ Analyte + Beginning + Ending, data=lod_dates, mean)

lod_final = lod_final.1 %>%  
  dplyr::rename(LOD_ppm = LOD.run) 

View(lod_final)

pnnl.user = "myer056"
# set working directories
# Main path is where the raw data will be stored
#For PC Users:#
#home.path = paste0("C:/Users/",pnnl.user,"/OneDrive - PNNL/Documents - Core Richland and Sequim Lab-Field Team/Data Generation and Files/")
#lod.path = paste0(home.path,"Raw_Instrument_Data/IC-6000 MCRL/Data for LOD calculations/")

#for Mac Users:#
home.path = paste0("/Users/",pnnl.user,"/OneDrive - PNNL/Documents/")
lod.path = paste0(home.path,"/GitHub/EXCHANGE/Data/LODs")

instrument.path = paste0(home.path,"/GitHub/EXCHANGE/Data/LODs/")


write.csv(lod_final, paste0(instrument.path,"ions_LODs_2020_Oct_2022_April_COMPASS_Only.csv"), row.names = FALSE)


#### Troubleshooting #####

Why.u.NA <- subset(data_m_sadl,is.na(Slope))
View(Why.u.NA)
save(Why.u.NA, file="Mystery_NAs_slope.rda")


high <- lod_dates %>% filter(LOD.run > .5)
View(high)
write.csv(high, "/Users/myer056/OneDrive - PNNL/Ion Chromatography MSL5-219/LOD_Workup/TroubleLODs_greaterthan0.5ppm.csv")

test <- ggplot()+
  geom_point(aes(x= Analyte, y= LOD.run, color=Date_Run), data=data_m_sadl)
