####### Calculating NPOC & TN LOD Script####### 
#created by Sammi Grieger on Oct 17, 2022, edited by Opal Otenburg Mar 2, 2023
#based on Alan Roebuck's LOD excel calculations given to Sammi 

##To Do before running this code:
#1. Run a regular NPOC & TN standard curve with at least 30 blanks on TOC-L (after column has been conditioned)
#2. Export the detailed data raw file from that run
#3. Convert data raw txt file to a csv and add three columns to file that have calibration curve slope, intercept, and R2 from instrument input on the first line of each standard curve).
#4. Run this code and calculate LOD! 

####### Set Up Library ####### 
library(lubridate)
library(tidyr)
library(tidyverse)
library(dplyr)


####### Set Up Working directory ####### 
#change this working directory as needed for your device
setwd("~/NPOCTN lod") ##change this as needed for your device


####### Read in LOD data ####### 
data_raw <- read.csv("20211119_LOD.csv") #change file name to match most recent run

###### filling in slopes #####
data_raw_1 <- data_raw %>% 
  fill(Slope, Intercept, R2, .direction = c("down")) %>% #filling in slope, int, R2 for all rows 
  filter(str_detect(Sample.Name, 'Blank')) %>% #filtering for blanks
  filter(!str_detect(Sample.Name, 'Cond')) #removing conditioning blanks
  

####### Calculating TN LOD ######
#1. Prepare Data frame for TN calculations
data_raw_TN <- data_raw_1 %>%  
  filter(Analysis.Inj..== "TN") %>%  #selecting only the TN injections
  filter(!str_detect(Excluded, "1")) %>% #selecting only the included instrument injections
  select(Sample.Name, Area, Slope, Intercept, R2) #selecting only needed columns

#2. Pull out slope, intercept and R2 values
slope_TN <- data_raw_TN$Slope[1]
intercept_TN <- data_raw_TN$Intercept[1]
r2_TN <- data_raw_TN$R2[1]

#2. Average the areas for each blank
TN_Area <- data_raw_TN %>% 
  group_by(Sample.Name) %>% 
  filter(str_detect(Sample.Name, 'Blank')) %>% 
  mutate_if(is.character, as.numeric) %>% #replacing all columns that are in characters to being as numberic
  na.omit('Area') 

#3. Take Average and Standard Deviation of all blank areas

tn_average <- mean(TN_Area$Area)
tn_std <- sd(TN_Area$Area)

#4. Calculation LOD for NPOC

TN_LOD <- ((tn_average + (3*tn_std)) / slope_TN )
TN_LOD

##input new TN_LOD number into the "TOC_MCRL_LOD.xlsx" sheet with date of column change or otherwise appropriate sheet
