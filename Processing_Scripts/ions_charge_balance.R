## EXCHANGE-IONS-CHARGE-BALANCE
##
## This is a data processing script for EXCHANGE, a sub-project of the DOE-funded 
## COMPASS project (https://compass.pnnl.gov/). 
##
## This script imports cleaned QA/QC data, and calculates a charge balance for ions.
#  Loosely based off of https://rdrr.io/github/USGS-R/WQ-Review/src/R/ionBalance.R
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
## Created: 2023-01-05
## Allison N Myers-Pigg & Opal Otenburg

library(tidyverse)
library(googlesheets4)
library(googledrive)

# Load Data

#This is a temp file: Need to load in from google drive eventually...
ion_concs_waters <- read.csv("./Data/Processed/L0B/EC1_Water_Ions_L0B_20221202_WITH_dilutions.csv")

#load in water quality datasets (pH, Alk)
directory = "https://drive.google.com/drive/folders/1r3sP7tvhG2btZACvxZUdtrtSiLVGUUGp"

## Create a function to read in data
read_L0B_data <- function(data){
  # read in data
  read_csv(file = data)
}

# 3. Import data ---------------------------------------------------------------

## Create a list of files to download
files <- drive_ls(directory) %>% 
  filter(grepl("WaterQuality", name))

## Download files to local (don't worry, we'll delete em in a sec)
lapply(files$name, drive_download, overwrite = TRUE)

# 3. Import data ---------------------------------------------------------------

## Read in data, filter to EC1 samples, and add sample name
water_quality <- files$name %>% 
  map(read_L0B_data) %>% 
  bind_rows() 

## Clean up local (delete downloaded files)
file.remove(c(files$name))

# First step is to convert all Ions to meq/L 

ion_charge <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1kVCTdSTEdz_4RvuHCRPL-A-8g2wRrqJTHPSUzASTmn4/edit?usp=sharing")

ion_concs_waters_charge <- cbind(ion_concs_waters,ion_charge)
# [HCO−3] + 2[CO2−3] = H2CO3

wq_charge <- ion_charge %>%
  select(H_ppm_charge, )

#meq = mg x # valence electrons / molecular weight 

#Charge Balance = ∑(cations) = ∑(anions)

#Cation-Anion Balance Difference = (∑eq cations - ∑eq anions) / (∑eq cations + ∑eq anions) x 100%

Cation_Anion_Balance_Difference <- 
  
  #function(var){ 
 # {{var}}/sum({{var}})*100 }

ion_meq <- ion_concs_waters_charge %>% #convert everything to milliequivalents per L
  mutate(nitrate_meql = nitrate_ppm * nitrate_ppm_charge / nitrate_mw,
         nitrite_meql = nitrite_ppm * nitrite_ppm_charge / nitrite_mw,
         fluoride_meql = fluoride_ppm * fluoride_ppm_charge / fluoride_mw,
         chloride_meql = chloride_ppm * chloride_ppm_charge / chloride_mw,
         sodium_meql = sodium_ppm * sodium_ppm_charge / sodium_mw,
         bromide_meql = bromide_ppm * bromide_ppm_charge / bromide_mw,
         ammonium_meql = ammonium_ppm * ammonium_ppm_charge / ammonium_mw,
         lithium_meql = lithium_ppm * lithium_ppm_charge / lithium_mw,
         phosphate_meql = phosphate_ppm * phosphate_ppm_charge / phosphate_mw,
         potassium_meql = potassium_ppm * potassium_ppm_charge / potassium_mw,
         sulfate_meql = sulfate_ppm * sulfate_ppm_charge / sulfate_mw,
         magnesium_meql = magnesium_ppm * magnesium_ppm_charge / magnesium_mw,
         calcium_meql = calcium_ppm * calcium_ppm_charge / calcium_mw) %>%
  select(kit_id, nitrate_meql:calcium_meql)


ion_CABD <- ion_meq %>%
  select(!kit_id)%>%
  mutate(CABD = rowSums(., na.rm = TRUE))
 
# summarize(across(where(is.numeric), mean, na.rm = T))

water_quality_meq <- water_quality %>%
  select(campaign, kit_id, transect_location, ph, alk_mgl_caco3) %>%
  mutate(ph_meql = (10exp(-ph) * H_ppm_charge),
         alk_meql= )
