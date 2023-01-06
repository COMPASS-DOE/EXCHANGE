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

# Load Data

#This is a temp file: Need to load in from google drive eventually...
ion_concs_waters <- read.csv("./Data/Processed/L0B/EC1_Water_Ions_L0B_20221202_WITH_dilutions.csv")

# First step is to convert all Ions to meq/L 

ion_charge <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1kVCTdSTEdz_4RvuHCRPL-A-8g2wRrqJTHPSUzASTmn4/edit?usp=sharing")


ion_concs_waters_charge <- cbind(ion_concs_waters,ion_charge)

#meq = mg x # valence electrons / molecular weight 

ion_meq <- ion_concs_waters_charge %>%
  mutate(nitrate_meql = nitrate_ppm * nitrate_ppm_charge / nitrate_mw,
         )
