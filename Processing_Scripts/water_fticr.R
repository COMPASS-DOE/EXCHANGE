## EXCHANGE-FTICR
##
## This is a data processing script for EXCHANGE, a sub-project of the DOE-funded 
## COMPASS project (https://compass.pnnl.gov/). 
##
## This script imports and processes FTICR data obtained from Formularity 
## 
## Created: November 2022
## Kaizad F. Patel

## These functions have been modified from the `fticrrr` package and workflow: 
## https://github.com/kaizadp/fticrrr

################################################## #

# The functions for this script can be found in `Processing_Scripts/fticrrr-functions`
# See function description for more details

##############################
##############################


# 0. load packages --------------------------------------------------------
library(tidyverse)


# 1. SET input file paths -------------------------------
REPORT = "Data/fticr/fticr_surface_water.csv"

# 2. source the functions --------------------------------------------------------
source("Processing_Scripts/fticrrr-functions/a-fticrrr-functions-initial-processing.R")

report = read.csv(REPORT)

fticr_meta = make_fticr_meta(report)$meta2
fticr_data = make_fticr_data_intensities(report)$data_samples_blank_corrected
fticr_blanks = make_fticr_data_intensities(report)$data_blanks

#

# 3.  Export processed data -----------------------------------------------
fticr_data %>% write.csv("Data/Processed/EC1_Water_FTICR_L2_20221118.csv", row.names = FALSE)
fticr_meta %>% write.csv("Data/Processed/EC1_Water_FTICR_meta_L2_20221118.csv", row.names = FALSE)

