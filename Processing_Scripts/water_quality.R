## This script imports raw data for basic water quality measured using
## a Mettler Toledo auto-titrator at PNNL MCRL and exports clean, Level 1 QC'ed 
##data. Measurements include salinity, pH, ORP, and alkalinity.
## Data are read in from Google Sheets.
## 
## Created: 2022-01-07
## Peter Regier
##
# #############
# #############

## 2022-01-05
## Peter Regier
##
# #############
# #############

# 1. Setup ---------------------------------------------------------------------

## load packages
require(pacman)
p_load(tidyverse, 
       googlesheets4,
       googledrive, 
       janitor,
       gsw)

## URLs for data
titrator_path = "https://docs.google.com/spreadsheets/d/1lP4ft29oknaR5Xthv3Qo29EZeco6R3WkRkH-TT_x1vc/edit#gid=1069041223"


# 2. Import titrator data ------------------------------------------------------

## Import titrator data from Google Drive
## Early samples were run with 0.1 M HCl that was made up incorrectly. To 
## correct for this error, alkalinity measurements in triplicate for the
## wrong acid and new, correct acid were run, and a correction factor of 4.02 
## was determined. This is incorporated into the alk_mgl_
##
## Salinity is calculated from spcond using the GSW toolbox
## Eh is calculated from orp_mv using Eh = orp_mv + 207, per sensor manual
titrator_data <- read_sheet(titrator_path, col_types = 'Tcddddddc') %>% 
  mutate(sal_psu = gsw_SP_from_C(C = spcond_mscm, t = 25, p = 0), 
         alk_mgl_caco3 = ifelse(hcl_molarity == 0.02, (ml_hcl_added * hcl_molarity * 50000) / 50, 
                             ifelse(hcl_molarity == 0.1, (ml_hcl_added * hcl_molarity * 50000) / 50 / 4.02, NA))) %>% 
  dplyr::select(kit_id, sal_psu, ph, orp_mv, alk_mgl_caco3)

# 3. QC data -------------------------------------------------------------------

clean_data <- function(data) {
  data %>% 
    ## First, round each parameter to proper significant figures
    mutate(sal_psu = round(sal_psu, 2), 
           ph = round(ph, 2), 
           orp_mv = round(orp_mv, 0), 
           alk_mgl_caco3 = round(alk_mgl_caco3, 0)) %>% 
    ## Second, add flags for 
    mutate(f_sal_psu = sal_psu < 0 | sal_psu > 50, 
           f_ph = ph < 0 | ph > 14, 
           f_orp = orp_mv < 0 | orp_mv > 500,
           f_alk_mgl_caco3 = alk_mgl_caco3 < 0)
}

titrator <- clean_data(titrator_data) %>% 
  drop_na()


# 4. Write L0B data to Google Drive ---------------------------------------------
write_csv(titrator, "data/EC1_Water_WaterQuality_L0B.csv")



