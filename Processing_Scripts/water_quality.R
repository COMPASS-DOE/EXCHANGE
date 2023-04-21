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
## Alk in mg/L CaCO3 = Alkmeq/L * 1mmolCaCO3/2meq * 100.87 mgCaCO3/1mmolCaCO3 = 
# 50044 * Volume of Acid titrant * Concentration of acid titrant (in N) * / Volume of sample in mL 
# Note that nomality of HCl ~ molarity HCl 
#per: https://pubs.usgs.gov/twri/twri9a6/twri9a66/twri9a_chapter6.6_9-2001.pdf 

volume_sample_ml = 50 
CF = 4.02

water_quality_primitive <- read_sheet(titrator_path, col_types = 'Tcddddddc') %>% 
  mutate(campaign = "EC1",
         transect_location = "Water",
         sal_psu = gsw_SP_from_C(C = spcond_mscm, t = 25, p = 0), 
         alk_mgl_caco3 = ifelse(hcl_molarity == 0.02, (ml_hcl_added * hcl_molarity * 50044) / volume_sample_ml, 
                             ifelse(hcl_molarity == 0.1, (ml_hcl_added * hcl_molarity * 50044) / volume_sample_ml / CF, NA))) %>% 
  dplyr::select(campaign, kit_id, transect_location, sal_psu, ph, orp_mv, alk_mgl_caco3)

# 3. QC data -------------------------------------------------------------------

clean_data <- function(data) {
  data %>% 
    ## First, round each parameter to proper significant figures
    mutate(sal_psu = round(sal_psu, 2), 
           ph = round(ph, 2), 
           orp_mv = round(orp_mv, 0), 
           alk_mgl_caco3 = round(alk_mgl_caco3, 0),
           transect_location = tolower(transect_location)) %>% 
    ## Second, add flags for 
    mutate(sal_flag = ifelse(sal_psu < 0 | sal_psu > 50, "outside range", NA), 
           ph_flag = ifelse(ph < 0 | ph > 14, "outside range", NA), 
           orp_flag = ifelse(orp_mv < -250 | orp_mv > 500, "outside range", NA), #https://www.ysi.com/parameters/orp-redox
           alk_flag = ifelse(alk_mgl_caco3 < 0, "outside range", NA))
}

water_quality <- clean_data(water_quality_primitive) 

# 4. Check with Metadata for missing:

source("./Processing_Scripts/Metadata_kit_list.R")

metadata_collected %>%
  filter(sample_method == "bottle_125ml")-> meta_filter

water_quality %>%
  full_join(meta_filter, by = c("campaign", "kit_id", "transect_location")) %>% 
  mutate(sal_flag = case_when(collected == TRUE & is.na(sal_psu) ~ "sample compromised",
                              collected == FALSE & is.na(sal_psu) ~ "sample not collected",
                              TRUE ~ sal_flag),
         ph_flag = case_when(collected == TRUE & is.na(ph) ~ "sample compromised",
                              collected == FALSE & is.na(ph) ~ "sample not collected",
                              TRUE ~ ph_flag),
         orp_flag = case_when(collected == TRUE & is.na(orp_mv) ~ "sample compromised",
                              collected == FALSE & is.na(orp_mv) ~ "sample not collected",
                              TRUE ~ orp_flag),
         alk_flag = case_when(collected == TRUE & is.na(alk_mgl_caco3) ~ "sample compromised",
                              collected == FALSE & is.na(alk_mgl_caco3) ~ "sample not collected",
                              TRUE ~ alk_flag)) %>% 
  mutate(sal_psu = case_when(kit_id %in% c("K001","K007") ~ NA,
                                                     TRUE ~ sal_psu),
         sal_flag = case_when(kit_id %in% c("K001","K007") ~ "kit compromised",
                              TRUE ~ sal_flag),
         ph = case_when(kit_id %in% c("K001","K007") ~ NA,
                             TRUE ~ ph),
         ph_flag = case_when(kit_id %in% c("K001","K007") ~ "kit compromised",
                              TRUE ~ ph_flag),
         orp_mv = case_when(kit_id %in% c("K001","K007") ~ NA,
                        TRUE ~ orp_mv),
         orp_flag = case_when(kit_id %in% c("K001","K007") ~ "kit compromised",
                             TRUE ~ orp_flag),
         alk_mgl_caco3 = case_when(kit_id %in% c("K001","K007") ~ NA,
                            TRUE ~ alk_mgl_caco3),
         alk_flag = case_when(kit_id %in% c("K001","K007") ~ "kit compromised",
                              TRUE ~ alk_flag)) %>%
          select(-sample_type, -collected, -sample_method) -> water_quality_full

View(water_quality_full)

# 5. Write L0B data to Google Drive ---------------------------------------------
water_quality_full %>% write.csv(paste0("./ec1_water_waterquality_L1_", Sys.Date(), ".csv"), row.names = FALSE)

L1directory = "https://drive.google.com/drive/folders/1yhukHvW4kCp6mN2jvcqmtq3XA5niKVR3"

drive_upload(media = paste0("./ec1_water_waterquality_L1_", Sys.Date(), ".csv"), name= paste0("ec1_water_waterquality_L1_", Sys.Date(), ".csv"), path = L1directory)

file.remove(paste0("./ec1_water_waterquality_L1_", Sys.Date(), ".csv"))
