## This script takes L0B data for LOI, GWC, BD, WQ, and O2 drawdown exps for 
## EXCHANGE EC1 and converts them to L2, meaning:
### 1. remove flagged data
### 2. remove outliers
### 3. split into separate files by type (e.g. soils separate from sediments)
## 
## 2022-09-23
## Peter Regier
# ########### #
# ########### #

# 1. Setup ---------------------------------------------------------------------

## Load packages
require(pacman)
p_load(tidyverse)

## Set ggplot theme
theme_set(theme_bw())


# 2. Import physicochemical datasets -------------------------------------------

## Read in L0B bulk density (BD)
bd_l0 <- read_csv("Data/Processed/L0B/EC1_Soil_BulkDensity_L0B_20220714.csv")

## Read in L0B gravimetric water content (GWC)
gwc_l0 <- read_csv("Data/Processed/L0B/EC1_Soil_GWC_L0B_20220714.csv")

## Read in L0B loss on ignition (LOI)
loi_l0 <- read_csv("Data/Processed/L0B/EC1_Soil_LOI_L0B_20220714.csv")


# 3. Clean up and export L2 Bulk Density ---------------------------------------

## Remove flagged values then remove flag column
bd_l2 <- bd_l0 %>% 
  filter(is.na(bulk_density_flag)) %>% 
  select(-bulk_density_flag)

## Write out
write_csv(bd_l2, "Data/Processed/L2/EC1_Soil_BD_L2_20220923.csv")


# 4. Clean up and export L2 GWC ------------------------------------------------

## Remove flagged values then remove flag column
gwc_l2 <- gwc_l0 %>% 
  filter(is.na(gwc_flag)) %>% 
  select(-gwc_flag)

## Split into soil and sediment
gwc_l2_sed <- gwc_l2 %>% filter(transect_location == "Sediment")
gwc_l2_soil <- gwc_l2 %>% filter(transect_location != "Sediment")

## Write out
write_csv(gwc_l2_sed, "Data/Processed/L2/EC1_Sediment_GWC_L2_20220923.csv")
write_csv(gwc_l2_soil, "Data/Processed/L2/EC1_Soil_GWC_L2_20220923.csv")


# 4. Clean up and export L2 GWC ------------------------------------------------

## Remove flagged values then remove flag column
loi_l2 <- loi_l0 %>% 
  filter(is.na(gwc_flag)) %>% 
  select(-gwc_flag)

## Split into soil and sediment
gwc_l2_sed <- gwc_l2 %>% filter(transect_location == "Sediment")
gwc_l2_soil <- gwc_l2 %>% filter(transect_location != "Sediment")

## Write out
write_csv(gwc_l2_sed, "Data/Processed/L2/EC1_Sediment_GWC_L2_20220923.csv")
write_csv(gwc_l2_soil, "Data/Processed/L2/EC1_Soil_GWC_L2_20220923.csv")

