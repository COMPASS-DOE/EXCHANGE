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


# 2. Import datasets -----------------------------------------------------------

## Read in L0B bulk density (BD)
bd_l0 <- read_csv("Data/Processed/L0B/EC1_Soil_BulkDensity_L0B_20220714.csv")

## Read in L0B gravimetric water content (GWC)
gwc_l0 <- read_csv("Data/Processed/L0B/EC1_Soil_GWC_L0B_20220714.csv")

## Read in L0B loss on ignition (LOI)
loi_l0 <- read_csv("Data/Processed/L0B/EC1_Soil_LOI_L0B_20220714.csv")

## Read in L0B water quality
wq_l0 <- read_csv("Data/Processed/L0B/EC1_Water_WaterQuality_L0B_20220509.csv")

## Read in L0B oxygen drawdown
o2_l0 <- read_csv("Data/Processed/L0B/EC1_SoilSedimentOxygenDrawdown_L0B_20220517.csv")

## Read in L0B ions (surface waters)
ions_l0 <- read_csv("Data/Processed/L0B/EC1_Water_Ions_L0B_20221012.csv")

## Read in L0B soil pH/conductivity
pH_l0 <- read_csv("Data/Processed/L0B/EC1_Soil_pH_L0B_20220531.csv")


# 
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


# 4. Clean up and export L2 LOI ------------------------------------------------

## Remove flagged values then remove flag column
loi_l2 <- loi_l0 %>% 
  filter(!is.na(loi_perc)) %>% 
  filter(is.na(loi_flag)) %>% 
  select(-loi_flag) 

## Split into soil and sediment
loi_l2_sed <- loi_l2 %>% filter(transect_location == "Sediment")
loi_l2_soil <- loi_l2 %>% filter(transect_location != "Sediment")

## Write out
write_csv(loi_l2_sed, "Data/Processed/L2/EC1_Sediment_LOI_L2_20220923.csv")
write_csv(loi_l2_soil, "Data/Processed/L2/EC1_Soil_LOI_L2_20220923.csv")


# 4. Clean up and export L2 water quality --------------------------------------

## Remove flagged values then remove flag column
wq_l2 <- wq_l0 %>% 
  filter(!is.na(sal_psu)) %>% 
  filter(is.na(sal_flag)) %>% 
  filter(!is.na(ph)) %>% 
  filter(is.na(ph_flag)) %>% 
  filter(!is.na(orp_mv)) %>% 
  filter(is.na(orp_flag)) %>% 
  filter(!is.na(alk_mgl_caco3)) %>% 
  filter(is.na(alk_flag)) %>% 
  select(-c(contains("flag"))) 

## Write out
write_csv(wq_l2, "Data/Processed/L2/EC1_Water_WaterQuality_L2_20221004.csv")


# 4. Clean up and export L2 water quality --------------------------------------

## Remove flagged values then remove flag column
o2_l2 <- o2_l0 %>% 
  filter(!is.na(delta_do_hr)) %>% 
  filter(is.na(delta_do_flag)) %>% 
  select(-delta_do_flag)

## Split into soil and sediment
o2_l2_sed <- o2_l2 %>% filter(transect_location == "Sediment")
o2_l2_soil <- o2_l2 %>% filter(transect_location != "Sediment")

## Write out
write_csv(o2_l2_sed, "Data/Processed/L2/EC1_Sediment_OxygenDrawdown_L2_20221004.csv")
write_csv(o2_l2_soil, "Data/Processed/L2/EC1_Soil_OxygenDrawdown_L2_20221004.csv")



# 5. Clean up and export L2 soil pH/conductivity --------------------------

## Remove flagged values then remove flag column
pH_l2 <- 
  pH_l0 %>% 
  filter(is.na(ph_flag) & is.na(specific_conductance_flag)) %>% 
  select(campaign, kit_id, transect_location, ph, specific_conductance_us_cm)

## Write out
write_csv(pH_l2, "Data/Processed/L2/EC1_Soil_pH_L2_20221012.csv")

#
# 5. Clean up and export L2 water ions ------------------------------------

make_l2_ions = function(ions_l0){
  
ions_l2 <- 
  ions_l0 %>% 
  mutate_all(as.character) %>% 
  pivot_longer(cols = -c(campaign, kit_id, transect_location, date_run)) %>% 
  separate(name, sep = "_", into = c("ion", "type")) %>% 
  pivot_wider(name)

}
