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

# 
# 3. Clean up and export L2 Bulk Density ---------------------------------------

import_l1_bd_data = function(directory){
  
  ## a. Create a list of files to download
  files <- 
    drive_ls(directory) %>% 
    filter(grepl("bulk_density", name))
  
  ## b. Download files to local (don't worry, we'll delete em in a sec)
  lapply(files$id, drive_download, overwrite = TRUE)
  
  dat <- read.csv(files$name)
  
  ## d. delete the temporary files
  file.remove(c(files$name))  
  
  ## e. output
  dat
}
## Remove flagged values then remove flag column

L1directory = "https://drive.google.com/drive/folders/1yhukHvW4kCp6mN2jvcqmtq3XA5niKVR3"

bd_l1 = import_l1_bd_data(L1directory)

bd_l2 <- 
  bd_l1 %>% 
  filter(!is.na(bulk_density_g_cm3)) %>%
  select(-bulk_density_flag) %>% 
  arrange(kit_id)

## Write out to drive

#soil
bd_l2 %>% write.csv("./ec1_soil_bulk_density_L2.csv", row.names = FALSE)

L2directory = "https://drive.google.com/drive/u/1/folders/1M-ASGuRoKqswiKbUWylWzoAyUmMPm367"

drive_upload(media = "ec1_soil_bulk_density_L2.csv", name= "ec1_soil_bulk_density_L2.csv", path = L2directory )

file.remove("ec1_soil_bulk_density_L2.csv")


# 4. Clean up and export L2 GWC ------------------------------------------------
import_l1_gwc_data = function(directory){
  
  ## a. Create a list of files to download
  files <- 
    drive_ls(directory) %>% 
    filter(grepl("gwc", name))
  
  ## b. Download files to local (don't worry, we'll delete em in a sec)
  lapply(files$id, drive_download, overwrite = TRUE)
  
  dat <- read.csv(files$name)
  
  ## d. delete the temporary files
  file.remove(c(files$name))  
  
  ## e. output
  dat
}
## Remove flagged values then remove flag column

L1directory = "https://drive.google.com/drive/folders/1yhukHvW4kCp6mN2jvcqmtq3XA5niKVR3"
  
gwc_l1 = import_l1_gwc_data(L1directory)

gwc_l2 <- 
  gwc_l1 %>% 
  filter(!is.na(moisturecontent_perc_drywtbasis)) %>%
  select(-gwc_flag)

## Split into soil and sediment
gwc_l2_sed <- gwc_l2 %>% filter(transect_location == "sediment") %>% arrange(kit_id)
gwc_l2_soil <- gwc_l2 %>% filter(transect_location != "sediment") %>% arrange(kit_id)

## Write out to drive
gwc_l2_sed %>% write.csv("./ec1_sediment_gwc_L2.csv", row.names = FALSE)

L2directory = "https://drive.google.com/drive/u/1/folders/1M-ASGuRoKqswiKbUWylWzoAyUmMPm367"

drive_upload(media = "ec1_sediment_gwc_L2.csv", name= "ec1_sediment_gwc_L2.csv", path = L2directory )

file.remove("ec1_sediment_gwc_L2.csv")

 #soil
gwc_l2_soil %>% write.csv("./ec1_soil_gwc_L2.csv", row.names = FALSE)

L2directory = "https://drive.google.com/drive/u/1/folders/1M-ASGuRoKqswiKbUWylWzoAyUmMPm367"

drive_upload(media = "ec1_soil_gwc_L2.csv", name= "ec1_soil_gwc_L2.csv", path = L2directory )

file.remove("ec1_soil_gwc_L2.csv")

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
import_l1_wq_data = function(directory){
  
  ## a. Create a list of files to download
  files <- 
    drive_ls(directory) %>% 
    filter(grepl("waterquality", name))
  
  ## b. Download files to local (don't worry, we'll delete em in a sec)
  lapply(files$id, drive_download, overwrite = TRUE)
  
  dat <- read.csv(files$name)
  
  ## d. delete the temporary files
  file.remove(c(files$name))  
  
  ## e. output
  dat
}
## Remove flagged values then remove flag column

L1directory = "https://drive.google.com/drive/folders/1yhukHvW4kCp6mN2jvcqmtq3XA5niKVR3"

wq_l1 = import_l1_wq_data(L1directory)

#leaving all these together since they all do match. need to seperate if we have one indvidually though. We don't. 
wq_l1 %>% 
  filter(!is.na(sal_psu),
         is.na(sal_flag),
         !is.na(ph),
         is.na(ph_flag),
         !is.na(orp_mv),
         is.na(orp_flag),
         !is.na(alk_mgl_caco3),
         is.na(alk_flag)) %>% 
  select(-c(contains("flag"))) -> wq_l2

## Write out
wq_l2 %>% select(campaign, kit_id, transect_location, ph) %>% arrange(kit_id) %>% write.csv("ec1_water_pH_L2.csv", row.names = FALSE)
wq_l2 %>% select(campaign, kit_id, transect_location, sal_psu) %>% arrange(kit_id) %>% write.csv("ec1_water_salinity_L2.csv", row.names = FALSE)
wq_l2 %>% select(campaign, kit_id, transect_location, orp_mv) %>% arrange(kit_id) %>% write.csv("ec1_water_ORP_L2.csv", row.names = FALSE)
wq_l2 %>% select(campaign, kit_id, transect_location, alk_mgl_caco3) %>% arrange(kit_id) %>% write.csv("ec1_water_alkalinity_L2.csv", row.names = FALSE)

L2directory = "https://drive.google.com/drive/u/1/folders/1M-ASGuRoKqswiKbUWylWzoAyUmMPm367"

drive_upload(media = "ec1_water_pH_L2.csv", name= "ec1_water_pH_L2.csv", path = L2directory)
drive_upload(media = "ec1_water_salinity_L2.csv", name=  "ec1_water_salinity_L2.csv", path = L2directory)
drive_upload(media ="ec1_water_ORP_L2.csv", name= "ec1_water_ORP_L2.csv", path = L2directory)
drive_upload(media = "ec1_water_alkalinity_L2.csv", name= "ec1_water_alkalinity_L2.csv", path = L2directory)

file.remove(c("ec1_water_pH_L2.csv", "ec1_water_salinity_L2.csv", "ec1_water_ORP_L2.csv", "ec1_water_alkalinity_L2.csv"))


# 4. Clean up and export L2 oxygen drawdown --------------------------------------

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

# Split ph and conductivity into two data frames
soil_ph_cond_full %>% 
  select(campaign, kit_id, transect_location, ph, ph_flag) -> ph_L1

soil_ph_cond_full %>% 
  select(campaign, kit_id, transect_location, specific_conductance_us_cm, specific_conductance_flag) -> cond_L1

## Remove flagged values then remove flag column
ph_L1 %>% 
  filter(!is.na(ph),
         !grepl("below range", ph_flag),
         !grepl("above range", ph_flag)) %>% 
  select(-ph_flag) %>% 
  arrange(kit_id) -> ph_L2

cond_L1 %>% 
  filter(!is.na(specific_conductance_us_cm),
         !grepl("below range", specific_conductance_flag),
         !grepl("above range", specific_conductance_flag)) %>% 
  select(-specific_conductance_flag) %>% 
  arrange(kit_id) -> cond_L2

## Write out to drive
L2directory = "https://drive.google.com/drive/u/1/folders/1M-ASGuRoKqswiKbUWylWzoAyUmMPm367"

ph_L2 %>% write.csv("./ec1_soil_ph_L2.csv", row.names = FALSE)
cond_L2 %>% write.csv("./ec1_soil_cond_L2.csv", row.names = FALSE)

drive_upload(media = "ec1_soil_ph_L2.csv", name= "ec1_soil_ph_L2.csv", path = L2directory )
drive_upload(media = "ec1_soil_cond_L2.csv", name= "ec1_soil_cond_L2.csv", path = L2directory )

file.remove("ec1_soil_ph_L2.csv")
file.remove("ec1_soil_cond_L2.csv")

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

# Clean up and export L2 TCTN ------------------------------------

# Split tc and tn into different dataframes
tctn_full %>% 
  select(campaign, kit_id, transect_location, carbon_weight_perc, tc_flag) -> tc_full

tctn_full %>% 
  select(campaign, kit_id, transect_location, nitrogen_weight_perc, tn_flag) -> tn_full

# Remove flagged values and NAs
tc_full %>% 
  filter(!is.na(carbon_weight_perc),
         !grepl("outside range", carbon_weight_perc)) %>% 
  select(-tc_flag) -> tc_L2

tn_full %>% 
  filter(!is.na(nitrogen_weight_perc),
         !grepl("outside range", nitrogen_weight_perc)) %>% 
  select(-tn_flag) -> tn_L2

# Write out
L2directory = "https://drive.google.com/drive/folders/1M-ASGuRoKqswiKbUWylWzoAyUmMPm367"

tc_L2 %>% write.csv("ec1_soil_tc_L2.csv", row.names = FALSE)
tn_L2 %>% write.csv("ec1_soil_tn_L2.csv", row.names = FALSE)

drive_upload(media = "ec1_soil_tc_L2.csv", name= "ec1_soil_tc_L2.csv", path = L2directory)
drive_upload(media = "ec1_soil_tn_L2.csv", name= "ec1_soil_tn_L2.csv", path = L2directory)

file.remove("ec1_soil_tc_L2.csv")
file.remove("ec1_soil_tn_L2.csv")

# Clean up and export L2 TSS ------------------------------------

tss_full %>% 
  filter(!grepl("outside range", tss_flag),
         !grepl("negative filter mass", tss_flag),
         !is.na(tss_mg_L)) %>% 
  select(campaign, kit_id, transect_location, tss_mg_L) -> tss_l2

# Write out
tss_l2 %>% write.csv("ec1_water_tss_L2.csv", row.names = FALSE)

L2directory = "https://drive.google.com/drive/u/1/folders/1M-ASGuRoKqswiKbUWylWzoAyUmMPm367"

drive_upload(media = "ec1_water_tss_L2.csv", name= "ec1_water_tss_L2.csv", path = L2directory)

file.remove("ec1_water_tss_L2.csv")

# Clean up and export L2 NPOC and TDN ------------------------------------
full_npoc %>% 
  filter(!is.na(doc_mgC_L)) %>% 
  select(-doc_flag) -> npoc_l2

npoc_l2$doc_mgC_L = format(npoc_l2$doc_mgC_L, digits = 3)

full_tdn %>% 
  filter(!is.na(tdn_mgN_L)) %>% 
  select(-tdn_flag) -> tdn_l2

# Write out
npoc_l2 %>% write_csv("ec1_water_doc_L2.csv")
tdn_l2 %>% write_csv("ec1_water_tdn_L2.csv")

L2directory = "https://drive.google.com/drive/u/1/folders/1M-ASGuRoKqswiKbUWylWzoAyUmMPm367"

drive_upload(media = "ec1_water_doc_L2.csv", name= "ec1_water_doc_L2.csv", path = L2directory)
drive_upload(media = "ec1_water_tdn_L2.csv", name= "ec1_water_tdn_L2.csv", path = L2directory)

file.remove("ec1_water_doc_L2.csv")
file.remove("ec1_water_tdn_L2.csv")
