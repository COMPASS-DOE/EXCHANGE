library(tidyverse)
library(googlesheets4)

inventory <- read_sheet("1Swr2Qx-mBXrhRPIpGf-YehSEC6zOwTPKB3u2K9s2Z7A", sheet = "Richland_Inventory_EXTERNAL")
metadata <- read.csv("EC1_Metadata_KitLevel.csv") 

kit_names <- metadata %>% 
  dplyr::select(kit_id, site_name, state, region)

inventory_new <- 
  inventory %>% 
  separate(Sample, sep = "_", into = c("kit_id", "transect"), remove = FALSE) %>% 
  left_join(kit_names) %>% 
  dplyr::select(Sample, kit_id, region, site_name, state, transect, sample_state, ground_or_unground, wt_sample_g, updated_date, updated_by)

inventory_new %>% write.csv("EC1_richland_inventory_20230719.csv", row.names = F)
