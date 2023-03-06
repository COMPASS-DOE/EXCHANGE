jars = googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1Swr2Qx-mBXrhRPIpGf-YehSEC6zOwTPKB3u2K9s2Z7A/edit#gid=1593904722")

jars_clean = jars %>%
  mutate(kit_id = stringr::str_extract(Sample,"K[:digit:]{3}" ),
         transect_location = stringr::str_extract(Sample,"upland|transition|wetland|sediment" ),
         campaign = "EC1") %>%
  mutate(transect_location = stringr::str_replace(transect_location,"upland","Upland"))  %>%
  mutate(transect_location = stringr::str_replace(transect_location,"transition","Transition"))  %>% 
  mutate(transect_location = stringr::str_replace(transect_location,"wetland","Wetland")) %>%
  mutate(transect_location = stringr::str_replace(transect_location,"sediment","Sediment")) %>%
  rename(sample_weight_g = `Sample weight (from tare) (g)`) %>%
  select(campaign, kit_id, transect_location, sample_weight_g)