
pacman::p_load(tidyr, dplyr, ggplot2, readr)

read_csv("~/Downloads/ec1_igsn - Samples.csv", skip = 1) -> igsn
read_csv("~/Downloads/ec1_metadata_kitlevel.csv") -> m_kit

read_csv("~/Downloads/Kit Tracking Sheet - 2. For Shipment.csv") %>% 
  select("Requestor Name", "Kit Numbers") %>% 
  separate_rows("Kit Numbers", sep = ", ") %>% 
  separate_rows("Kit Numbers", sep = "; ") %>% 
  rename(kit_id = "Kit Numbers") -> collectors

read_csv("~/Downloads/igbp.csv") -> igbp
read_csv("~/Downloads/ec1_metadata_collectionlevel.csv") %>% 
  select(kit_id , upland_ecosystem_code_IGBP, water_systemtype, 
         contains("latitude"), contains("longitude")) %>% 
  pivot_longer(-c("kit_id", "upland_ecosystem_code_IGBP", "water_systemtype"), 
               names_to = c("transect_location", ".value"), names_sep = "_") %>% 
  left_join(igbp, by = c("upland_ecosystem_code_IGBP" = "Code")) %>% 
  rename(Biome = Name) %>% 
  select(-upland_ecosystem_code_IGBP) %>% 
  mutate(water_systemtype = ifelse(!transect_location %in% c("water", "sediment"), NA, water_systemtype)) -> m_coll

m_coll %>% 
  filter(transect_location %in% c("water", "sediment")) %>% 
  select(kit_id, transect_location, water_systemtype) -> water_types

ec1_kits <- unique(m_kit$kit_id)

m_kit %>% 
  rename('Locality Description' = region, 'State/Province' = state,
         'collection Start Date' = collection_date, 'Locality' = site_name) %>% 
  select(kit_id, 'Locality Description', 'State/Province', 
         'collection Start Date', 'Locality') -> kit_small

igsn %>% 
  separate('Sample Name', 
           into = c("campaign", "kit_id", "transect_location"), 
           remove = FALSE) %>% 
  filter(kit_id %in% ec1_kits) %>% 
  mutate(transect_location = tolower(transect_location)) -> igsn_2

igsn_2 %>% 
  left_join(kit_small, by = "kit_id") %>% 
  left_join(m_coll, by = c("kit_id", "transect_location")) %>% 
  select(-c("Locality Description.x", "State/Province.x", "Locality.x", "Latitude Start", 
            "Longitude Start", "Biome.x", "collection Start Date.x", "campaign", "kit_id", "transect_location")) %>% 
  unite("z", c("Location Description", "water_systemtype"), remove = TRUE, na.rm = TRUE) %>% 
  rename("Locality Description" = "Locality Description.y", "State/Province" = "State/Province.y",
         "Locality" = "Locality.y", "Latitude Start" = "latitude", "Longitude Start" = "longitude",
         "Biome" = "Biome.y", "collection Start Date" = "collection Start Date.y") -> test

