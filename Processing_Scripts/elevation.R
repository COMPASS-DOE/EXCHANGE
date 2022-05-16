# Testing out the `elevatr` package for extracting elevation from Lat, Lons
# EXCHANGE Campaign 1 Data

# Created 2022-05-12 by Stephanie Pennington

pacman::p_load(elevatr, dplyr, readr, tidyr)

# Read in metadata
metadata <- read_csv("~/Downloads/EXCHANGE Metadata Form (Responses) - Collection Metadata (1).csv")
sites <- read_csv("~/Downloads/EC1_sitelocations.csv")

# Isolate lat/lons
metadata %>% 
  select(Kit_ID, ends_with("Latitude"), ends_with("Longitude")) %>% 
  pivot_longer(2:11,
               names_to = c("Transect_Location", ".value"),
               names_pattern = "(.+)_(.+)") %>% 
  select(Longitude, Latitude, Kit_ID, Transect_Location) %>% 
  na.omit() -> coordinates
#  rename(x = Longitude, y = Latitude) %>% 
#  select(x, y, Kit_) -> coordinates

# Must be in a data frame to be used in elevatr
locations <- as.data.frame(coordinates)

get_elev_point(locations = locations, prj = "EPSG:4326", src = "epqs") -> elev

elev_df <- as.data.frame(elev)

# Check that the points make sense (increase up the gradient)
elev_df %>% 
  left_join(sites, by = "Kit_ID") %>% 
  ggplot(aes(x = Transect_Location, y = elevation, color = `Site Location`)) +
  geom_point() + theme_minimal()

# Wide format for joining back with metadata
elev_df %>% 
  select(Kit_ID, Transect_Location, elevation) %>% 
  pivot_wider(names_from = Transect_Location, values_from = elevation,
              names_glue = "{Transect_Location}_Elevation_m") -> elev_long

metadata %>% 
  select(-ends_with("Elevation_m")) %>% 
  left_join(elev_long, by = "Kit_ID")

