library(tidyverse)
library(googlesheets4)

# I.  LOAD FILES ---------------------------------------------------------

# import data from Google Sheets
hydrometer_data = read_sheet("1PSKKbnmr3jVhjv172mPynmkt3mVlOH_5KQgSa7DTCIU") %>% 
  mutate_all(as.character) 

hydrometer_data_processed = 
  hydrometer_data %>% 
  mutate_at(vars(-c(sample_id, date_started, notes)), as.numeric) %>% 
  mutate(wt_dry_soil_g = case_when(!is.na(wt_half_gallon_jar_g) ~ (wt_jar_soil_g - wt_half_gallon_jar_g) + (wt_sieve_soil_53um_g - wt_sieve_g),
                                   is.na(wt_half_gallon_jar_g) ~ wt_dry_soil_postdigest_g))


#
# II. COMPUTING PERCENT SAND-SILT-CLAY -----------------------------------

## This function will use the equations provided in Gee & Bauder
## to compute % sand, clay, silt

## % sand = fraction weight of material collected on the 53 μm sieve.
## % clay = computed using 90 and 1440 minute hydrometer readings
## % silt = 100 - (% sand + % clay)

compute_soil_texture = function(dat){
  
  dat %>% 
    mutate(
      B = (30 * 0.0091) / (9.98 * (2.65 - 1)), # constant
      
      #h = 16.3 - (0.164 * R),
      h_90min = 16.3 - (0.164 * reading_90min),
      h_1440min = 16.3 - (0.164 * reading_1440min),
      
      theta_90min = 1000 * (B * h_90min)^0.5,
      theta_1440min = 1000 * (B * h_1440min)^0.5,
      
      # P = summation %
      P_90min = ((reading_90min - blank_90min)/wt_dry_soil_g) * 100, 
      P_1440min = ((reading_1440min - blank_1440min)/wt_dry_soil_g) * 100,
      
      # X = mean diameter
      X_90min = theta_90min * (90)^-0.5, 
      X_1440min = theta_1440min * (1440)^-0.5,
      
      m = (P_90min - P_1440min)/log(X_90min/X_1440min),
      
      # percent sand-silt-clay
      percent_clay = (m * log(2/X_1440min)) + P_1440min,
      percent_sand = ((wt_sieve_soil_53um_g - wt_sieve_g)/wt_dry_soil_g) * 100,
      percent_silt = 100 - (percent_sand + percent_clay)
    ) %>% 
    dplyr::select(sample_id, starts_with("percent_"))
  
}

# now use this function
soil_texture = compute_soil_texture(dat = hydrometer_data_processed)

# process/clean up the data
soil_texture_processed = 
  soil_texture %>% 
  separate(sample_id, sep = "_", into = c("kit_id", "transect")) %>% 
  mutate(transect = recode(transect, "U" = "upland", "T" = "transition", "W" = "wetland"))

# plot the soil texture triangle
library(ggtern) 

#textures<-
gg_texture = 
  ggtern(
  data = soil_texture_processed,
  aes(x = percent_sand, y = percent_clay, z = percent_silt)) +
  geom_polygon(
    data = USDA, 
    aes(x = Sand, y = Clay, z = Silt, group = Label),
    fill = NA, size = 0.3, alpha = 0.5, color = "grey30"
  )+
  geom_point(aes(color = transect),
             size = 3)+
  theme_bw()+
  theme_showarrows()+
  theme_hidetitles()+
  theme_clockwise() 
 