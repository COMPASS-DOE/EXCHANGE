library(tidyverse)
library(googlesheets4)

texture_data = read_sheet("https://docs.google.com/spreadsheets/d/1PSKKbnmr3jVhjv172mPynmkt3mVlOH_5KQgSa7DTCIU/edit?gid=0#gid=0", col_types = "c")
texture_data2 = 
  texture_data %>% 
  mutate_at(vars(-c(sample_id, date_started, notes)), as.numeric)

compute_soil_texture = function(dat){
  
  dat %>% 
    mutate(
      
      wt_sand_g = wt_sieve_soil_53um_g - wt_sieve_g,
      wt_fines = wt_jar_soil_g - wt_half_gallon_jar_g,
      wt_dry_soil_g = wt_sand_g + wt_fines,
      
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
    dplyr::select(sample_id, starts_with("percent"), notes) %>% 
    force()
  
}


texture = compute_soil_texture(dat = texture_data2)
