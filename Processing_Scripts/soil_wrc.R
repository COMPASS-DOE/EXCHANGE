library(tidyverse)


wrc_filepath = "Data/wrc"


import_data = function(FILEPATH, SHEETNAME){
  
  # pull a list of file names in the target folder with the target pattern
  # then read all files and combine
  
  filePaths <- list.files(path = FILEPATH, pattern = ".xlsx", full.names = TRUE)
  
  # dat <- 
  do.call(bind_rows, lapply(filePaths, function(path){
    # then add a new column `source` to denote the file name
    df <- readxl::read_excel(path, sheet = SHEETNAME)
    #  df <- read.delim(path, skip = 2)
    df[["source"]] <- basename(path)
    df}))
  
}
wrc_fitted = import_data(FILEPATH = wrc_filepath, SHEETNAME = "Fitting-Retention Θ(pF)")
wrc_evaluation = import_data(FILEPATH = wrc_filepath, SHEETNAME = "Evaluation-Retention Θ(pF)")
wrc_measurements = import_data(FILEPATH = wrc_filepath, SHEETNAME = 2)

process_data = function(wrc_fitted, wrc_evaluation){
  wrc_fitted_processed = 
  wrc_fitted %>% 
    rename(pF = `pF [-]`,
           water_percent_vol = `Water Content [Vol%]`) %>% 
    mutate(source = str_remove(source, "[0-9]{6}_")) %>% 
    mutate(source = str_remove(source, ".xlsx")) %>% 
    separate(source, sep = "_", into = c("campaign", "kit_id", "transect_location"))
  
  wrc_evaluation_procesed = 
    wrc_evaluation %>% 
    rename(pF = `pF [-]`,
           water_percent_vol = `Water Content [Vol%]`) %>% 
    mutate(source = str_remove(source, "[0-9]{6}_")) %>% 
    mutate(source = str_remove(source, ".xlsx")) %>% 
    separate(source, sep = "_", into = c("campaign", "kit_id", "transect_location"))
  
    
  wrc_measurements_procesed = 
    wrc_measurements %>% 
    rename(datetime = `Date / Time`,
           tension_bottom_hPa = `Tension Bottom [hPa]`,
           tension_top_hPa = `Tension Top [hPa]`) %>% 
    dplyr::select(datetime, starts_with("tension"), source) %>% 
    mutate(datetime = lubridate::ymd_hms(datetime)) %>% 
    mutate(source = str_remove(source, "[0-9]{6}_")) %>% 
    mutate(source = str_remove(source, ".xlsx")) %>% 
    separate(source, sep = "_", into = c("campaign", "kit_id", "transect_location"))
}



wrc_fitted_processed %>% 
  ggplot(aes(x = pF, y = water_percent_vol, color = transect_location))+
  geom_path()+
  geom_point(data = wrc_evaluation_procesed, size = 0.7)+
  xlim(0, 8)+
  facet_wrap(~kit_id)


wrc_measurements_procesed %>% 
  ggplot(aes(x = datetime))+
  geom_path(aes(y = tension_top_hPa), color = "red")+
  geom_path(aes(y = tension_bottom_hPa), color = "blue")+
  facet_wrap(~kit_id+transect_location, scales = "free_x", ncol = 6)+
  ylim(0, 810)+
  labs(caption = "red = top, blue = bottom")
