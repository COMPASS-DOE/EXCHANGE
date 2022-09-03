library(tidyverse)


wrc_filepath = "Data/wrc"


import_data = function(FILEPATH, SHEETNAME){
  
  # pull a list of file names in the target folder with the target pattern
  # then read all files and combine
  
  filePaths <- list.files(path = FILEPATH, pattern = ".xlsx", full.names = TRUE)
  
  # dat <- 
  do.call(rbind, lapply(filePaths, function(path){
    # then add a new column `source` to denote the file name
    df <- readxl::read_excel(path, sheet = SHEETNAME)
    #  df <- read.delim(path, skip = 2)
    df[["source"]] <- basename(path)
    df}))
  
}
wrc_fitted = import_data(FILEPATH = wrc_filepath, SHEETNAME = "Fitting-Retention Θ(pF)")

process_data = function(wrc_fitted){
  wrc_fitted_processed = 
  wrc_fitted %>% 
    rename(pF = `pF [-]`,
           water_percent_vol = `Water Content [Vol%]`) %>% 
    mutate(source = str_remove(source, "[0-9]{6}_")) %>% 
    mutate(source = str_remove(source, ".xlsx")) %>% 
    separate(source, sep = "_", into = c("campaign", "kit_id", "transect_location"))
    
}



wrc_fitted_processed %>% 
  ggplot(aes(x = pF, y = water_percent_vol, color = transect_location))+
  geom_path()+
  xlim(0, 8)+
  facet_wrap(~kit_id)
