library(tidyverse)


import_data = function(FILEPATH){
  
  # pull a list of file names in the target folder with the target pattern
  # then read all files and combine
  
  filePaths <- list.files(path = FILEPATH, pattern = ".csv", full.names = TRUE)
  
  # dat <- 
  do.call(bind_rows, lapply(filePaths, function(path){
    # then add a new column `source` to denote the file name
    df <- read.csv(path, header = TRUE)
    #  df <- read.delim(path, skip = 2)
    df[["source"]] <- rep(path, nrow(df))
    df}))
  
}

# Now, run this function
all_data <- import_data(FILEPATH = "Data/Processed/L0B")

all_data_processed = 
  all_data %>% 
  mutate(transect_location = tolower(transect_location)) %>% 
  dplyr::select(-ends_with("flag"),
                -starts_with("date")) %>% 
  separate(source, sep = "EC1_", into = c("source1", "analysis")) %>% 
  mutate(analysis = str_remove(analysis, "_[0-9]{8}"),
         analysis = str_remove(analysis, ".csv"),
         analysis = str_remove(analysis, "_L[0-9][A-Z]")) %>% 
  dplyr::select(-source1) %>% 
  mutate(analysis = str_remove(analysis, "Soil_"),
         analysis = str_remove(analysis, "Water_")) %>% 
#  pivot_longer(-c(campaign, kit_id, transect_location)) %>% 
  dplyr::select(campaign, kit_id, transect_location, analysis) %>% 
 # drop_na() %>% 
  mutate(transect_location = factor(transect_location, levels = c("upland", "transition", "wetland", "sediment", "water"))) %>% 
  mutate(value = "x") %>% 
  distinct() %>% 
  pivot_wider(names_from = "analysis", values_from = "value") %>% 
  arrange(kit_id, transect_location) %>% 
  filter(!is.na(kit_id))


all_data_processed %>% 
  write.csv("Data/00-EC1_Analysis_Completion_Matrix_2022-10-04.csv", row.names = FALSE, na = "")
