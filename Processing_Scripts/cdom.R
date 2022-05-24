## CDOM data - 

## Pre-processing step for CDOM data
## populating the SampleLog file, so it can be used in the MatLab script

library(tidyverse)

#  create empty SampleLog template ----

col_names = c("index",
              "Analysis",
              "Date",
              "Sample Description (60 Character Max) - Will display on final EEM figure",
              "Sample ID",
              "Analytical Replicate No",
              "Integration Time",
              "EEM file name",
              "Absorbance File Name",
              "Absorbance Pathlength",
              "Dilution Factor *Decimal Format…e.g. 1 part sample and 1 part water = 0.50",
              "DOC (mg/L)")

samplelog_empty = 
  as_tibble(matrix(nrow = 0, ncol = length(col_names)), 
            .name_repair = ~ col_names) %>% 
  mutate_all(as.character)

#
# extract metadata from file names ---- 

FILEPATH = "Data/cdom/EXCHANGE_Aqualog_Data"
filePaths <- list.files(path = FILEPATH, pattern = ".dat", full.names = TRUE, recursive = TRUE)
cdom_list = as_tibble(filePaths)

cdom_list_2 = 
  cdom_list %>% 
  mutate(filename = fs::path_file(value)) %>% 
  mutate(Date = str_extract(value, "/[0-9]{4}-[0-9]{2}-[0-9]{2}/"),
         Date = str_remove_all(Date, "/"),
         Kit_ID = str_extract(filename, "K[0-9]{3}"),
         Integration_Time = str_extract(filename, "[0-9]s"),
         Dilution = str_extract(filename, "[0-9]xd"),
         Dilution = parse_number(Dilution),
         Absorbance = case_when(grepl("Abs", filename) ~ filename),
       #  PEM = case_when(grepl("PEM", filename) ~ filename),
         EEM = case_when(grepl("Processed Graph_ RM_IFE_NRM", filename) ~ filename)
         ) %>% 
    dplyr::select(-value)
  
cdom_list_kitnames = 
  cdom_list_2 %>% 
  filter(!is.na(Kit_ID)) %>% 
  filter(!is.na(Integration_Time)) %>% 
  filter(!is.na(Absorbance) | !is.na(EEM)) %>% 
  dplyr::select(-filename) %>% 
  pivot_longer(-c(Kit_ID, Date, Integration_Time, Dilution)) %>% 
  filter(!is.na(value)) %>% 
  pivot_wider() %>% 
  rename(`Absorbance file name` = Absorbance,
         `EEM file name` = EEM,
         `Dilution Factor *Decimal Format…e.g. 1 part sample and 1 part water = 0.50` = Dilution,
         ) %>% 
  mutate_all(as.character) 

samplelog_filled = 
  bind_rows(samplelog_empty, cdom_list_2) %>% 
  dplyr::select(Kit_ID, everything())

samplelog_filled %>% write.csv("data/CDOM_SampleLog.csv", row.names = FALSE, na = "")

## 2022-05-23 KFP NOTE: this formats the list only when file names have the `K000`, `0s`, `0xdil` format
## the other files will be done next.