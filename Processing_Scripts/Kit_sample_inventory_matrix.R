# 1. Setup ---------------------------------------------------------------------
cat("Setup")

# load packages
require(pacman)
pacman::p_load(cowsay, tidyverse, googlesheets4, googledrive, purrr, stringr)

## Welcome
say("Welcome to EXCHANGE!", by = "random")

## Define analyte
var <- "sample inventory"

## URL for data
L1directory <- "https://drive.google.com/drive/folders/1yhukHvW4kCp6mN2jvcqmtq3XA5niKVR3"

# 2. Import data ---------------------------------------------------------------
cat("Importing", var, "data...")

files <-
  drive_ls(L1directory, recursive = TRUE) %>%
  filter(grepl("L1", name))

## b. Download files to local (don't worry, we'll delete em in a sec)
lapply(files$id, drive_download, overwrite = TRUE)

csvs_2 <- list.files("./", pattern = "soil|water", full.names = TRUE) |>
  set_names() |>
  map_dfr(read_delim, .id = "file")

file.remove(c(files$name))  

#csvs <- list.files("./", pattern = "soil|water", full.names = TRUE)

#data <- lapply(csvs, read_csv)

test4 <- csvs_2 %>% mutate(data_type = (stringr::str_split(file,"_",simplify=TRUE)[,3])) %>%
  mutate(data_type = case_when(data_type == "bulk" ~ "bulkdensity",
                               data_type =="ph" ~ "soilphspc",
                                TRUE ~ data_type)) %>% 
  select(data_type, campaign, kit_id, transect_location, note, contains("_flag")) %>% pivot_wider(id_cols=c(campaign, kit_id, transect_location), names_from = data_type, values_from = c(note, contains("_flag"))) %>%
  janitor::remove_empty(which = "cols") %>%
  rename(ec1_water_cdom_L2 = note_cdom,
         ec1_water_fticrms_L2 = note_fticrms,
         ec1_water_alkalinity_L2.csv = alk_flag_waterquality,
         ec1_water_doc_L2.csv = doc_flag_doc,
         ec1_water_orp_L2.csv = orp_flag_waterquality,
         ec1_water_ph_L2.csv = ph_flag_waterquality,
         ec1_salinity_L2.csv = sal_flag_waterquality,
         ec1_water_tdn_L2.csv = tdn_flag_tdn,
         ec1_water_tss_L2.csv = tss_flag_tss,
         ec1_soil_bulk_density_L2.csv = bulk_density_flag_bulkdensity,
         ec1_soil_cond_L2.csv = specific_conductance_flag_soilphspc,
         ec1_soil_ph_L2.csv = ph_flag_soilphspc,
         ec1_soil_tc_L2.csv = tc_flag_tctn,
         ec1_soil_tn_L2.csv = tn_flag_tctn,
        `ec1_soil/sediment_L2.csv` = gwc_flag_gwc) 
 
# mutate(across(all_of(starts_with("ec1"), replace_na("data available"))))
#reduce(full_join, by=c("campaign", "kit_id", "transect_location")) 
