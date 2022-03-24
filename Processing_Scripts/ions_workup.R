## Quick n dirty workup for ions data for EC1 workshop

require(pacman)
p_load(tidyverse, readxl, stringr)

assign_ions = function(FILEPATH, PATTERN, IONS){
  
  # a. read and combine files ----
  
  # pull a list of file names in the target folder with the target pattern
  # then read all files and combine
  filePaths <- list.files(path = FILEPATH, pattern = PATTERN, full.names = TRUE)
  # filePaths <- list.files(path = FILEPATH, pattern = c("Anion", ".xls"), full.names = TRUE)
  
  dat <- do.call(rbind, lapply(filePaths, function(path) {
    # then add a new column `source` to denote the file name
    df <- readxl::read_xls(path, skip = 2)
    df[["source"]] <- rep(path, nrow(df))
    df}))
  
  # b. start processing the ions data ----
  
  # identify the rows that contain ions names
  label_rows = which(grepl(paste(IONS, collapse = "|"), dat$Time))
  
  # make this a dataframe/tibble
  label_rows_df = 
    label_rows %>% 
    as_tibble() %>%
    rename(Row_number = value) %>% 
    mutate(label = TRUE, 
           Row_number = as.character(Row_number))
  
  # now join this to the dataframe
  data_new = 
    dat %>% 
    rownames_to_column("Row_number") %>% 
    left_join(label_rows_df) %>% 
    mutate(Ion = case_when(label ~ Amount)) %>% 
    # ^ this pulls the Ion name only for certain rows
    # use fill() to down-fill the values
    # it will down-fill until it hits the next non-empty cell
    # therefore, make sure to include ALL ion names in the IONS parameter
    fill(Ion) %>% 
    dplyr::select(-Row_number, -label)
  
  # create header by collapsing the header + first row
  new_header = data_new %>% 
    colnames() %>% 
    paste0(data_new[1,]) %>% 
    str_remove_all("NA")
  # the "source" column has "source" with a lot more crap
  # use grepl to replace that full value with just "source"
  new_header = replace(new_header, grep("source", new_header), "source")
  
  # set column names for the dataframe
  names(data_new) <- new_header  
  
  # preliminary processing to make it tidy
  data_new_processed = 
    data_new %>% 
    filter(!is.na(`No.`)) %>% 
    mutate_at(vars(-Name, -Ion, -source), as.numeric) %>% 
    mutate(date_run = str_extract(source, "[0-9]{8}"),
           date_run = lubridate::as_date(date_run)) %>% 
    dplyr::select(Name, Amount, Ion, date_run)
  
  data_new_processed
  
}
  
sites = read_csv("data/EC1_sitelocations.csv") %>% 
  rename("site_location" = `Site Location`)

anions_raw = assign_ions("/Users/regi350/OneDrive - PNNL/Documents/projects/compass/exchange/data/ions/", PATTERN = "Anion", IONS = all_ions)
cations_raw = assign_ions("/Users/regi350/OneDrive - PNNL/Documents/projects/compass/exchange/data/ions/", PATTERN = "Cation", IONS = all_ions)


anions <- anions_raw %>% filter(grepl("K[0-9]{3}", Name)) %>% 
  mutate(Kit_ID = str_remove(Name, "EC1_")) %>% 
  inner_join(., sites, by = "Kit_ID") %>% 
  drop_na()

cations <- cations_raw %>% filter(grepl("K[0-9]{3}", Name)) %>% 
  mutate(Kit_ID = str_remove(Name, "EC1_")) %>% 
  inner_join(., sites, by = "Kit_ID") %>% 
  drop_na()

theme_set(theme_bw())


ggplot(cations, aes(site_location, Amount)) + 
  geom_boxplot(outlier.alpha = 0) + 
  geom_jitter(width = 0.1, alpha = 0.8) +
  facet_wrap(~Ion, nrow = 1) + 
  grattantheme::watermark("Preliminary", fontsize = 40)
  
write_csv(anions, "data/220223_initial_anions.csv")
  
  
