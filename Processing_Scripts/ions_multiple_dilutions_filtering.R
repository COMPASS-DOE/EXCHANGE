
#Step 1 & 2: Read in readmes & select Name, Dilution, Run Date


read_mes <- function(readme){
  # First, scrape date from filename
  date <- str_extract(readme, "[0-9]{8}")
  # Second, read in Read Me
  readxl::read_excel(path = readme, sheet = 1) %>% 
    rename(sample_name = `Sample Name`,
           sample_vol = `Sample wt`,
           total_vol = `Total vol:`) %>% 
    select(sample_name, Action, sample_vol, total_vol) %>% 
    mutate(date = date)
}


readmes_all<- ReadMes %>% 
  map_df(read_mes) %>% 
  filter(grepl("TMP", sample_name)) %>% # filter to TMP samples only
  bind_rows() 

#Step 2 Combine by Kit Id, with date and dilution PER unique date run (e.g. multiple date columns per kit)