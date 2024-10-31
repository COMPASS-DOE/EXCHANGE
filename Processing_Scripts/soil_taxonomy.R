########################### #
########################### #

## EXCHANGE EC1
## SOIL TAXONOMY

# This script will identify Soil Taxonomy for soils sampled, based on Kit metadata.
# The Soil Map Unit Descriptions were manually obtained from https://casoilresource.lawr.ucdavis.edu/gmap/.
# The `aqp` and `soilDB` packages will pull Soil Order and Suborder data based on Soil Series name.

# KFP, 2022-02-20

########################### #
########################### #


# 1. load packages --------------------------------------------------------

library(tidyverse)
library(aqp)
library(soilDB)
library(googlesheets4)

# devtools::install_github("grattan/grattantheme")


# 2. import sample metadata file ------------------------------------------
soil_metadata = read_sheet("1e6CLRYQcjsxPjWJ3-zFWzuUJNMAfK37CRLNWH1r-xro", sheet = "SoilSeries")
dat = soil_metadata


# 3. create function for Soil Orders --------------------------------------
assign_soil_taxonomy = function(dat){
  
  dat_longform = 
    dat %>% 
    filter(is.na(skip)) %>% 
    dplyr::select(Kit_ID, Transect_location, starts_with("Series")) %>% 
    pivot_longer(cols = starts_with("Series"), values_to = "Series") %>% 
    drop_na()
  
  tax_from_series = 
    dat_longform %>% 
    distinct(Series) %>% 
    rowwise() %>% 
    mutate(Order = fetchOSD(Series)@site$soilorder,
           Suborder = fetchOSD(Series)@site$suborder)
  
  tax_from_notes = 
    dat %>% 
    dplyr::select(Kit_ID, Transect_location, notes_orders) %>% 
    drop_na() %>% 
    separate(notes_orders, sep = ", ", into = c("Order1", "Order2")) %>% 
    pivot_longer(cols = starts_with("Order"), values_to = "Order") %>% 
    drop_na()
  
  # meta_tax = 
  dat_longform %>% 
    left_join(tax_from_series) %>% 
    bind_rows(tax_from_notes) %>% 
    mutate(Order = str_to_title(Order))
  
}

# run the function
soil_orders = 
  soil_metadata %>% 
  do(assign_soil_taxonomy(.)) %>% 
  mutate_all(as.character) %>% 
  filter(Transect_location != "Sediment")


# 4. analyze and summarize the data ---------------------------------------

## by transect location
soil_orders_count_transect = 
  soil_orders %>% 
  group_by(Transect_location, Order) %>% 
  dplyr::summarise(n = n())

## and overall
soil_orders_count_all = 
  soil_orders %>% 
  group_by(Order) %>% 
  dplyr::summarise(n = n())


# 5. make graphs ----------------------------------------------------------

# soil orders infographic -- all soils
soil_orders_count_all %>% 
  arrange((n)) %>% 
  rownames_to_column("row") %>% 
  ggplot(aes(x = 0, y = row))+
  geom_text(aes(label = Order, size = n), 
            hjust = 0, vjust = 0, fontface = "bold",
            show.legend = FALSE)+
  geom_text(aes(label = paste("n =", n)), hjust = 0, nudge_y = -0.10, size = 4)+
  scale_size(range = c(8, 20))+
  xlim(0, 0.05)+
  theme_void()+   
  #  annotate("text", x = Inf, y = -Inf, label = "PRELIMINARY",
  #                           hjust=2.7, vjust= -2, col="red", cex=6,
  #                           fontface = "bold", alpha = 0.8) +
#  grattantheme::watermark("PRELIMINARY", fontsize = 54)+
  labs(caption = "Figure created 2022-02-20")

# barbie theme
soil_orders_count_all %>% 
  arrange((n)) %>% 
  rownames_to_column("row") %>% 
  ggplot(aes(x = 0, y = row))+
  geom_text(aes(label = Order, size = n), 
            hjust = 0, vjust = 0, fontface = "bold", color = "#a62675ff",
            show.legend = FALSE)+
  geom_text(aes(label = paste("n =", n)), hjust = 0, nudge_y = -0.10, size = 4, color = "#a62675ff")+
  scale_size(range = c(8, 20))+
  xlim(0, 0.05)+
  theme_barbie(barbie_font = F)+
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank()
        )+
#  theme_void()+   
  #  annotate("text", x = Inf, y = -Inf, label = "PRELIMINARY",
  #                           hjust=2.7, vjust= -2, col="red", cex=6,
  #                           fontface = "bold", alpha = 0.8) +
  #  grattantheme::watermark("PRELIMINARY", fontsize = 54)+
  labs(caption = "Figure created 2022-02-20")

font_add_google(name = "Sansita Swashed", family = "barbie")
showtext_auto()



# soil orders infographic -- by transect location
soil_orders_count_transect %>% 
  arrange((n)) %>% 
  left_join(soil_orders_count_all %>% 
              arrange((n)) %>% 
              rownames_to_column("row") %>% 
              dplyr::select(Order, row)) %>% 
  
  mutate(Transect_location = factor(Transect_location,
                                    levels = c("Upland", "Transition", "Wetland"))) %>% 
  
  ggplot(aes(x = 0, y = row))+
  geom_text(aes(label = Order, size = n), 
            hjust = 0, fontface = "bold",
            show.legend = FALSE)+
  geom_text(aes(label = paste("n =", n)), hjust = 0, nudge_y = -0.35, size = 4)+
  scale_size(range = c(4, 16))+
  facet_wrap(~Transect_location,
             ncol = 3, 
             strip.position = "left")+
  xlim(0, 0.13)+
  theme_void()+
  theme(strip.text = element_text(size = 24, face = "bold", color = "grey60", hjust = 0.9))+
  labs(caption = "Figure created 2022-02-20")
