## graphs for alli's presentation
## kfp 2023-03-09

library(tidyverse)
library(googlesheets4)
library(ggrepel)

# setup ----
# import metadata
metadata <- read.csv("EC1_Metadata_KitLevel.csv") %>% dplyr::select(kit_id, site_name, region)

# make kit labels
label <- 
  tribble(
    ~Kit_ID, ~site,
    "K034", "King's Creek",
    "K035", "Horn Point",
    "K036", "Calfpasture Cove"
  )


#
# pH ----
ph_data = read_sheet("1hNnMjyM8zelUqI-AwLeG_lrGGgLyTfztaLp1btzqxZ0") %>% mutate_all(as.character)

ph_data2 = 
  ph_data %>% 
  mutate(pH = as.numeric(pH),
         Conductivity_uS_cm = as.numeric(Conductivity_uS_cm),
         Transect_location = tolower(Transect_location)) %>% 
  left_join(label) %>% 
  left_join(metadata, by = c("Kit_ID" = "kit_id")) %>% 
  mutate(Transect_location = factor(Transect_location, levels = c("upland", "transition", "wetland")))

# red dots for the three sites
ph_data2 %>% 
  ggplot(aes(x = Transect_location, y = pH))+
  geom_jitter(width = 0.2)+
  geom_point(data = ph_data2 %>% filter(Kit_ID %in% c("K034", "K035", "K036")),
             color = "red", size = 2)+
  facet_wrap(~region)

# red dots with labels
## pH
ph_data2 %>% 
  ggplot(aes(x = Transect_location, y = pH, label = site, shape = region))+
  geom_jitter(width = 0.2, size = 3, stroke = 1)+
  geom_point(data = ph_data2 %>% filter(Kit_ID %in% c("K034", "K035", "K036")),
             color = "red", size = 3, stroke = 1)+
  geom_text_repel(
    min.segment.length = 0,
    color = "red",
    point.padding = unit(0.5, "lines"),
    box.padding = unit(1, "lines"),
    segment.size = 1,
    show.legend = FALSE
  )+
  scale_shape_manual(values = c(16, 1))
facet_wrap(~region)

## Conductivity
ph_data2 %>% 
  ggplot(aes(x = Transect_location, y = Conductivity_uS_cm, label = site, shape = region))+
  geom_jitter(width = 0.2, size = 3, stroke = 1)+
  geom_point(data = ph_data2 %>% filter(Kit_ID %in% c("K034", "K035", "K036")),
             color = "red", size = 3, stroke = 1)+
  geom_text_repel(
    min.segment.length = 0,
    color = "red",
    point.padding = unit(0.5, "lines"),
    box.padding = unit(1, "lines"),
    segment.size = 1,
    show.legend = FALSE
  )+
  scale_shape_manual(values = c(16, 1))
facet_wrap(~region)


#
# ions ----

ions_data = read.csv("EC1_Water_Ions_L0B_TEMP_WITH_dilutions_03092023.csv")

ions_long = 
  ions_data %>% 
  dplyr::select(kit_id, transect_location, 
                nitrate_ppm, sodium_ppm, potassium_ppm, magnesium_ppm, calcium_ppm, chloride_ppm) %>% 
  pivot_longer(-c(kit_id, transect_location), names_to = "ion", values_to = "ppm") %>% 
  left_join(metadata)

ions_long %>% 
  ggplot(aes(x = region, y = ppm))+
  geom_jitter(width = 0.1)+
  geom_point(data = ions_long %>% filter(kit_id %in% c("K034", "K035", "K036")),
             color = "red", size = 2, stroke = 1)+
  facet_wrap(~ion, scales = "free_y")+
  labs(title = "ions in water")
