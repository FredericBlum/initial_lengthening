library(tidyverse)
library(ggrepel)
library(rnaturalearth)
library(rnaturalearthdata)
library(viridis)
library(knitr)
library(xtable)
source("cl_helper_functions.R")

###################################
data <- load_data('./utils/consonant_data.csv', filter_long = T, sample = T, n = 30, 
                  outlier_mad = 3, distance_pause = -3)

lang_vec <- unique(data$Language)
languages <- read_csv('./utils/languages.csv') %>% 
  mutate(Name = str_replace(Name, "English", "Southern English")) %>% 
  filter(Name %in% lang_vec)

###################################
###     Preprocessing numbers   ###
###################################
count_speaker <- data %>% group_by(Language) %>% 
  summarise("speakers" = n_distinct(speaker))

count_pos <- data %>% group_by(Language, Initial) %>% count() %>% 
  pivot_wider(names_from = Initial, values_from = n) %>% ungroup()

count_table <- count_pos %>% 
  mutate(total = NonInitial + UttInitial + WordInitial) %>% 
  left_join(count_speaker, by = "Language") %>%
  xtable(caption = "Number of speakers and data per position in each language",
         label = "table: data")
print(xtable(count_table), include.rownames = FALSE)

################################################
#####             Maps                     #####
################################################
sf_oceans <- ne_download(type = "ocean", category = "physical", return = "sf")

map_doreco <- ggplot()  +
  geom_sf(data = sf_oceans) +
  geom_point(data = languages,
             aes(Longitude, Latitude),
             size = 4, shape = 21, fill = "black", alpha = 0.8) +
  geom_label_repel(box.padding = 0.5, point.padding = 0.5,
                   data = languages, aes(Longitude, Latitude, label = Name), 
                   min.segment.length = unit(0, 'lines'),
                   size = 4, max.overlaps = 99) +
  scale_x_continuous(name = "") + scale_y_continuous(name = "") + 
  coord_sf(ylim = c(-60, 90), xlim = c(-170, 170))

ggsave('images/map_doreco.png', map_doreco, scale = 1,
       width = 3000, height = 2000, units = "px")

write_bib(
  x = .packages(),
  file = "R-packages.bib",
  tweak = TRUE,
  width = NULL,
  prefix = getOption("knitr.bib.prefix", "R-"),
  lib.loc = NULL)
