library(readr)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(rnaturalearth)
library(rnaturalearthdata)

###################################
data <- read_tsv('data/res.tsv') %>% 
  mutate(sound_class=paste(voicing, sound_class),
         word_initial=as.factor(word_initial),
         utt_initial=as.factor(utt_initial))

lang_vec <- unique(data$Language)
languages <- read_csv('languages.csv')

###################################
###     Preprocessing numbers   ###
###################################
count_speaker <- data %>% group_by(Language) %>% 
  summarise("speakers"=n_distinct(Speaker))


################################################
#####             Maps                     #####
################################################
languages$Longitude<-sapply(languages$Longitude,function(x) ifelse(x<(-25),x + 360,x))
world <- map_data('world', interior=F, wrap=c(-25,335), ylim=c(-54,79))

map_doreco <- ggplot()  +
  geom_polygon(data=world,
               aes(x=long,y=lat,group=group),
               colour="#F2DDC1",linewidth=0.2, fill="#F2DDC1") + 
  geom_point(data=languages,
             aes(Longitude, Latitude),
             size=3, shape=21, fill="black", alpha=0.8) +
  geom_label_repel(box.padding=0.5, point.padding=0.5,
                   data=languages, aes(Longitude, Latitude, label=Language), 
                   min.segment.length=unit(0, 'lines'),
                   size=3.5, max.overlaps=99) +
  scale_x_continuous(name="") + scale_y_continuous(name="") 

ggsave('images/map_doreco.png', map_doreco, scale=1,
       width=3000, height=2000, units="px")

write_bib(
  x=.packages(),
  file="R-packages.bib",
  tweak=TRUE,
  width=NULL,
  prefix=getOption("knitr.bib.prefix", "R-"),
  lib.loc=NULL)
