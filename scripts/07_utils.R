library(readr)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(rnaturalearth)
library(rnaturalearthdata)
library(xtable)

###################################
data <- read_tsv('data.tsv') %>% 
  mutate(Glottocode=Language,
        sound_class=paste(voicing, sound_class),
        initial=ifelse(
           utt_initial==1, "utterance-initial", ifelse(
             word_initial==1, "word-initial", "non-initial"
           )))

lang_vec <- unique(data$Language)
languages <- read_csv('languages.csv')

# Speaker Table has 396 entries, but only 393 contribute data
speakers <- read_csv('../doreco/cldf/speakers.csv')
data %>% group_by(Speaker) %>% summarise(n=n())

###################################
###     Preprocessing numbers   ###
###################################
spk <- tibble(ID = unique(data$Speaker))
speaker_sex <- speakers %>%
  inner_join(spk) %>% group_by(sex) %>% 
  summarise("Sex"=n())

count_speaker <- data %>% group_by(Glottocode) %>% 
  summarise("Speakers"=n_distinct(Speaker))

count_ipu <- data %>% group_by(Glottocode) %>% 
  summarise("IPU"=sum(utt_initial))

count_words <- data %>% group_by(Glottocode) %>% 
  summarise("Words"=sum(word_initial))

count_phones <- data %>% group_by(Glottocode) %>% 
  summarise("Phones"=n_distinct(ID))


data_table <- languages %>% 
  mutate(Source = paste("\\", "cite{", Source,"}", sep="")) %>% 
  left_join(count_speaker) %>% left_join(count_phones) %>% 
  left_join(count_ipu) %>% left_join(count_words) %>% 
  select(Name, Glottocode, Speakers, Phones, IPU, Words, Source) %>% 
  mutate(IPU=as.character(IPU), Words=as.character(Words)) %>% 
  arrange(Name) %>% 
  xtable(caption="Extended Data table with Glottocode, number of speakers, number of phones, and source for each dataset.",
         label="table:data")

print(xtable(data_table), include.rownames=FALSE)

################################################
#####             Random statistics         ####
################################################
data %>% 
  # Comment out for overall mean
  group_by(initial) %>%
  summarize(mean = mean(Duration))


################################################
#####             Maps                     #####
################################################
languages$Longitude<-sapply(languages$Longitude,function(x) ifelse(x<(-25),x + 360,x))
world <- map_data('world', interior=F, wrap=c(-25,335), ylim=c(-54,79))

map_doreco <- ggplot() +
  geom_polygon(data=world,
               aes(x=long,y=lat,group=group),
               colour="#F2DDC1",linewidth=0.2, fill="#F2DDC1") + 
  geom_jitter(data=languages, height=3, width=3,
             aes(Longitude, Latitude, fill=name_macro_family),
             size=6, shape=21, alpha=0.8) +
  scale_colour_viridis_d() +
  # geom_label_repel(box.padding=0.5, point.padding=0.5,
  #                  data=languages, aes(Longitude, Latitude, label=Name), 
  #                  min.segment.length=unit(0, 'lines'),
  #                  size=5, max.overlaps=99) +
  scale_x_continuous(name="") + scale_y_continuous(name="") +
  theme(legend.position="none")

ggsave('images/map_doreco.png', map_doreco, scale=1,
       width=3000, height=2000, units="px")
