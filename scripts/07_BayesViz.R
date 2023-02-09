library(bayesplot)
library(brms)
library(tidyverse)
library(tidybayes)
library(patchwork)
library(viridis)
library(xtable)
source("cl_helper_functions.R")


###################################################################
cl_max_sep <- readRDS(file="models/cl_max_small.rds")

rope_high=0.01
rope_low=-0.01

#########################################
###     Parameters                    ###
#########################################
hpdi_vals89 <- posterior_interval(cl_max_sep, prob=0.89) %>% 
  data.frame() %>% as_tibble(rownames="Parameter") %>% 
  rename(hpdi_89_low=X5.5., hpdi_89_high=X94.5.)

hpdi_vals99 <- posterior_interval(cl_max_sep, prob=0.997) %>% 
  data.frame() %>% as_tibble(rownames="Parameter") %>% 
  rename(hpdi_low=X0.15., hpdi_high=X99.85.)

para_vals <- posterior_summary(cl_max_sep) %>% 
  data.frame() %>% as_tibble(rownames="Parameter") %>% 
  left_join(hpdi_vals89) %>% left_join(hpdi_vals99) %>% 
  mutate(Parameter=str_replace(Parameter, "utt_initial", "utterance-initial"),
         Parameter=str_replace(Parameter, "word_initial", "word-initial"),
         Parameter=str_replace(Parameter, "b_initial", "")) 

rm(hpdi_vals89, hpdi_vals99)

fix_eff <- para_vals %>% 
  filter(Parameter == "word-initial"|Parameter == "utterance-initial")

lang_params <- para_vals %>% 
  filter(grepl("^r_Language\\[.*", Parameter)) %>% 
  mutate(Parameter=gsub("r_Language\\[(.*)(,-initial|,)(.*)]", "\\1__\\3", Parameter),
         Parameter=str_replace(Parameter, "initialutterance", "utterance"),
         Parameter=str_replace(Parameter, "initialword", "word")) %>% 
  separate(sep="__", col=Parameter, into=c("Language", "Parameter")) %>% 
  filter(Parameter != "Intercept") %>% 
  left_join(fix_eff, by="Parameter") %>% 
  transmute(
    Language=Language, Parameter=Parameter,
    Estimate=Estimate.x + Estimate.y,
     hpdi_89_high=hpdi_89_high.x + hpdi_89_high.y,
     hpdi_89_low=hpdi_89_low.x + hpdi_89_low.y,
     hpdi_high=hpdi_high.x + hpdi_high.y,
     hpdi_low=hpdi_low.x + hpdi_low.y
   ) %>% 
  mutate(outside=ifelse(hpdi_89_high < rope_low, TRUE, 
                          ifelse(hpdi_89_low > rope_high, TRUE, FALSE)),
         outside_99=ifelse(hpdi_high < rope_low, "yes", 
                             ifelse(hpdi_low > rope_high, "yes", "no")),
         Language=str_replace(Language, "\\.", " "))

langWord <- lang_params %>% filter(Parameter == "word-initial")
langUtt <- lang_params %>% filter(Parameter == "utterance-initial")

languages <- read_csv('./utils/languages.csv')

pop_level <- c("b_Initialutterance-initial", "b_Initialword-initial", 
                "b_z_logSpeechRate" , "b_z_logPhonWord",
                "b_z_logWordFormFreq", "b_ConCluster")


r_sound_class

#########################################
###   Sound Class analysis            ###
#########################################

consonant_data <- para_vals %>% filter(grepl("^r_sound_class", Parameter)) %>% 
  mutate(Parameter = str_replace(Parameter, "^r_sound_class", ""),
         Parameter = str_replace(Parameter, "\\[", ""),
         Parameter = str_replace(Parameter, "\\]", "")) %>% 
  separate(sep=",", col=Parameter, into=c("sound_class", "position")) %>% 
  mutate(position = str_replace(position, "initial", ""),
         position = str_replace(position, "-initial", "")) %>% 
  arrange(sound_class)

consonant_data %>% 
  ggplot(aes(x=Estimate, y=paste(sound_class, position))) +
  geom_linerange(aes(xmin=hpdi_low, xmax=hpdi_high)) + 
  geom_pointrange(aes(xmin=hpdi_89_low, xmax=hpdi_89_high, color=position,
                      alpha=1), size=1.2) +  
  geom_vline(xintercept=0, color="red") +
  annotate("rect", xmin=rope_low, xmax=rope_high, ymin=0, ymax=Inf, alpha=.2) +
  scale_y_discrete(name=NULL) +
  scale_color_viridis(discrete=T, end=0.7) +
  scale_x_continuous(name=NULL) +
  theme(legend.position="none")

#########################################
###   Tables: Raw values              ###
#########################################
fixed_effects <- para_vals %>% filter(Parameter %in% pop_level) %>% 
  mutate(Estimate=round(Estimate, 2),
         Parameter=str_replace(Parameter, "b_Initial", ""),
         Parameter=str_replace(Parameter, "b_z_log", ""),
         Parameter=str_replace(Parameter, "b_ConCluster", "Consonant cluster"),
         Parameter=str_replace(Parameter, "PhonesWord", "phones per word"),
         Parameter=str_replace(Parameter, "WordFormFreq", "word-form frequency"), 
         "99.7% HPDI"=paste(format(round(hpdi_low, 2), nsmall=2), "to", 
                              format(round(hpdi_high, 2), nsmall=2))) %>%
  select(Parameter, Estimate, "99.7% HPDI") %>% 
  xtable(caption="99.7% HPDI of the population-level predictors",
         label="table: fixed_effects")
print(xtable(fixed_effects), include.rownames=FALSE)

corr <- para_vals %>% filter(str_detect(para_vals$Parameter, "cor")) %>% 
  mutate(#Estimate=round(Estimate, 2),
         Parameter=str_replace(Parameter, "speaker", "Speaker"),
         "89% HPDI"=paste(format(round(hpdi_89_low, 2), nsmall=2), "to", 
                            format(round(hpdi_89_high, 2), nsmall=2))) %>% 
  select(Parameter, "89% HPDI") %>% 
  separate(sep="__", col=Parameter, into=c("Level", "Parameter", "Parameter2")) %>% 
  mutate(Level=str_replace(Level, "cor_", ""),
         Parameter=str_replace(Parameter, "Initial", ""),
         Parameter2=str_replace(Parameter2, "Initial", "")) %>% 
  pivot_wider(names_from="Parameter2", values_from="89% HPDI") 
print(xtable(corr), include.rownames=FALSE)

#########################################
###    Overall areas                  ###
#########################################
np_max <- nuts_params(cl_max_sep)
posterior_max <- as.array(cl_max_sep)

overall_areas <- mcmc_areas(cl_max_sep, regex_pars=c("^b_Init", "^b_z_", "^b_ConCluster"),
             prob=0.89, prob_outer=0.997, point_est="mean") +
  geom_hline(yintercept=0, color="red", alpha=0.5, size=0.5)+
  annotate("rect", xmin=rope_low, xmax=rope_high, ymin=0, ymax=Inf, alpha=.3) +
  scale_y_discrete(labels=c('Utterance-initial','Word-initial','Speech rate', 
                              'Phones per word', 'Word-form frequency', 'Consonant cluster')) +
  scale_x_continuous(name="Effect on log-scale") + theme_bw()

ggsave('images/viz_overall.png', overall_areas, scale=1,
       width=2000, height=1200, units="px")

#########################################
###     Language plots                ###
#########################################
word_init <- langWord %>% 
  ggplot(aes(x=Estimate, y=reorder(Language, Estimate))) +
  geom_linerange(aes(xmin=hpdi_low, xmax=hpdi_high)) + 
  geom_pointrange(aes(xmin=hpdi_89_low, xmax=hpdi_89_high, color=Parameter,
                      alpha=ifelse(outside == 1, 1, 0.5)), size=1.2) +  
  geom_vline(xintercept=0, color="red") +
  annotate("rect", xmin=rope_low, xmax=rope_high, ymin=0, ymax=Inf, alpha=.2) +
  scale_y_discrete(name=NULL) +
  scale_color_viridis(discrete=T, end=0.7) +
  scale_x_continuous(name=NULL) +
  theme(legend.position="none")

ggsave("images/viz_wordInit.png", word_init,
       width=2000, height=1400, units="px")

###################################################################
utt_init <- langUtt %>% 
  ggplot(aes(x=Estimate, y=reorder(Language, Estimate))) +
  geom_linerange(aes(xmin=hpdi_low, xmax=hpdi_high)) + 
  geom_pointrange(aes(xmin=hpdi_89_low, xmax=hpdi_89_high, color=Parameter,
                      alpha=ifelse(outside == 1, 1, 0.5)), size=1.2) +  
  geom_vline(xintercept=0, color="red") +
  annotate("rect", xmin=rope_low, xmax=rope_high, ymin=0, ymax=Inf, alpha=.2) +
  scale_y_discrete(name=NULL) +
  scale_color_viridis(discrete=T, end=0.7) +
  scale_x_continuous(name=NULL) +
  theme(legend.position="none")

ggsave("images/viz_uttInit.png", utt_init,
       width=2000, height=1400, units="px")

###################################################################
both <- c("Beja", "Bora", "Cabécar", "Daakie", "Dolgan", "Fanbyak", "Goemai",
          "Gorwaa", "Hoocak", "Kamas", "Komnzo", "Mojeño Trinitario", "Movima",
          "N||ng", "Pnar", "Sadu", "Savosavo", "Tabaq", "Teop", "Urum",
          "Vera'a", "Warlpiri", "Yucatec Maya", "Yurakaré")

only_word <- c("English", "French", "Jahai", "Jejuan", "Kakabe", "Light Warlpiri",
               "Lower Sorbian", "Nisvai", "Northern Kurdish", "Sanzhi Dargwa",
               "Svan", "Tabarasan", "Yongning Na")

only_utt <- c("Anal", "Arapaho", "Asimjeeg Datooga", "Dalabon", "Evenki",
              "Northern Alta", "Resígaro", "Sumi")

others <- c("Bainounk Gujaher", "Cashinahua", "Nafsan", "Ruuli", "Texistepec Popoluca", "Yali")


utt_initial <- lang_params %>% 
  filter(Language %in% only_utt) %>% 
  ggplot(aes(x=Parameter, y=Estimate)) +
  geom_crossbar(aes(ymin=hpdi_89_low, ymax=hpdi_89_high, fill=Parameter,
                    alpha=ifelse(outside==TRUE, 1, 0.1)), 
                  size=0.5, width=0.7, fatten=0) + 
  geom_errorbar(aes(ymin=hpdi_low, ymax=hpdi_high, width=0.5)) +
  geom_hline(yintercept=0, color="red", alpha=0.7, size=1)+
  annotate("rect", ymin=rope_low, ymax=rope_high, xmin=0, xmax=Inf, alpha=.5) +
  scale_fill_viridis(discrete=T, begin=0, end=0.75) +
  facet_wrap(~Language, ncol=8) +
  scale_x_discrete(name=NULL, labels=NULL) +
  scale_y_continuous(breaks = c(0.3, 0, -0.3)) +
  scale_alpha(guide="none") +
  theme(legend.position='bottom') + labs(fill="")

word_initial <- lang_params %>% 
  filter(Language %in% only_word) %>% 
  ggplot(aes(x=Parameter, y=Estimate)) +
  geom_crossbar(aes(ymin=hpdi_89_low, ymax=hpdi_89_high, fill=Parameter,
                    alpha=ifelse(outside==TRUE, 1, 0.1)), 
                size=0.5, width=0.7, fatten=0) + 
  geom_errorbar(aes(ymin=hpdi_low, ymax=hpdi_high, width=0.5)) +
  geom_hline(yintercept=0, color="red", alpha=0.7, size=1)+
  annotate("rect", ymin=rope_low, ymax=rope_high, xmin=0, xmax=Inf, alpha=.5) +
  scale_fill_viridis(discrete=T, begin=0, end=0.75) +
  facet_wrap(~Language, ncol=8) +
  scale_x_discrete(name=NULL, labels=NULL) +
  scale_y_continuous(breaks = c(0.3, 0, -0.3)) +
  scale_alpha(guide="none") +
  theme(legend.position='bottom') + labs(fill="")

comb_one <- (word_initial/utt_initial) + 
  plot_layout(guides = "collect", heights=c(0.66, 0.33)) & theme(legend.position = "bottom")
ggsave("images/viz_lang_one.png", comb_one, scale=1,
       width=2500, height=1700, units="px")

#########################################
###     Speaker parameters            ###
#########################################
lang_estimates <- lang_params %>% select(Language, Parameter, Estimate) %>% 
  mutate(Language=str_replace(Language, "Mojeño.Trinitario", "Mojeño Trinitario"))

speaker_variation <- para_vals %>% 
  filter(grepl("^r_.*speaker.*initial]", .$Parameter)) %>%
  mutate(Parameter=gsub("r_Language:speaker\\[(.*)(,-initial|,)(.*)]", "\\1__\\3", Parameter)) %>% 
  separate(sep="__", col=Parameter, into=c("speaker", "Parameter")) %>% 
  separate(sep="_[a-z]*[0-9]*", col=speaker, into=c("Language", "speaker")) %>% 
  mutate(Language=str_replace(Language, "\\.", " ")) %>% 
  left_join(lang_params, by=c("Language", "Parameter")) %>% 
  transmute(Language=Language, speaker=speaker, Parameter=Parameter,
         Estimate=Estimate.x + Estimate.y,
         Estimate_Lang=Estimate.y,
         hpdi_89_low=(hpdi_89_low.x + Estimate_Lang),
         hpdi_89_high=(hpdi_89_high.x + Estimate_Lang),
         hpdi_low=(hpdi_low.x + Estimate_Lang),
         hpdi_high=(hpdi_high.x + Estimate_Lang)) %>% 
  mutate(outside=ifelse(hpdi_89_high < rope_low, 1, 
                          ifelse(hpdi_89_low > rope_high, 1, 0)))

#########################################
###     Speaker plots                 ###
#########################################
spk_low <- speaker_variation %>% filter(Language %in% c("Yurakaré", "Lower.Sorbian", "Resígaro"))
spk_high <- speaker_variation %>% filter(Language %in% c("Arapaho", "Evenki"))
spk_sorb <- speaker_variation %>% filter(Language == "Lower Sorbian")

speaker_plot <- spk_high %>% 
  ggplot(aes(x=speaker, y=Estimate)) +
  geom_crossbar(aes(ymin=hpdi_89_low, ymax=hpdi_89_high, fill=Language,
                    alpha=ifelse(outside == 1, 1, 0.1)), 
                size=0.5, width=0.5, fatten=0) + 
  geom_errorbar(aes(ymin=hpdi_low, ymax=hpdi_high, width=0.3)) +
  geom_hline(yintercept=0, color="red", alpha=0.5, size=0.5)+
  annotate("rect", ymin=rope_low, ymax=rope_high, xmin=0, xmax=Inf, alpha=.5) +
  scale_fill_viridis(discrete =T, begin=0, end=0.2) +
  facet_grid(Parameter~Language, space="free_x", scales="free_x") +
  scale_x_discrete(name=NULL, labels=NULL)  +
  scale_y_continuous(limits=c(-0.61, 0.55), name="Estimated value") +
  theme(legend.position='none')

speaker_plot_sorb <- spk_sorb %>% 
  ggplot(aes(x=speaker, y=Estimate)) +
  geom_crossbar(aes(ymin=hpdi_89_low, ymax=hpdi_89_high, fill=Language,
                    alpha=ifelse(outside == 1, 1, 0.1)), 
                size=0.5, width=0.5, fatten=0) + 
  geom_errorbar(aes(ymin=hpdi_low, ymax=hpdi_high, width=0.3)) +
  geom_hline(yintercept=0, color="red", alpha=0.5, size=0.5)+
  annotate("rect", ymin=rope_low, ymax=rope_high, xmin=0, xmax=Inf, alpha=.5) +
  scale_fill_viridis(discrete =T, begin=0.35, end=0.35) +
  facet_grid(Parameter~Language, space="free_x", scales="free_x") +
  scale_x_discrete(name=NULL, labels=NULL)  +
  scale_y_continuous(limits=c(-0.61, 0.55), name="Estimated value") +
  theme(legend.position='none')

speaker_plot2 <- spk_low %>% 
  ggplot(aes(x=speaker, y=Estimate)) +
  geom_crossbar(aes(ymin=hpdi_89_low, ymax=hpdi_89_high, fill=Language,
                    alpha=ifelse(outside == 1, 1, 0.1)), 
                size=0.5, width=0.5, fatten=0) + 
  geom_errorbar(aes(ymin=hpdi_low, ymax=hpdi_high, width=0.3)) +
  geom_hline(yintercept=0, color="red", alpha=0.5, size=0.5)+
  annotate("rect", ymin=rope_low, ymax=rope_high, xmin=0, xmax=Inf, alpha=.5) +
  scale_fill_viridis(discrete =T, begin=0.5, end=0.7) +
  facet_grid(Parameter~Language, space="free_x", scales="free_x") +
  scale_x_discrete(name=NULL, labels=NULL)  +
  scale_y_continuous(limits=c(-0.61, 0.55), name="Estimated value") +
  theme(legend.position='none')

ggsave("images/viz_LowerSorbian_speakers.png", speaker_plot_sorb, scale=1,
       width=2000, height=2000, units="px")

speakers_combined <- speaker_plot / speaker_plot_urum / speaker_plot2
ggsave("images/viz_speakers.png", speakers_combined, scale=1,
       width=2000, height=2900, units="px")

short_word_spk <- speaker_variation %>% filter(Language %in% c("Ruuli", "Vera'a"),
                                               Parameter == "word-initial")
long_utt_spk <-  speaker_variation %>% filter(Language %in% c("Jejuan", "Svan", "Yali"),
                                              Parameter == "utterance-initial")
short_utt_spk <- speaker_variation %>% filter(Language %in% c("Fanbyak", "Mojeño Trinitario"),
                                              Parameter == "utterance-initial")

short_word_plot <- short_word_spk %>% 
  ggplot(aes(x=speaker, y=Estimate)) +
  geom_crossbar(aes(ymin=hpdi_89_low, ymax=hpdi_89_high, fill=Language,
                    alpha=ifelse(outside == 1, 1, 0.1)), 
                size=0.5, width=0.5, fatten=0) + 
  geom_errorbar(aes(ymin=hpdi_low, ymax=hpdi_high, width=0.3)) +
  geom_hline(yintercept=0, color="red", alpha=0.5, size=0.5)+
  annotate("rect", ymin=rope_low, ymax=rope_high, xmin=0, xmax=Inf, alpha=.5) +
  scale_fill_viridis(discrete =T, begin=0, end=0.1) +
  facet_grid(Parameter~Language, space="free_x", scales="free_x") +
  scale_x_discrete(name=NULL, labels=NULL)  +
  scale_y_continuous(limits=c(-0.61, 0.55), name="Estimated value") +
  theme(legend.position='none')

long_utt_plot <- long_utt_spk %>% 
  ggplot(aes(x=speaker, y=Estimate)) +
  geom_crossbar(aes(ymin=hpdi_89_low, ymax=hpdi_89_high, fill=Language,
                    alpha=ifelse(outside == 1, 1, 0.1)), 
                size=0.5, width=0.5, fatten=0) + 
  geom_errorbar(aes(ymin=hpdi_low, ymax=hpdi_high, width=0.3)) +
  geom_hline(yintercept=0, color="red", alpha=0.5, size=0.5)+
  annotate("rect", ymin=rope_low, ymax=rope_high, xmin=0, xmax=Inf, alpha=.5) +
  scale_fill_viridis(discrete =T, begin=0.2, end=0.5) +
  facet_grid(Parameter~Language, space="free_x", scales="free_x") +
  scale_x_discrete(name=NULL, labels=NULL)  +
  scale_y_continuous(limits=c(-0.61, 0.55), name="Estimated value") +
  theme(legend.position='none')

short_utt_plot <- short_utt_spk %>% 
  ggplot(aes(x=speaker, y=Estimate)) +
  geom_crossbar(aes(ymin=hpdi_89_low, ymax=hpdi_89_high, fill=Language,
                    alpha=ifelse(outside == 1, 1, 0.1)), 
                size=0.5, width=0.5, fatten=0) + 
  geom_errorbar(aes(ymin=hpdi_low, ymax=hpdi_high, width=0.3)) +
  geom_hline(yintercept=0, color="red", alpha=0.5, size=0.5)+
  annotate("rect", ymin=rope_low, ymax=rope_high, xmin=0, xmax=Inf, alpha=.5) +
  scale_fill_viridis(discrete =T, begin=0.6, end=0.7) +
  facet_grid(Parameter~Language, space="free_x", scales="free_x") +
  scale_x_discrete(name=NULL, labels=NULL)  +
  scale_y_continuous(limits=c(-0.61, 0.55), name="Estimated value") +
  theme(legend.position='none')

speakers_single <- short_word_plot / long_utt_plot / short_utt_plot
ggsave("./images/viz_singleEffect.png", speakers_single, scale=1,
       width=2000, height=1450, units="px")
