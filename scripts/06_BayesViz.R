library(bayesplot)
library(brms)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(tidybayes)
library(patchwork)
library(viridis)
library(xtable)


###################################################################
model <- readRDS(file="models/cl_noCluster.rds")

languages <- read_csv('languages.csv')

rope_high = 0.01
rope_low = -0.01


#########################################
###     Parameters                    ###
#########################################
hpdi_89 <- posterior_interval(model, prob=0.89) %>% 
  data.frame() %>% as_tibble(rownames="Parameter") %>% 
  rename(hpdi_89_low=X5.5., hpdi_89_high=X94.5.)


hpdi_95 <- posterior_interval(model, prob=0.95) %>% 
  data.frame() %>% as_tibble(rownames="Parameter") %>% 
  rename(hpdi_low=X2.5., hpdi_high=X97.5.)

para_vals <- posterior_summary(model) %>% 
  data.frame() %>% as_tibble(rownames="Parameter") %>% 
  left_join(hpdi_89) %>% left_join(hpdi_95) %>% 
  mutate(Parameter=str_replace(Parameter, "b_([a-z]*)_initial1", "\\1-initial"),
         Parameter=str_replace(Parameter, "b_", "")) 

fix_eff <- para_vals %>% 
  filter(Parameter %in% c("word-initial", "utt-initial", "ClusterInitial"))

lang_params <- para_vals %>% 
  filter(grepl("^r_Language\\[.*", Parameter)) %>% 
  mutate(Parameter=gsub("r_Language\\[(.*)(,_initial1|,)(.*)]", "\\1__\\3", Parameter),
         Parameter=str_replace(Parameter, "_initial1", "-initial")) %>% 
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
         Language=str_replace(Language, "\\.", " ")) %>% 
  mutate(Glottocode=Language) %>% select(-Language) %>% 
  left_join(languages)

langWord <- lang_params %>% filter(Parameter == "word-initial")
langUtt <- lang_params %>% filter(Parameter == "utt-initial")
langCluster <- lang_params %>% filter(Parameter == "ClusterInitial")


pop_level <- c("b_z_speech_rate", "b_z_num_phones", "b_z_word_freq",
               "utt-initial", "word-initial")

#########################################
###   Tables: Raw values              ###
#########################################
fixed_effects <- para_vals %>% filter(Parameter %in% pop_level) %>% 
  mutate(Estimate=round(Estimate, 3),
         Parameter=str_replace(Parameter, "b_Initial", ""),
         Parameter=str_replace(Parameter, "b_z_", ""),
         Parameter=str_replace(Parameter, "num_phones", "phones per word"),
         Parameter=str_replace(Parameter, "word_freq", "word-form frequency"),
         Parameter=str_replace(Parameter, "speech_rate", 'speech rate'),
         "95% HPDI"=paste(format(round(hpdi_low, 2), nsmall=2), "to", 
                              format(round(hpdi_high, 2), nsmall=2))) %>%
  select(Parameter, Estimate, "95% HPDI") %>% 
  xtable(caption="95% HPDI of the population-level predictors",
         label="table: fixed_effects")
print(xtable(fixed_effects), include.rownames=FALSE)

# This table is not currently used in the paper
corr <- para_vals %>% filter(str_detect(para_vals$Parameter, "cor")) %>% 
  mutate(Parameter=str_replace(Parameter, "Speaker", "Speaker"),
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
np_max <- nuts_params(model)
posterior_max <- as.array(model)

overall_areas <- mcmc_areas(model, regex_pars=c("^b_(utt|word)_initial1$", "z_"),
             prob=0.89, prob_outer=0.997, point_est="mean") +
  geom_vline(xintercept=0, color="red", alpha=0.5, linewidth=1)+
  annotate("rect", xmin=rope_low, xmax=rope_high, ymin=0, ymax=Inf, alpha=.3) +
  scale_y_discrete(labels=c('Utterance-initial','Word-initial','Speech rate', 
                            'Phones per word', 'Word-form frequency')) +
  scale_x_continuous(name="Effect on log-scale") +
  theme_bw()

ggsave('images/viz_overall.png', overall_areas, scale=1,
       width=2000, height=1000, units="px")

#########################################
###     Language plots              ###
#########################################
word_init <- lang_params %>% 
  filter(Parameter == "word-initial") %>% 
  ggplot(aes(x=Estimate, y=reorder(Language, Estimate))) +
  geom_errorbar(aes(xmin=hpdi_low, xmax=hpdi_high)) + 
  geom_crossbar(aes(
    xmin=hpdi_89_low, xmax=hpdi_89_high, fill=Parameter,
    alpha=ifelse(outside == 1, 0.8, 0.5)), fatten=2, linewidth=0.5, width=0.8) +  
  geom_vline(xintercept=0, color="red") +
  annotate("rect", xmin=rope_low, xmax=rope_high, ymin=0, ymax=Inf, alpha=.2) +
  scale_y_discrete(name=NULL) +
  scale_fill_viridis(discrete=T, begin=0.75, end=0.75) +
  scale_x_continuous(name=NULL) +
  theme(legend.position="none")

ggsave("images/viz_wordInit_noCluster.png", word_init,
       width=2000, height=2500, units="px")

###################################################################
utt_init <- lang_params %>%
  filter(Parameter == "utt-initial") %>% 
  ggplot(aes(x=Estimate, y=reorder(Language, Estimate))) +
  geom_errorbar(aes(xmin=hpdi_low, xmax=hpdi_high)) + 
  geom_crossbar(aes(
    xmin=hpdi_89_low, xmax=hpdi_89_high, fill=Parameter,
    alpha=ifelse(outside == 1, 0.8, 0.5)), 
    fatten=2, linewidth=0.5, width=0.8) +  
  geom_vline(xintercept=0, color="red") +
  annotate("rect", xmin=rope_low, xmax=rope_high, ymin=0, ymax=Inf, alpha=.2) +
  scale_y_discrete(name=NULL) +
  scale_fill_viridis(discrete=T, begin=0.35, end=0.35) +
  scale_x_continuous(name=NULL) +
  theme(legend.position="none")

ggsave("images/viz_uttInit.png", utt_init,
       width=2000, height=2500, units="px")

combined <- lang_params %>% 
  mutate(Parameter=str_replace(Parameter, "utt-", "utterance-")) %>% 
  ggplot(aes(x=Parameter, y=Estimate)) +
  geom_crossbar(aes(ymin=hpdi_89_low, ymax=hpdi_89_high, fill=Parameter,
                    alpha=ifelse(outside==TRUE, 1, 0.1)), 
                linewidth=0.5, width=0.7, fatten=0) + 
  geom_errorbar(aes(ymin=hpdi_low, ymax=hpdi_high, width=0.5)) +
  annotate("rect", ymin=rope_low, ymax=rope_high, xmin=0, xmax=Inf,
           alpha=.8) +
  scale_fill_viridis(discrete=T, begin=0.35, end=0.75) +
  facet_wrap(~Language, ncol=6) +
  scale_x_discrete(name=NULL, labels=NULL) +
  scale_y_continuous(breaks=c(0.3, 0, -0.3)) +
  scale_alpha(guide="none") +
  theme(legend.position='bottom') + labs(fill="")

ggsave("images/viz_combined.png", combined,
       width=3000, height=4000, scale=0.8, units="px")

###################################################################
cluster_init <- lang_params %>%
  filter(Parameter == "ClusterInitial") %>% 
  ggplot(aes(x=Estimate, y=reorder(Language, Estimate))) +
  geom_errorbar(aes(xmin=hpdi_low, xmax=hpdi_high)) + 
  geom_crossbar(aes(
    xmin=hpdi_89_low, xmax=hpdi_89_high, fill=Parameter,
    alpha=ifelse(outside == 1, 0.8, 0.5)), 
    fatten=2, linewidth=0.5, width=0.8) +  
  geom_vline(xintercept=0, color="red") +
  annotate("rect", xmin=rope_low, xmax=rope_high, ymin=0, ymax=Inf, alpha=.2) +
  scale_y_discrete(name=NULL) +
  scale_fill_viridis(discrete=T, begin=0.35, end=0.35) +
  scale_x_continuous(name=NULL) +
  theme(legend.position="none")

ggsave("images/viz_cluster_init.png", cluster_init,
       width=2000, height=2500, units="px")
#########################################
###   Sound Class analysis            ###
#########################################
sc_params <- para_vals %>% 
  filter(grepl("^r_Language:sound_class\\[.*", Parameter)) %>% 
  mutate(
    Parameter=gsub("^r_Language:sound_class\\[(.*)(,-initial|,)(.*)]","\\1__\\3",Parameter),
    Parameter=str_replace(Parameter, "_initial1", "-initial"),
    Parameter=str_replace(Parameter, "__", "_")) %>% 
  separate(sep="_", col=Parameter, into=c("Language", "SoundClass", "Parameter")) %>% 
  filter(Parameter != "Intercept") %>% 
  mutate(Language=str_replace(Language, "\\.", " ")) %>% 
  mutate(Glottocode=Language) %>% select(-Language) %>% left_join(languages) %>% 
  left_join(lang_params, by=c("Language", "Parameter")) %>%
  transmute(
    Language=Language, Parameter=Parameter, SoundClass=SoundClass,
    Estimate=Estimate.x + Estimate.y,
    hpdi_89_high=hpdi_89_high.x + hpdi_89_high.y,
    hpdi_89_low=hpdi_89_low.x + hpdi_89_low.y,
    hpdi_high=hpdi_high.x + hpdi_high.y,
    hpdi_low=hpdi_low.x + hpdi_low.y
  ) %>%
  mutate(outside=ifelse(hpdi_89_high < rope_low, TRUE, 
                        ifelse(hpdi_89_low > rope_high, TRUE, FALSE)),
         outside_99=ifelse(hpdi_high < rope_low, "yes", 
                           ifelse(hpdi_low > rope_high, "yes", "no")))

sc_per_lang_word <- sc_params %>% 
  filter(Parameter=="word-initial") %>% 
  ggplot(aes(x=SoundClass, y=Estimate)) +
  geom_crossbar(aes(ymin=hpdi_89_low, ymax=hpdi_89_high, fill=SoundClass,
                    alpha=ifelse(outside==TRUE, 1, 0.1)), 
                linewidth=0.5, width=0.7, fatten=0) + 
  geom_errorbar(aes(ymin=hpdi_low, ymax=hpdi_high, width=0.5)) +
  geom_hline(yintercept=0, color="red", alpha=0.7, linewidth=1)+
  annotate("rect", ymin=rope_low, ymax=rope_high, xmin=0, xmax=Inf, alpha=.5) +
  scale_fill_viridis(discrete=T, begin=0, end=1) +
  facet_wrap(~Language, ncol=4) +
  scale_x_discrete(name=NULL, labels=NULL) +
  scale_y_continuous(breaks=c(0.3, 0, -0.3)) +
  scale_alpha(guide="none") +
  theme(legend.position='bottom') + labs(fill="")

ggsave('images/sc_per_lang_word.png', sc_per_lang_word, scale=1,
       width=2200, height=3500, units="px")

sc_per_lang_utt <- sc_params %>% 
  filter(Parameter=="utt-initial") %>% 
  ggplot(aes(x=SoundClass, y=Estimate)) +
  geom_crossbar(aes(ymin=hpdi_89_low, ymax=hpdi_89_high, fill=SoundClass,
                    alpha=ifelse(outside==TRUE, 1, 0.1)), 
                linewidth=0.5, width=0.7, fatten=0) + 
  geom_errorbar(aes(ymin=hpdi_low, ymax=hpdi_high, width=0.5)) +
  geom_hline(yintercept=0, color="red", alpha=0.7, linewidth=1)+
  annotate("rect", ymin=rope_low, ymax=rope_high, xmin=0, xmax=Inf, alpha=.5) +
  scale_fill_viridis(discrete=T, begin=0, end=1) +
  facet_wrap(~Language, ncol=4) +
  scale_x_discrete(name=NULL, labels=NULL) +
  scale_y_continuous(breaks=c(0.3, 0, -0.3)) +
  scale_alpha(guide="none") +
  theme(legend.position='bottom') + labs(fill="")

ggsave('images/sc_per_lang_utt.png', sc_per_lang_utt, scale=1,
       width=2200, height=3500, units="px")

sc_per_param_utt <- sc_params %>% 
  filter(Parameter=="utt-initial") %>% 
  ggplot(aes(x=Language, y=Estimate)) +
  geom_errorbar(aes(ymin=hpdi_low, ymax=hpdi_high, width=0.5)) +
  geom_crossbar(aes(ymin=hpdi_89_low, ymax=hpdi_89_high, fill=SoundClass,
                    alpha=ifelse(outside==TRUE, 1, 0.7)),
                linewidth=0.5, width=0.7, fatten=0) + 
  geom_hline(yintercept=0, color="red", alpha=0.7, linewidth=1)+
  annotate("rect", ymin=rope_low, ymax=rope_high, xmin=0, xmax=Inf, alpha=.5) +
  scale_fill_viridis(discrete=T, begin=0, end=1) +
  facet_grid(SoundClass~Parameter) +
  scale_x_discrete(name=NULL) +
  scale_y_continuous(breaks=c(0.3, 0, -0.3)) +
  scale_alpha(guide="none") +
  theme(legend.position='bottom', strip.text.y=element_blank(),
        axis.text.x=element_text(angle=90, vjust=0.5, hjust=1)) 

ggsave('images/sc_per_param_utt.png', sc_per_param_utt, scale=1,
       width=3000, height=3000, units="px")

sc_per_param_word <- sc_params %>% 
  filter(Parameter=="word-initial") %>% 
  ggplot(aes(x=Language, y=Estimate)) +
  geom_errorbar(aes(ymin=hpdi_low, ymax=hpdi_high, width=0.5)) +
  geom_crossbar(aes(ymin=hpdi_89_low, ymax=hpdi_89_high, fill=SoundClass,
                    alpha=ifelse(outside==TRUE, 1, 0.7)),
                linewidth=0.5, width=0.7, fatten=0) + 
  geom_hline(yintercept=0, color="red", alpha=0.7, linewidth=1)+
  annotate("rect", ymin=rope_low, ymax=rope_high, xmin=0, xmax=Inf, alpha=.5) +
  scale_fill_viridis(discrete=T, begin=0, end=1) +
  facet_grid(SoundClass~Parameter) +
  scale_x_discrete(name=NULL) +
  scale_y_continuous(breaks=c(0.3, 0, -0.3)) +
  scale_alpha(guide="none") +
  theme(legend.position='bottom', strip.text.y=element_blank(),
        axis.text.x=element_text(angle=90, vjust=0.5, hjust=1)) 

ggsave('images/sc_per_param_word.png', sc_per_param_word, scale=1,
       width=3000, height=3000, units="px")

#########################################
###     Speaker parameters            ###
#########################################
speaker_variation <- para_vals %>% 
  filter(grepl("^r.*Speaker", .$Parameter)) %>%
  mutate(Parameter=gsub("r_Language:Speaker\\[(.*)()(.*)]", "\\1__\\3", Parameter)) %>% 
  separate(sep=",", col=Parameter, into=c("Speaker", "Parameter")) %>% 
  separate(sep="_[a-z]*[0-9]*_", col=Speaker, into=c("Language", "Speaker")) %>% 
  mutate(Language=str_replace(Language, "\\.", " ")) %>% 
  mutate(Parameter=str_replace(Parameter, "__", ""),
         Parameter=str_replace(Parameter, "1", ""),
    Parameter=str_replace(Parameter, "_", "-")) %>% 
  mutate(Glottocode=Language) %>% select(-Language) %>% left_join(languages) %>% 
  filter(Parameter != "Intercept") %>% 
  left_join(lang_params, by=c("Language", "Parameter")) %>% 
  transmute(Language=Language, Speaker=Speaker, Parameter=Parameter,
         Estimate=Estimate.x + Estimate.y,
         hpdi_89_low=(hpdi_89_low.x + Estimate.y),
         hpdi_89_high=(hpdi_89_high.x + Estimate.y),
         hpdi_low=(hpdi_low.x + Estimate.y),
         hpdi_high=(hpdi_high.x + Estimate.y)) %>% 
  mutate(outside=ifelse(hpdi_89_high < rope_low, 1, 
                          ifelse(hpdi_89_low > rope_high, 1, 0)))

#########################################
###     Speaker plots                 ###
#########################################
speaker_plot_word <- speaker_variation %>% 
  filter(Parameter=='word-initial') %>% 
  ggplot(aes(x=Speaker, y=Estimate)) +
  geom_crossbar(aes(ymin=hpdi_89_low, ymax=hpdi_89_high, fill=Language,
                    alpha=ifelse(outside == 1, 1, 0.1)), 
                linewidth=0.5, width=0.5, fatten=0) + 
  geom_errorbar(aes(ymin=hpdi_low, ymax=hpdi_high, width=0.3)) +
  geom_hline(yintercept=0, color="red", alpha=0.5, linewidth=0.5)+
  annotate("rect", ymin=rope_low, ymax=rope_high, xmin=0, xmax=Inf, alpha=.5) +
  scale_fill_viridis(discrete =T, begin=0, end=0.2) +
  facet_wrap(~Language, scales="free_x", ncol=6) +
  scale_x_discrete(name=NULL, labels=NULL)  +
  scale_y_continuous(limits=c(-0.3, 0.32), name="Estimated value") +
  theme(legend.position='none')

ggsave('images/speaker_word.png', speaker_plot_word, scale=0.8,
       width=3000, height=4000, units="px")

speaker_plot_utt <- speaker_variation %>% 
  filter(Parameter=='utt-initial') %>% 
  ggplot(aes(x=Speaker, y=Estimate)) +
  geom_crossbar(aes(ymin=hpdi_89_low, ymax=hpdi_89_high, fill=Language,
                    alpha=ifelse(outside == 1, 1, 0.1)), 
                linewidth=0.5, width=0.5, fatten=0) + 
  geom_errorbar(aes(ymin=hpdi_low, ymax=hpdi_high, width=0.3)) +
  geom_hline(yintercept=0, color="red", alpha=0.5, linewidth=0.5)+
  annotate("rect", ymin=rope_low, ymax=rope_high, xmin=0, xmax=Inf, alpha=.5) +
  scale_fill_viridis(discrete =T, begin=0, end=0.2) +
  facet_wrap(~Language, scales="free_x", ncol=6) +
  scale_x_discrete(name=NULL, labels=NULL)  +
  scale_y_continuous(limits=c(-0.32, 0.32), name="Estimated value") +
  theme(legend.position='none')

ggsave('images/speaker_utt.png', speaker_plot_utt, scale=0.8,
       width=3000, height=4000, units="px")
