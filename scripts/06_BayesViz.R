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
model <- readRDS(file='models/cl_max.rds')

languages <- read_csv('languages.csv')

rope_high <-  0.01
rope_low <-  -0.01


#########################################
###     Parameters                    ###
#########################################
hpdi_89 <- posterior_interval(model, prob=0.89) %>% data.frame() %>% as_tibble(rownames='Parameter') %>% 
  rename(hpdi_89_low=X5.5., hpdi_89_high=X94.5.)

hpdi_95 <- posterior_interval(model, prob=0.95) %>% data.frame() %>% as_tibble(rownames='Parameter') %>% 
  rename(hpdi_low=X2.5., hpdi_high=X97.5.)

para_vals <- posterior_summary(model) %>% as_tibble(rownames='Parameter') %>% 
  left_join(hpdi_89) %>% left_join(hpdi_95) %>% 
  mutate(
    Parameter=str_replace(Parameter, 'b_', ''),
    Parameter=str_replace(Parameter, '_initial', '-initial'),
    Parameter=str_replace(Parameter, 'cluster_status', 'cs_')
    )

fix_eff <- para_vals %>% filter(Parameter %in% c('word-initial', 'utt-initial', 'cs_noInitial', 'cs_noCluster'))

lang_params <- para_vals %>% 
  filter(grepl('^r_Language\\[.*', Parameter)) %>% 
  mutate(
    Parameter=str_replace(Parameter, 'r_Language\\[', ''),
    Parameter=str_replace(Parameter, '\\]', '')
    ) %>% 
  separate(sep=',', col=Parameter, into=c('Language', 'Parameter')) %>% 
  filter(Parameter != 'Intercept') %>% 
  left_join(fix_eff, by='Parameter') %>%
  transmute(
    Language=Language, Parameter=Parameter,
    Estimate=Estimate.x + Estimate.y,
    hpdi_89_high=hpdi_89_high.x + hpdi_89_high.y,
    hpdi_89_low=hpdi_89_low.x + hpdi_89_low.y,
    hpdi_high=hpdi_high.x + hpdi_high.y,
    hpdi_low=hpdi_low.x + hpdi_low.y
   ) %>%
  mutate(outside=ifelse(hpdi_89_high < rope_low, TRUE, ifelse(hpdi_89_low > rope_high, TRUE, FALSE))) %>% 
  mutate(Glottocode=Language) %>% select(-Language) %>% left_join(languages)

langWord <- lang_params %>% filter(Parameter == 'word-initial')
langUtt <- lang_params %>% filter(Parameter == 'utt-initial')
langCluster <- lang_params %>% filter(Parameter %in% c('cs_noInitial', 'cs_noCluster'))


pop_level <- c(
  'z_speech_rate', 'z_num_phones', 'z_word_freq',
  'utt-initial', 'word-initial',
  'cluster-status_noInitial', 'cluster-status_noCluster'
  )

#########################################
###   Tables: Raw values              ###
#########################################
fixed_effects <- para_vals %>% filter(Parameter %in% pop_level) %>% 
  mutate(
    Estimate=round(Estimate, 2),
    Parameter=str_replace(Parameter, 'z_', ''),
    Parameter=str_replace(Parameter, 'num_phones', 'phones per word'),
    Parameter=str_replace(Parameter, 'word_freq', 'word-form frequency'),
    Parameter=str_replace(Parameter, 'speech_rate', 'speech rate'),
    '95% HPDI'=paste(format(round(hpdi_low, 2), nsmall=2), 'to', format(round(hpdi_high, 2), nsmall=2))
    ) %>%
  select(Parameter, Estimate, '95% HPDI') %>% 
  xtable(caption='95% HPDI of the population-level predictors', label='table: fixed_effects')
print(xtable(fixed_effects), include.rownames=FALSE)


#########################################
###    Overall areas                  ###
#########################################
np_max <- nuts_params(model)
posterior_max <- as.array(model)

overall_areas <- model %>%
  mcmc_areas(regex_pars=c('^b_(utt|word)_initial$', '^b_z_', '^b_cluster_status'), prob=0.89, prob_outer=0.997, point_est='mean') +
  geom_vline(xintercept=0, color='red', alpha=0.5, linewidth=1)+
  annotate('rect', xmin=rope_low, xmax=rope_high, ymin=0, ymax=Inf, alpha=.3) +
  scale_y_discrete(labels=c(
    'utt-initial','word-initial',
    'phones per word', 'word-form freq', 'speech rate',
    'singleton', 'cluster-internal')) +
  scale_x_continuous(name='Effect on log-scale') +
  theme_bw()

ggsave('images/viz_overall.png', overall_areas, scale=1, width=2000, height=1000, units='px')

#########################################
###     Language plots              ###
#########################################
word_init <- lang_params %>% 
  filter(Parameter == 'word-initial') %>% 
  ggplot(aes(x=Estimate, y=reorder(Language, Estimate))) +
  geom_errorbar(aes(xmin=hpdi_low, xmax=hpdi_high)) + 
  geom_crossbar(aes(
    xmin=hpdi_89_low, xmax=hpdi_89_high, fill=Parameter,
    alpha=ifelse(outside == 1, 0.8, 0.5)),
    fatten=2, linewidth=0.5, width=0.8
    ) +  
  geom_vline(xintercept=0, color='red') +
  annotate('rect', xmin=rope_low, xmax=rope_high, ymin=0, ymax=Inf, alpha=.2) +
  scale_y_discrete(name=NULL) +
  scale_fill_viridis(discrete=T, begin=0.75, end=0.75) +
  scale_x_continuous(name=NULL) +
  theme(legend.position='none')

ggsave('images/viz_wordInit.png', word_init, width=2000, height=2500, units='px')

###################################################################
utt_init <- lang_params %>%
  filter(Parameter == 'utt-initial') %>% 
  ggplot(aes(x=Estimate, y=reorder(Language, Estimate))) +
  geom_errorbar(aes(xmin=hpdi_low, xmax=hpdi_high)) + 
  geom_crossbar(aes(
    xmin=hpdi_89_low, xmax=hpdi_89_high, fill=Parameter,
    alpha=ifelse(outside == 1, 0.8, 0.5)), 
    fatten=2, linewidth=0.5, width=0.8
    ) +  
  geom_vline(xintercept=0, color='red') +
  annotate('rect', xmin=rope_low, xmax=rope_high, ymin=0, ymax=Inf, alpha=.2) +
  scale_y_discrete(name=NULL) +
  scale_fill_viridis(discrete=T, begin=0.35, end=0.35) +
  scale_x_continuous(name=NULL) +
  theme(legend.position='none')

ggsave('images/viz_uttInit.png', utt_init, width=2000, height=2500, units='px')

combined <- lang_params %>% 
  filter(Parameter %in% c('utt-initial', 'word-initial')) %>% 
  mutate(Parameter=str_replace(Parameter, 'utt-', 'utterance-')) %>% 
  ggplot(aes(x=Parameter, y=Estimate)) +
  geom_crossbar(
    aes(
      ymin=hpdi_89_low,
      ymax=hpdi_89_high,
      fill=Parameter,
      alpha=ifelse(outside==TRUE, 1, 0.1)
      ), 
    linewidth=0.5, width=0.7, fatten=0
    ) + 
  geom_errorbar(aes(ymin=hpdi_low, ymax=hpdi_high, width=0.5)) +
  annotate('rect', ymin=rope_low, ymax=rope_high, xmin=0, xmax=Inf, alpha=.8) +
  scale_fill_viridis(discrete=T, begin=0.35, end=0.75) +
  facet_wrap(~Language, ncol=6) +
  scale_x_discrete(name=NULL, labels=NULL) +
  scale_y_continuous(breaks=c(0.3, 0, -0.3)) +
  scale_alpha(guide='none') +
  theme(legend.position='bottom') + labs(fill='')

ggsave('images/viz_combined.png', combined, width=3000, height=4000, scale=0.8, units='px')

###################################################################
cluster_plot <- lang_params %>%
  filter(Parameter %in% c('cs_noCluster', 'cs_noInitial')) %>% 
  ggplot(aes(x=Estimate, y=reorder(Language, Estimate))) +
  geom_errorbar(aes(xmin=hpdi_low, xmax=hpdi_high)) + 
  geom_crossbar(aes(
    xmin=hpdi_89_low, xmax=hpdi_89_high, fill=Parameter,
    alpha=ifelse(outside == 1, 0.8, 0.5)), 
    fatten=2, linewidth=0.5, width=0.8
    ) +  
  geom_vline(xintercept=0, color='red') +
  annotate('rect', xmin=rope_low, xmax=rope_high, ymin=0, ymax=Inf, alpha=.2) +
  scale_y_discrete(name=NULL) +
  scale_x_continuous(name=NULL) +
  scale_fill_viridis(discrete=T, begin=0.35, end=0.35) +
  facet_wrap(~Parameter) +
  theme(legend.position='none')

ggsave('images/viz_cluster.png', cluster_plot, width=2000, height=2500, units='px')

#########################################
###   Sound Class analysis            ###
#########################################
sc_params <- para_vals %>% 
  filter(grepl('^r_CLTS\\[.*', Parameter)) %>% 
  mutate(
    Parameter=gsub('^r_CLTS\\[(.*)(,-initial|,)(.*)]','\\1_\\3',Parameter),
    Parameter=str_replace(Parameter, 'consonant', ''),
    CLTS=ifelse(str_detect(Parameter, 'click'), 'click', ifelse(
      str_detect(Parameter, 'stop'), 'stop', ifelse(
        str_detect(Parameter, 'nasal'), 'nasal', ifelse(
          str_detect(Parameter, 'trill'), 'trill', ifelse(
            str_detect(Parameter, 'ejective'), 'ejective', ifelse(
              str_detect(Parameter, 'affricate'), 'affricate', ifelse(
                str_detect(Parameter, 'fricative'), 'fricative', ifelse(
                  str_detect(Parameter, 'approximant'), 'approximant', ifelse(
                    str_detect(Parameter, 'implosive'), 'implosive', ifelse(
                      str_detect(Parameter, 'tap'), 'tap', 'other'
    ))))))))))) %>% 
  mutate(outside=ifelse(hpdi_89_high < rope_low, TRUE, ifelse(hpdi_89_low > rope_high, TRUE, FALSE)))

sc_plot_utterance<- sc_params %>% 
  filter(str_detect(Parameter, 'utt-initial')) %>% 
  ggplot(aes(x=reorder(Parameter, Estimate), y=Estimate)) +
  geom_crossbar(
    aes(
      ymin=hpdi_89_low,
      ymax=hpdi_89_high,
      fill=CLTS,
      alpha=ifelse(outside == 1, 1, 0.1)
    ), 
    linewidth=0.5, width=0.5, fatten=0
  ) + 
  geom_errorbar(aes(ymin=hpdi_low, ymax=hpdi_high, width=0.3)) +
  geom_hline(yintercept=0, color='red', alpha=0.5, linewidth=0.5)+
  annotate('rect', ymin=rope_low, ymax=rope_high, xmin=0, xmax=Inf, alpha=.5) +
  scale_fill_viridis(discrete=T, begin=0, end=1) +
  scale_y_continuous(breaks=c(0.3, 0, -0.3)) +
  scale_alpha(guide='none') +
  facet_wrap(~CLTS, scales='free_x') +
  theme(legend.position='bottom', axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + labs(fill='')

ggsave('images/viz_sc_utterance.png', sc_plot_utterance, scale=1, width=6000, height=6000, units='px')


sc_plot_word <- sc_params %>% 
  filter(str_detect(Parameter, 'word-initial')) %>% 
  ggplot(aes(x=reorder(Parameter, Estimate), y=Estimate)) +
  geom_crossbar(
    aes(
      ymin=hpdi_89_low,
      ymax=hpdi_89_high,
      fill=CLTS,
      alpha=ifelse(outside == 1, 1, 0.1)
    ), 
    linewidth=0.5, width=0.5, fatten=0
  ) + 
  geom_errorbar(aes(ymin=hpdi_low, ymax=hpdi_high, width=0.3)) +
  geom_hline(yintercept=0, color='red', alpha=0.5, linewidth=0.5)+
  annotate('rect', ymin=rope_low, ymax=rope_high, xmin=0, xmax=Inf, alpha=.5) +
  scale_fill_viridis(discrete=T, begin=0, end=1) +
  scale_y_continuous(breaks=c(0.3, 0, -0.3)) +
  scale_alpha(guide='none') +
  facet_wrap(~CLTS, scales='free_x') +
  theme(legend.position='bottom', axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + labs(fill='')

ggsave('images/viz_sc_word.png', sc_plot_word, scale=1, width=6000, height=6000, units='px')


#########################################
###     Speaker parameters            ###
#########################################
speaker_variation <- para_vals %>% 
  filter(grepl('^r.*Speaker', .$Parameter)) %>%
  mutate(Parameter=gsub('r_Speaker\\[(.*)()(.*)]', '\\1__\\3', Parameter)) %>% 
  separate(sep=',', col=Parameter, into=c('Speaker', 'Parameter')) %>% 
  separate(sep='_', col=Speaker, into=c('Language', 'Speaker')) %>% 
  mutate(Language=str_replace(Language, '\\.', ' ')) %>% 
  mutate(Parameter=str_replace(Parameter, '__', '')) %>% 
  mutate(Glottocode=Language) %>% select(-Language) %>% left_join(languages) %>% 
  filter(Parameter != 'Intercept') %>% 
  left_join(lang_params, by=c('Language', 'Parameter')) %>% 
  transmute(
    Language=Language, Speaker=Speaker, Parameter=Parameter,
    Estimate=Estimate.x + Estimate.y,
    hpdi_89_low=(hpdi_89_low.x + Estimate.y),
    hpdi_89_high=(hpdi_89_high.x + Estimate.y),
    hpdi_low=(hpdi_low.x + Estimate.y),
    hpdi_high=(hpdi_high.x + Estimate.y)
    ) %>% 
  mutate(outside=ifelse(hpdi_89_high < rope_low, 1, ifelse(hpdi_89_low > rope_high, 1, 0)))

#########################################
###     Speaker plots                 ###
#########################################
speaker_plot_word <- speaker_variation %>% 
  filter(Parameter=='word-initial') %>% 
  ggplot(aes(x=Speaker, y=Estimate)) +
  geom_crossbar(
    aes(
      ymin=hpdi_89_low,
      ymax=hpdi_89_high,
      fill=Language,
      alpha=ifelse(outside == 1, 1, 0.1)
      ), 
    linewidth=0.5, width=0.5, fatten=0
    ) + 
  geom_errorbar(aes(ymin=hpdi_low, ymax=hpdi_high, width=0.3)) +
  geom_hline(yintercept=0, color='red', alpha=0.5, linewidth=0.5)+
  annotate('rect', ymin=rope_low, ymax=rope_high, xmin=0, xmax=Inf, alpha=.5) +
  scale_fill_viridis(discrete=T, begin=0, end=0.2) +
  facet_wrap(~Language, scales='free_x', ncol=6) +
  scale_x_discrete(name=NULL, labels=NULL)  +
  scale_y_continuous(limits=c(-0.4, 0.4), name='Estimated value') +
  theme(legend.position='none')

ggsave('images/speaker_word.png', speaker_plot_word, scale=0.8, width=3000, height=4000, units='px')

speaker_plot_utt <- speaker_variation %>% 
  filter(Parameter=='utt-initial') %>% 
  ggplot(aes(x=Speaker, y=Estimate)) +
  geom_crossbar(
    aes(
      ymin=hpdi_89_low,
      ymax=hpdi_89_high,
      fill=Language,
      alpha=ifelse(outside == 1, 1, 0.1)
      ), 
    linewidth=0.5, width=0.5, fatten=0
    ) + 
  geom_errorbar(aes(ymin=hpdi_low, ymax=hpdi_high, width=0.3)) +
  geom_hline(yintercept=0, color='red', alpha=0.5, linewidth=0.5)+
  annotate('rect', ymin=rope_low, ymax=rope_high, xmin=0, xmax=Inf, alpha=.5) +
  scale_fill_viridis(discrete =T, begin=0, end=0.2) +
  facet_wrap(~Language, scales='free_x', ncol=6) +
  scale_x_discrete(name=NULL, labels=NULL)  +
  scale_y_continuous(limits=c(-0.6, 0.45), name='Estimated value') +
  theme(legend.position='none')

ggsave('images/speaker_utt.png', speaker_plot_utt, scale=0.8, width=3000, height=4000, units='px')
