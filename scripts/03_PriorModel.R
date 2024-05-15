library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(brms)
library(ggplot2)
library(viridis)
library(cmdstanr)
library(bayesplot)
library(patchwork)


data <- read_tsv('data.tsv')
languages <- read_csv('languages.csv')


model <- 
  brm(data=data,
      family=Gamma("log"),
      formula=Duration ~ 1 + utt_initial + word_initial + cluster_status +
        (1 + utt_initial + word_initial + cluster_status | Language) +
        (1 + utt_initial + word_initial | Speaker) +
        (1 + utt_initial + word_initial | CLTS) +
        (1 | Family) +
        z_num_phones + z_word_freq + z_speech_rate,
      prior=c(
        prior(normal(4.4, 0.05), class=Intercept),
        prior(normal(6, 0.5), class=shape),
        prior(normal(0, 0.3), class=b),
        prior(gamma(3, 30), class=sd),
        prior(lkj(5), class=cor)
      ),
      iter=7500, warmup=2500, chains=4, cores=4,
      threads=threading(3),
      control=list(adapt_delta=0.90, max_treedepth=10),
      seed=1,
      silent=0,
      sample_prior='only',
      file="models/cl_prior",
      backend="cmdstanr"
  )


# Prior predictive checks
draws <- 6e3
new_data <- tibble(
  Language='NewLang',
  Family='NewFam',
  Speaker=sample(c('1', '2', '3'), 10000, replace=TRUE),
  word_initial=sample(c(0, 1), 10000, replace=TRUE),
  utt_initial=sample(c(0, 1), 10000, replace=TRUE),
  CLTS=sample(unique(data$CLTS), 10000, replace=TRUE),
  cluster_status=sample(unique(data$cluster_status), 10000, replace=TRUE),
  z_num_phones=rnorm(10000),
  z_word_freq=rnorm(10000),
  z_speech_rate=rnorm(10000)
) %>% mutate(initial=as.factor(ifelse(utt_initial==1, "utterance-initial", ifelse(
  word_initial==1, "word-initial", "other"
))))


if (file.exists("models/priorpred_new.rds")) {
  new_epreds <- readRDS(file="models/priorpred_new.rds")
} else{
  print("Sorry, the file does not yet exist. This may take some time.")
  new_epreds <- new_data %>% 
    add_epred_draws(model, ndraws=draws, allow_new_levels=TRUE, seed=42) %>%
    ungroup() %>% select(Language, utt_initial, word_initial, initial, .epred)
  saveRDS(new_epreds, file="models/priorpred_new.rds")  
}

avg <- new_epreds %>% group_by(initial) %>% summarise(mean=mean(.epred))
print('#############################')
print("Average for expected draws:")
print(avg)
print('#############################')

plot_newdata <- new_epreds %>% 
  ggplot(aes(y=.epred, x=initial)) +
  geom_violin(aes(fill=initial)) +
  geom_boxplot(width=0.5, 
               outlier.size=1, outlier.color="black", outlier.alpha=0.3) +
  scale_fill_viridis(discrete=TRUE, end=0.7) +
  scale_y_log10(limits=c(2, 2000), name="duration on log-axis") +
  scale_x_discrete(label=NULL, name=NULL) +
  theme_grey(base_size=11) +
  theme(legend.position='bottom', legend.title=element_blank())

ggsave(plot_newdata, filename='images/viz_prior_newdata.png', 
       width=2000, height=1500, units="px")


################################################################################
### Prior Predictive Checks                                                 ###
################################################################################
if (file.exists("models/pred_prior.rds")) {
  priorsim_durations <- readRDS(file="models/pred_prior.rds")
} else{
  print("Sorry, the file does not yet exist. This may take some time.")
  priorsim_durations <- posterior_predict(model, ndraws=8, 
                                          cores=getOption("mc.cores", 4))
  saveRDS(priorsim_durations, file="models/pred_prior.rds")
} 

raw_durations <- data %>% .$Duration
prior_box <- ppc_boxplot(raw_durations, priorsim_durations[4:8, ])  + 
  scale_y_log10(breaks=c(2, 10, 30, 100, 500, 2000, 5000), limit=c(1, 5000), name="Duration on log-axis") +
  theme(legend.position="none") +
  labs(title="Real and simulated data")

prior_overlay <- ppc_dens_overlay(raw_durations, priorsim_durations[1:100, ], alpha=0.5, size=0.7, adjust=1) +
  scale_x_log10(breaks=c(2, 5, 10, 30, 100, 500, 2000, 5000), limit=c(1, 5000), name="")

prior_overlay$scales$scales[[1]]$labels <- c("data", "prior")

prior_sim <- ((prior_box / prior_overlay) + plot_layout(guides="collect"))
ggsave("images/prior_simData.png", prior_sim, scale=1, width=2000, height=1400, units="px")
