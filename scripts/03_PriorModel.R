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


data <- read_tsv('data.tsv') %>% sample_frac(0.01)
languages <- read_csv('languages.csv')

# If necessary, set path to specific cmdstan installation
set_cmdstan_path(path="/data/users/blum/tools/cmdstan-2.32.2-threaded/")

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


# Visualising the prior checks
rope_high <-  0.01
rope_low <-  -0.01

para_vals <- posterior_summary(model) %>% data.frame() %>% as_tibble(rownames="Parameter")

hpdi_vals <- posterior_interval(model, prob=0.89) %>% data.frame() %>% as_tibble(rownames='Parameter') %>% 
  rename(hpdi_low=X5.5., hpdi_high=X94.5.)

para_vals <- para_vals %>% left_join(hpdi_vals)
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

prior_lang_all <- lang_params %>% filter(Parameter != "Intercept") %>% 
  mutate(Parameter=str_replace(Parameter, "_initial", "-")) %>% 
  ggplot(aes(x=Parameter, y=Estimate)) +
  geom_crossbar(aes(ymin=hpdi_low, ymax=hpdi_high, fill=Parameter), linewidth=0.5, width=0.5, fatten=0) + 
  geom_hline(yintercept=0, color="red", alpha=0.5, linewidth=0.5)+
  scale_fill_viridis(discrete =T, end=0.7) +
  scale_y_continuous(breaks=seq(from=-0.5, to=0.5, by=0.5)) +
  facet_wrap(~Language, ncol=5) +
  scale_x_discrete(name=NULL, labels=NULL)  +
  scale_alpha(guide="none") +
  theme(legend.position='bottom') + labs(fill="")

ggsave("images/prior_langAll.png", prior_lang_all, scale=1, width=2000, height=2000, units="px")


# Preiod predictive checks
raw_durations <- data %>% .$Duration

if (file.exists("models/pred_prior.rds")) {
  priorsim_durations <- readRDS(file="models/pred_prior.rds")
} else{
  print("Sorry, the file does not yet exist. This may take some time.")
  priorsim_durations <- posterior_predict(model, ndraws=100, cores=getOption("mc.cores", 4))
  saveRDS(priorsim_durations, file="models/pred_prior.rds")
} 

prior_box <- ppc_boxplot(raw_durations, priorsim_durations[4:8, ])  + 
  scale_y_log10(breaks=c(2, 10, 30, 100, 500, 2000, 5000), limit=c(1, 5000), name="Duration on log-axis") +
  theme(legend.position="none") +
  labs(title="Real and simulated data")

prior_overlay <- ppc_dens_overlay(raw_durations, priorsim_durations[1:100, ], alpha=0.5, size=0.7, adjust=1) +
  scale_x_log10(breaks=c(2, 5, 10, 30, 100, 500, 2000, 5000), limit=c(1, 5000), name="")

prior_overlay$scales$scales[[1]]$labels <- c("data", "prior")

prior_sim <- ((prior_box / prior_overlay) + plot_layout(guides="collect"))
ggsave("images/prior_simData.png", prior_sim, scale=1, width=2000, height=1400, units="px")
