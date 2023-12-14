library(bayesplot)
library(brms)
library(patchwork)
library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(tidybayes)
library(viridis)

color_scheme_set("pink")

data <- read_tsv('data.tsv') %>% 
  mutate(sound_class = paste(voicing, sound_class),
         word_initial = as.factor(word_initial),
         utt_initial = as.factor(utt_initial))

# If necessary, set path to specific cmdstan installation
set_cmdstan_path(path = "/data/tools/stan/cmdstan-2.32.2/")
print(cmdstan_path())

cl_priors <- 
  brm(data=data,
      family=lognormal(),
      formula=Duration ~ 1 + utt_initial + word_initial + 
        (1 + utt_initial + word_initial | Language / (sound_class + Speaker)) +
        z_speech_rate + z_num_phones + z_word_freq,
      prior=c(prior(normal(4.4, 0.2), class=Intercept),
              prior(exponential(12), class=sigma),
              prior(normal(0, 0.3), class=b),
              prior(exponential(12), class=sd),
              prior(lkj(5), class=cor)),    
      iter=5000, warmup=2000, chains=2, cores=4,
      threads=threading(2),
      seed=42,
      sample_prior="only",
      file="models/cl_priors",
      backend="cmdstanr"
  )

para_vals <- posterior_summary(cl_priors) %>% 
  data.frame() %>% as_tibble(rownames="Parameter")

hpdi_vals <- posterior_interval(cl_priors, prob=0.89) %>% 
  data.frame() %>% as_tibble(rownames="Parameter") %>% 
  rename(hpdi_low=X5.5., hpdi_high=X94.5.)

para_vals <- para_vals %>% left_join(hpdi_vals)
lang_params <- para_vals %>% filter(grepl("^r_Language.*", para_vals$Parameter)) %>% 
  filter(grepl("^r_Language:sound_class\\[.*", Parameter)) %>% 
  mutate(
    Parameter=gsub("^r_Language:sound_class\\[(.*)(,-initial|,)(.*)]","\\1__\\3",Parameter),
    Parameter=str_replace(Parameter, "_initial1", "-initial"),
    Parameter=str_replace(Parameter, "__", "_")) %>% 
  separate(sep="_", col=Parameter, into=c("Language", "SoundClass", "Parameter")) %>% 
  filter(Parameter != "Intercept") %>% 
  mutate(sd_min=Estimate - 3*Est.Error, 
         sd_max=Estimate + 3*Est.Error,
         Parameter=str_replace(Parameter, "UttInitial", "utterance-initial"),
         Parameter=str_replace(Parameter, "WordInitial", "word-initial"))

prior_lang_all <- lang_params %>% filter(Parameter != "Intercept") %>% 
  mutate(Parameter=str_replace(Parameter, "_initial", "")) %>% 
  ggplot(aes(x=Parameter, y=Estimate)) +
  geom_crossbar(aes(ymin=hpdi_low, ymax=hpdi_high, fill=Parameter), 
                linewidth=0.5, width=0.5, fatten=0) + 
  geom_errorbar(aes(ymin=sd_min, ymax=sd_max, width=0.3)) +
  geom_hline(yintercept=0, color="red", alpha=0.5, linewidth=0.5)+
  scale_fill_viridis(discrete =T, end=0.7) +
  scale_y_continuous(breaks=seq(from=-0.5, to=0.5, by=0.5)) +
  facet_wrap(~Language, ncol=5) +
  scale_x_discrete(name=NULL, labels=NULL)  +
  scale_alpha(guide="none") +
  theme(legend.position='bottom') + labs(fill="")

ggsave("images/prior_langAll.png", prior_lang_all, scale=1,
       width=2000, height=2000, units="px")

raw_durations <- data %>% .$Duration

if (file.exists("models/prior_pred.rds")) {
  priorsim_durations <- readRDS(file="models/prior_pred.rds")
} else{
  priorsim_durations <- posterior_predict(cl_priors, ndraws=8, 
                                          cores=getOption("mc.cores", 4))
  saveRDS(priorsim_durations, file="models/prior_pred.rds")
} 

prior_box <- ppc_boxplot(raw_durations, priorsim_durations[4:8, ])  + 
  scale_y_log10(breaks=c(1, 5, 10, 30, 80, 200, 500, 2000),
                limit=c(1, 2000),
                name="Duration on log-axis") +
  theme(legend.position="none") +
  labs(title="Real and simulated data")

prior_overlay <- ppc_dens_overlay(raw_durations, priorsim_durations[1:8, ], 
                                  alpha=0.5, size=0.7, adjust=1) +
  scale_x_log10(breaks=c(1, 5, 10, 30, 80, 200, 500, 1000, 2000),
                limit=c(1, 2000),
                name="")

prior_overlay$scales$scales[[1]]$labels <- c("data", "prior")

prior_sim <- ((prior_box / prior_overlay) + plot_layout(guides="collect"))
ggsave("images/prior_simData.png", prior_sim, scale=1.2,
       width=2000, height=1400, units="px")
