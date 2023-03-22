library(bayesplot)
library(brms)
library(patchwork)
library(tidyverse)
library(tidybayes)
library(viridis)

data <- read_csv('../data/consonant_data.csv') %>% 
  mutate(utt_initial = as.factor(utt_initial),
         word_initial = as.factor(word_initial))

cl_priors <- readRDS(file = "../models/cl_priors.rds")
color_scheme_set("pink")

para_vals <- posterior_summary(cl_priors) %>% 
  data.frame() %>% as_tibble(rownames = "parameter")

hpdi_vals <- posterior_interval(cl_priors, prob=0.89) %>% 
  data.frame() %>% as_tibble(rownames = "parameter") %>% 
  rename(hpdi_low = X5.5., hpdi_high = X94.5.)

para_vals <- para_vals %>% left_join(hpdi_vals)
lang_params <- para_vals %>% filter(grepl("^r_Language.*", para_vals$parameter)) %>% 
  mutate(parameter = gsub("r_Language\\[(.*)(,Initial|,)(.*)]", "\\1__\\3", parameter)) %>% 
  separate(sep = "__", col = parameter, into = c("Language", "parameter")) %>% 
  mutate(sd_min = Estimate - 3*Est.Error, 
         sd_max = Estimate + 3*Est.Error,
         parameter = str_replace(parameter, "UttInitial", "utterance-initial"),
         parameter = str_replace(parameter, "WordInitial", "word-initial"))

prior_lang_all <- lang_params %>% filter(parameter != "Intercept") %>% 
  mutate(parameter = str_replace(parameter, "_initial", "")) %>% 
  ggplot(aes(x = parameter, y = Estimate)) +
  geom_crossbar(aes(ymin = hpdi_low, ymax = hpdi_high, fill = parameter), 
                size = 0.5, width = 0.5, fatten = 0) + 
  geom_errorbar(aes(ymin = sd_min, ymax = sd_max, width = 0.3)) +
  geom_hline(yintercept = 0, color = "red", alpha = 0.5, size = 0.5)+
  scale_fill_viridis(discrete  = T, end = 0.7) +
  scale_y_continuous(breaks = seq(from = -0.5, to = 0.5, by = 0.5)) +
  facet_wrap(~Language, ncol = 5) +
  scale_x_discrete(name = NULL, labels = NULL)  +
  scale_alpha(guide = "none") +
  theme(legend.position = 'bottom') + labs(fill = "")

ggsave("images/prior_langAll.png", prior_lang_all, scale = 0.95,
       width = 2000, height = 1400, units = "px")

raw_durations <- data %>% .$duration

if (file.exists("../models/prior_pred.rds")) {
  priorsim_durations <- readRDS(file = "../models/prior_pred.rds")
} else{
  priorsim_durations <- posterior_predict(cl_priors, ndraws = 8, 
                                          cores = getOption("mc.cores", 4))
  saveRDS(priorsim_durations, file = "../models/prior_pred.rds")
} 

prior_box <- ppc_boxplot(raw_durations, priorsim_durations[4:8, ])  + 
  scale_y_log10(breaks = c(5, 10, 30, 80, 200, 500, 1000),
                limit = c(2, 1300),
                name = "Duration on log-axis") +
  theme(legend.position = "none") +
  labs(title = "Real and simulated data")

prior_overlay <- ppc_dens_overlay(raw_durations, priorsim_durations[1:8, ], 
                                  alpha = 0.5, size = 0.7, adjust = 1) +
  scale_x_log10(breaks = c(5, 10, 30, 80, 200, 500),
                limit = c(5, 1000),
                name = "Overall data on log-axis")

prior_overlay$scales$scales[[1]]$labels <- c("data", "prior")

prior_sim <- ((prior_box / prior_overlay) + plot_layout(guides = "collect"))
ggsave("images/prior_simData.png", prior_sim, scale = 1.2,
       width = 2000, height = 1400, units = "px")
