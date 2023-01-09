library(bayesplot)
library(brms)
library(tidyverse)
library(tidybayes)
library(patchwork)
library(viridis)
library(xtable)
source("cl_helper_functions.R")


###################################################################
cl_max_two <- readRDS(file = "../models/cl_max_two_20.rds")

rope_high = 0.01
rope_low = -0.01

#########################################
###     Parameters                    ###
#########################################
hpdi_vals89 <- posterior_interval(cl_max_two, prob=0.89) %>% 
  data.frame() %>% as_tibble(rownames = "Parameter") %>% 
  rename(hpdi_89_low = X5.5., hpdi_89_high = X94.5.)

hpdi_vals99 <- posterior_interval(cl_max_two, prob=0.997) %>% 
  data.frame() %>% as_tibble(rownames = "Parameter") %>% 
  rename(hpdi_low = X0.15., hpdi_high = X99.85.)

para_vals <- posterior_summary(cl_max_two) %>% 
  data.frame() %>% as_tibble(rownames = "Parameter") %>% 
  left_join(hpdi_vals89) %>% left_join(hpdi_vals99) %>% 
  mutate(Parameter = str_replace(Parameter, "utt", "utterance-initial"),
         Parameter = str_replace(Parameter, "word", "word-initial"))

rm(hpdi_vals89, hpdi_vals99)

lang_params <- para_vals %>% 
  filter(grepl("^r_Language.*", Parameter)) %>% 
  mutate(Parameter = gsub("r_Language\\[(.*)(,-initial|,)(.*)]", "\\1__\\3", Parameter)) %>% 
  separate(sep = "__", col = Parameter, into = c("Language", "Parameter")) %>% 
  filter(Parameter != "Intercept") %>% 
  mutate(outside = ifelse(hpdi_89_high < rope_low, TRUE, 
                          ifelse(hpdi_89_low > rope_high, TRUE, FALSE)),
         outside_99 = ifelse(hpdi_high < rope_low, "yes", 
                             ifelse(hpdi_low > rope_high, "yes", "no")),
         Language = str_replace(Language, "\\.", " "))

langWord <- lang_params %>% filter(Parameter == "word-initial")
langUtt <- lang_params %>% filter(Parameter == "utterance-initial")

languages <- read_csv('./utils/languages.csv')

pop_level <- c("b_Initialutterance-initial", "b_Initialword-initial", 
               "b_z_logSpeechRate" , "b_z_logPhonWord",
               "b_z_logWordFormFreq", "b_ConCluster")

#########################################
###   Tables: Raw values              ###
#########################################
fixed_effects <- para_vals %>% filter(Parameter %in% pop_level) %>% 
  mutate(Estimate = round(Estimate, 2),
         Parameter = str_replace(Parameter, "b_Initial", ""),
         Parameter = str_replace(Parameter, "b_z_log", ""),
         Parameter = str_replace(Parameter, "b_ConCluster", "Consonant cluster"),
         Parameter = str_replace(Parameter, "PhonesWord", "phones per word"),
         Parameter = str_replace(Parameter, "WordFormFreq", "word-form frequency"), 
         "99.7% HPDI" = paste(format(round(hpdi_low, 2), nsmall = 2), "to", 
                              format(round(hpdi_high, 2), nsmall = 2))) %>%
  select(Parameter, Estimate, "99.7% HPDI") %>% 
  xtable(caption = "99.7% HPDI of the population-level predictors",
         label = "table: fixed_effects")
print(xtable(fixed_effects), include.rownames = FALSE)

lang_results <- lang_params %>% filter(outside == 1) %>% 
  mutate(Estimate = round(Estimate, 2),
         "89% HPDI" = paste(format(round(hpdi_89_low, 2), nsmall = 2), "to", 
                            format(round(hpdi_89_high, 2), nsmall = 2)),
         "99.7% HPDI" = paste(format(round(hpdi_low, 2), nsmall = 2), "to", 
                              format(round(hpdi_high, 2), nsmall = 2))) %>% 
  arrange(Language, Parameter) %>% 
  select(Language, Parameter, Estimate, "89% HPDI", "99.7% HPDI") %>% 
  xtable(caption = "Parameter values for languages with the 89% HPDI outside the ROPE",
         label = "table: slopes_lang")
print(xtable(lang_results), include.rownames = FALSE)

corr <- para_vals %>% filter(str_detect(para_vals$Parameter, "cor")) %>% 
  mutate(#Estimate = round(Estimate, 2),
    Parameter = str_replace(Parameter, "speaker", "Speaker"),
    "89% HPDI" = paste(format(round(hpdi_89_low, 2), nsmall = 2), "to", 
                       format(round(hpdi_89_high, 2), nsmall = 2))) %>% 
  select(Parameter, "89% HPDI") %>% 
  separate(sep = "__", col = Parameter, into = c("Level", "Parameter", "Parameter2")) %>% 
  mutate(Level = str_replace(Level, "cor_", ""),
         Parameter = str_replace(Parameter, "Initial", ""),
         Parameter2 = str_replace(Parameter2, "Initial", "")) %>% 
  pivot_wider(names_from = "Parameter2", values_from = "89% HPDI") 
print(xtable(corr), include.rownames = FALSE)


###################################################################
all_initial <- lang_params %>% 
  ggplot(aes(x = Parameter, y = Estimate)) +
  geom_crossbar(aes(ymin = hpdi_89_low, ymax = hpdi_89_high, fill = Parameter,
                    alpha = ifelse(outside == 1, 1, 0.1)), 
                size = 0.5, width = 0.5, fatten = 0) + 
  geom_errorbar(aes(ymin = hpdi_low, ymax = hpdi_high, width = 0.3)) +
  geom_hline(yintercept = 0, color = "red", alpha = 0.5, size = 0.5)+
  annotate("rect", ymin = rope_low, ymax = rope_high, xmin = 0, xmax = Inf, alpha = .5) +
  scale_fill_viridis(discrete  = T, begin = 0, end = 0.7) +
  facet_wrap(~Language) +
  scale_x_discrete(name = NULL, labels = NULL)  +
  scale_alpha(guide = "none") +
  theme(legend.position = 'bottom') + labs(fill = "")

ggsave("images/viz_langAll_seperate.png", all_initial, scale = 1,
       width = 2000, height = 2900, units = "px")

#########################################
###     Speaker parameters            ###
#########################################
lang_estimates <- lang_params %>% select(Language, Parameter, Estimate) %>% 
  mutate(Language = str_replace(Language, "Mojeño.Trinitario", "Mojeño Trinitario"))
