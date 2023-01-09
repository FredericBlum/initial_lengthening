library(tidyverse)
source("cl_helper_functions.R")
library(brms)

data <- load_data('../data/consonant_data.csv', filter_long = T,
                  n = 30, outlier_mad = 3)

cl_priors <- 
  brm(data = data,
      family = lognormal(),
      formula = duration ~ 1 + word_initial + utt_initial + 
        (1 + word_initial + utt_initial | Language / speaker) +
        z_logSpeechRate + z_logPhonWord + z_logWordFormFreq,
      prior = c(prior(normal(4.4, 0.2), class = Intercept),
                prior(exponential(12), class = sigma),
                prior(normal(0, 0.3), class = b),
                prior(exponential(12), class = sd),
                prior(lkj(5), class = cor)),   
      #iter = 16000, warmup = 2000, chains = 4, cores = 4,
      iter = 100, warmup = 20, chains = 4, cores = 4,
      seed = 42,
      sample_prior = "only",
      file = "../models/cl_priors"
  )

cl_max_nested <- 
  brm(data = data,
      family = lognormal(),
      formula = duration ~ 1 + word_initial + utt_initial + 
        (1 + word_initial + utt_initial | Language / speaker) +
        z_logSpeechRate + z_logPhonWord + z_logWordFormFreq,
      prior = c(prior(normal(4.4, 0.2), class = Intercept),
                prior(exponential(12), class = sigma),
                prior(normal(0, 0.3), class = b),
                prior(exponential(12), class = sd),
                prior(lkj(5), class = cor)),    
      iter = 16000, warmup = 2000, chains = 4, cores = 4,
      seed = 42,
      file = "../models/cl_max_large"
      )
