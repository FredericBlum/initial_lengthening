library(tidyverse)
library(brms)

data <- read_csv('../data/consonant_data.csv')

cl_priors <- 
  brm(data = data,
      family = lognormal(),
      formula = duration ~ 1 + utt_initial + word_initial + 
        (1 + utt_initial + word_initial | Language / (sound_class + speaker)) +
        z_logSpeechRate + z_logPhonWord + z_logWordFormFreq,
      prior = c(prior(normal(4.4, 0.2), class = Intercept),
                prior(exponential(12), class = sigma),
                prior(normal(0, 0.3), class = b),
                prior(exponential(12), class = sd),
                prior(lkj(5), class = cor)),   
      iter = 5000, warmup = 2000, chains = 4, cores = 4,
      seed = 42,
      sample_prior = "only",
      file = "models/cl_priors"
  )

cl_max_nested <- 
  brm(data = data,
      family = lognormal(),
      formula = duration ~ 1 + utt_initial + word_initial + 
        (1 + utt_initial + word_initial | Language / (sound_class + speaker)) +
        z_logSpeechRate + z_logPhonWord + z_logWordFormFreq,
      prior = c(prior(normal(4.4, 0.2), class = Intercept),
                prior(exponential(12), class = sigma),
                prior(normal(0, 0.3), class = b),
                prior(exponential(12), class = sd),
                prior(lkj(5), class = cor)),    
     iter = 5000, warmup = 2000, chains = 4, cores = 4,
      seed = 42,
      file = "models/cl_max"
      )
