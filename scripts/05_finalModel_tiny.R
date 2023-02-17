library(tidyverse)
source("cl_helper_functions.R")
library(brms)

data <- load_data('../data/consonant_data.csv', n = 30, outlier_mad = 3)

cl_max_nested <- 
  brm(data = data,
      family = lognormal(),
      formula = duration ~ 1 + initial + 
        (1 + initial + fbc | Language / (sound_class + speaker)) +
        z_logSpeechRate + z_logPhonWord + z_logWordFormFreq,
      prior = c(prior(normal(4.4, 0.2), class = Intercept),
                prior(exponential(12), class = sigma),
                prior(normal(0, 0.3), class = b),
                prior(exponential(12), class = sd),
                prior(lkj(5), class = cor)),    
      iter = 1000, warmup = 500, chains = 1, cores = 1,
      seed = 42,
      file = "models/cl_max_tiny"
  )
