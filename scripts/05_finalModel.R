library(dplyr)
library(readr)
library(brms)

data <- read_tsv('../data/res.tsv') %>% 
  mutate(sound_class = paste(voicing, sound_class),
         word_initial = as.factor(word_initial),
         utt_initial = as.factor(utt_initial))

cl_max <- 
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
      file="models/cl_max_final",
      backend="cmdstanr"
  )

cl_max <- 
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
      file="models/cl_max_priors",
      backend="cmdstanr"
  )

