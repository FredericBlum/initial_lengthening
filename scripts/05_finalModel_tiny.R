library(tidyverse)
library(brms)

data <- read_tsv('../data/res.tsv') %>% 
  filter(duration>0.03) %>% 
  mutate(duration=duration*1000,
         word_form_freq = freq/count,
         word_initial = as.factor(word_initial),
         utt_initial = as.factor(utt_initial))

cl_max <- 
  brm(data=data,
      family=lognormal(),
      formula=duration ~ 1 + utt_initial + word_initial + 
        (1 + utt_initial + word_initial | Glottocode / (sound_class + Speaker)) +
        z_speech_rate + z_num_phones + log(word_form_freq),
      prior=c(prior(normal(4.4, 0.2), class=Intercept),
                prior(exponential(12), class=sigma),
                prior(normal(0, 0.3), class=b),
                prior(exponential(12), class=sd),
                prior(lkj(5), class=cor)),    
      iter=200, warmup=100, chains=2, cores=4,
      threads=threading(2),
      seed=42,
      file="models/cl_max_tiny",
      backend="cmdstanr"
  )
