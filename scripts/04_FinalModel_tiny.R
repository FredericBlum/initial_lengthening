library(dplyr)
library(readr)
library(brms)

data <- read_tsv('data/res.tsv') %>% 
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
      iter=2000, warmup=1000, chains=4, cores=4,
      threads=threading(2),
      adapt_delta=0.85,
      seed=42,
      file="models/cl_tiny",
      backend="cmdstanr"
  )
