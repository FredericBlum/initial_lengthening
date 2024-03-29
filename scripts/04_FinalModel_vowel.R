library(dplyr)
library(readr)
library(brms)
library(cmdstanr)

data <- read_tsv('vowels.tsv') %>% 
  mutate(word_initial = as.factor(word_initial),
         utt_initial = as.factor(utt_initial),
		 utt_final = as.factor(utt_final))

# If necessary, set path to specific cmdstan installation
# set_cmdstan_path(path="")
set_cmdstan_path(path="/data/tools/stan/cmdstan-2.32.2/")

cl_max <- 
  brm(data=data,
      family=Gamma("log"),
      formula=Duration ~ 1 + utt_initial + word_initial + utt_final +
        (1 + utt_initial + word_initial + utt_final| (Language / Speaker)) +
        z_num_phones + z_word_freq + z_speech_rate,
      prior=c(prior(normal(4.6, 0.1), class=Intercept),
              prior(normal(5, 0.5), class=shape),
              prior(normal(0, 0.3), class=b),
              prior(exponential(12), class=sd),
              prior(lkj(5), class=cor)),
      iter=5000, warmup=2500, chains=4, cores=4,
      control=list(adapt_delta=0.90, max_treedepth=10),
      seed=1,
      silent=0,
      file="models/cl_gamma_v",
      backend="cmdstanr"
  )