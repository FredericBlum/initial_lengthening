library(dplyr)
library(readr)
library(stringr)
library(extraDist)
library(brms)
library(cmdstanr)


data <- read_tsv('data.tsv')

# If necessary, set path to specific cmdstan installation
set_cmdstan_path(path="/data/users/blum/tools/cmdstan-2.32.2-threaded/")

model <- 
  brm(data=data,
      family=Gamma("log"),
      formula=Duration ~ 1 + utt_initial + word_initial + cluster_status +
        (1 + utt_initial + word_initial + cluster_status | Language) +
        (1 + utt_initial + word_initial | Speaker) +
        (1 + utt_initial + word_initial | CLTS) +
        (1 | Family) +
        z_num_phones + z_word_freq + z_speech_rate,
      prior=c(prior(normal(4.5, 0.1), class=Intercept),
              prior(normal(6, 0.5), class=shape),
              prior(normal(0, 0.3), class=b),
              prior(exponential(15), class=sd),
              prior(lkj(5), class=cor)
      ),
      iter=4000, warmup=2500, chains=4, cores=4,
      threads=threading(3),
      control=list(adapt_delta=0.90, max_treedepth=10),
      seed=1,
      silent=0,
      file="models/cl_speaker",
      backend="cmdstanr"
  )
