library(dplyr)
library(readr)
library(stringr)
library(brms)
library(cmdstanr)
library(ape)


langs <- read_csv('languages.csv')
data <- read_tsv('data.tsv') %>% 
  mutate(sound_class = paste(voicing, sound_class),
         word_initial = as.factor(word_initial),
         utt_initial = as.factor(utt_initial)) %>% 
  left_join(langs, by = join_by(Language==ID))


# All data
df_phylo <- read_rds("df-phylo.rds")
phylo <- vcv.phylo(df_phylo, corr=TRUE)


# If necessary, set path to specific cmdstan installation
# set_cmdstan_path(path="")
set_cmdstan_path(path="/data/tools/stan/cmdstan-2.32.2/")

cl_max <- 
  brm(data=data,
      data2 = list(phylo=phylo),
      family=Gamma("log"),
      formula=Duration ~ 1 + utt_initial + word_initial + 
        gp(Longitude, Latitude, by=Macroarea, gr=TRUE) +
        (1 + utt_initial + word_initial | Language / Speaker) +
        (1 | gr(phylo, cov=phylo)) +
        CLTS + z_num_phones + z_word_freq + z_speech_rate,
      prior=c(prior(normal(4.5, 0.1), class=Intercept),
              prior(normal(6, 0.5), class=shape),
              prior(normal(0, 0.3), class=b),
              prior(exponential(12), class=sd),
              prior(lkj(5), class=cor),
              prior(exponential(2), class=sdgp)
      ),
      iter=3000, warmup=2000, chains=4, cores=4,
      control=list(adapt_delta=0.80, max_treedepth=10),
      seed=1,
      silent=0,
      file="models/cl_bias_large",
      backend="cmdstanr"
  )
