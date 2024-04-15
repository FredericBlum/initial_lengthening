library(dplyr)
library(readr)
library(stringr)
library(brms)
library(cmdstanr)
library(ape)


langs <- read_csv('languages.csv') %>% 
  select(Name, Macroarea, Latitude, Longitude, Glottocode, Family, phylo)
data <- read_tsv('data.tsv') %>% 
  left_join(langs, by = join_by(Language==Glottocode))


# Read in phylogenetic control
df_phylo <- read_rds("df-phylo.rds")
phylo <- vcv.phylo(df_phylo, corr=TRUE)


# If necessary, set path to specific cmdstan installation
set_cmdstan_path(path="/data/users/blum/tools/cmdstan-2.32.2-threaded/")

model <- 
  brm(data=data,
      family=Gamma("log"),
      formula=Duration ~ 1 + utt_initial + word_initial + cluster_status +
        (1 + utt_initial + word_initial + cluster_status | Language) +
        (1 | Speaker) + (1 | Family) +
        (1 + word_initial + utt_initial | CLTS) +
        z_num_phones + z_word_freq + z_speech_rate,
      prior=c(prior(normal(4.5, 0.1), class=Intercept),
              prior(normal(6, 0.5), class=shape),
              prior(normal(0, 0.3), class=b),
              prior(exponential(12), class=sd),
              prior(lkj(5), class=cor)
      ),
      iter=3000, warmup=1500, chains=4, cores=4,
      thread=threading(3),
      control=list(adapt_delta=0.85, max_treedepth=10),
      seed=1,
      silent=0,
      file="models/cl_var",
      backend="cmdstanr"
  )
