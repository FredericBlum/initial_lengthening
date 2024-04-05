library(dplyr)
library(readr)
library(stringr)
library(brms)
library(cmdstanr)
library(ape)


langs <- read_csv('languages.csv')
data <- read_tsv('data.tsv') %>% 
  mutate(
    word_initial = as.factor(word_initial),
    utt_initial = as.factor(utt_initial)) %>% 
  filter(InCluster==0) %>% 
  left_join(langs, by = join_by(Language==ID))


# Read in phylogenetic control
df_phylo <- read_rds("df-phylo.rds")
phylo <- vcv.phylo(df_phylo, corr=TRUE)


# If necessary, set path to specific cmdstan installation
set_cmdstan_path(path="/users/blum/tools/cmdstan-2.32.2-threaded/")

model <- 
  brm(data=data,
      family=Gamma("log"),
      formula=Duration ~ 1 + utt_initial + word_initial +
        (1 + utt_initial + word_initial | Language) +
        (1 | Speaker) + (1 | CLTS) +
        z_num_phones + z_word_freq + z_speech_rate,
      prior=c(prior(normal(4.5, 0.1), class=Intercept),
              prior(normal(6, 0.5), class=shape),
              prior(normal(0, 0.3), class=b),
              prior(exponential(12), class=sd),
              prior(lkj(5), class=cor)
      ),
      iter=3000, warmup=1000, chains=4, cores=4,
      thread=threading(3),
      control=list(adapt_delta=0.80, max_treedepth=10),
      seed=1,
      silent=0,
      file="models/cl_noCluster",
      backend="cmdstanr"
  )
