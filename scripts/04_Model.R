#library(dplyr)
library(readr)
#library(stringr)
library(brms)
#library(cmdstanr)
library(chkptstanr)


data <- read_tsv('data.tsv')

# If necessary, set path to specific cmdstan installation
# set_cmdstan_path(path="/data/users/blum/tools/cmdstan-2.32.2-threaded/")
cores <-  parallel::detectCores()
chains <- 4

model <- chkpt_brms(
  formula=bf(formula=Duration ~ 1 + utt_initial + word_initial + cluster_status +
    (1 + utt_initial + word_initial + cluster_status | Language) +
    (1 + utt_initial + word_initial | Speaker) +
    (1 + utt_initial + word_initial | CLTS) +
    (1 | Family) +
    z_num_phones + z_word_freq + z_speech_rate,
    family=Gamma("log")),
  data=data,
  path="chkpt_folder/",
  prior=c(
    prior(normal(4.4, 0.05), class=Intercept),
    prior(normal(6, 0.5), class=shape),
    prior(normal(0, 0.3), class=b),
    prior(gamma(3, 30), class=sd),
    prior(lkj(5), class=cor)
  ),
  iter_sampling=5000,
  iter_warmup=3000,
  iter_per_chkpt=200,
  brmsfit=TRUE,
  parallel_chains=4, cores=cores,
  threads_per=cores/chains,
  control=list(adapt_delta=0.90, max_treedepth=10),
  seed=1,
  silent=0,
  file="models/cl_max_new"
)

# model <- 
#   brm(data=data,
#       family=Gamma("log"),
#       formula=Duration ~ 1 + utt_initial + word_initial + cluster_status +
#         (1 + utt_initial + word_initial + cluster_status | Language) +
#         (1 + utt_initial + word_initial | Speaker) +
#         (1 + utt_initial + word_initial | CLTS) +
#         (1 | Family) +
#         z_num_phones + z_word_freq + z_speech_rate,
#       prior=c(
#         prior(normal(4.4, 0.05), class=Intercept),
#         prior(normal(6, 0.5), class=shape),
#         prior(normal(0, 0.3), class=b),
#         prior(gamma(3, 30), class=sd),
#         prior(lkj(5), class=cor)
#       ),
#       iter=8000, warmup=3000, chains=4, cores=4,
#       threads=threading(18),
#       control=list(adapt_delta=0.90, max_treedepth=10),
#       seed=1,
#       silent=0,
#       #file="models/cl_max",
#       backend="cmdstanr"
#   )
