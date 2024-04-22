install.packages('groundhog', repos='https://ftp.fau.de/cran/')
library('groundhog')
set.groundhog.folder('./R_groundhog/')
pkgs <- c('brms','viridis', 'readr', 'posterior', 'dplyr', 'ggplot2', 'ggdist',
          'gghalves', 'patchwork', 'bayesplot', 'tidybayes', 'xtable', 'tidyr',
          'ggrepel', 'rnaturalearth', 'rnaturalearthdata', , 'stringr',
          'geostan', 'geodist', 'gridExtra', 'extraDistr')
groundhog.library(pkgs, '2023-04-01', force.install=TRUE)
groundhog.library('github::stan-dev/cmdstanr', '2023-08-01', force.install=TRUE)

source('01_DataExplorations.R')
source('02_PriorDistributions.R')
source('03_PriorModel.R')

# The models and posterior predictions will take a long time to compile,
# feel free to run, or to use our provided models via OSF.
# source('04_FinalModel.R')
# source('05_ModelConvergence.R')
# source('test_moran.R)

source('06_BayesViz.R')
source('07_utils.R')
