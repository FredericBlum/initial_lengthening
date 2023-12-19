install.packages("groundhog", repos="https://ftp.fau.de/cran/")
library("groundhog")
set.groundhog.folder("./R_groundhog/")
pkgs <- c("brms","viridis", "readr", "posterior", "dplyr", "ggplot2", "ggdist",
          "gghalves", "patchwork", "bayesplot", "tidybayes", "xtable", "ggrepel",
          "rnaturalearth", "rnaturalearthdata", "tidyr", "stringr")
groundhog.library(pkgs, "2023-12-03", force.install=TRUE)
groundhog.library('github::stan-dev/cmdstanr', "2023-08-01", force.install=TRUE)

source("01_DataExplorations.R")
source("02_PriorDistributions.R")
source("03_PriorModel.R")

# The models will take a long time to compile, feel free to run, or to use
# our provided models via OSF.
# source("04_FinalModel.R")

source("05_ModelConvergence.R")
source("06_BayesViz.R")
source("07_utils.R")
