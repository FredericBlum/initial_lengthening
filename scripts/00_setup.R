install.packages("groundhog", repos="https://ftp.fau.de/cran/")
library("groundhog")
set.groundhog.folder("./R_groundhog/")
pkgs <- c("brms","viridis", "readr", "posterior", "dplyr", "ggplot2", "ggdist",
          "gghalves", "patchwork", "bayesplot", "tidybayes", "xtable", 
          "ggrepel", "rnaturalearth", "rnaturalearthdata")
groundhog.library(pkgs, "2023-12-03", force.install=TRUE)


source("01_DataExplorations.R")
source("02_PriorDistributions.R")
source("03_PriorModel.R")

# The models will take a long time to compile, feel free to run, or to use
# our provided models via OSF.
# source("04_FinalModel_tiny.R")
# source("04_FinalModel.R")


source("05_ModelConvergence.R")
source("06_BayesViz.R")
source("utils.R")
