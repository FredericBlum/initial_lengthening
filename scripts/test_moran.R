library(geostan)
library(ggplot2)
library(gridExtra)
library(geodist)
library(dplyr)
library(readr)
library(brms)

size <- 0.001

languages <- read_csv('languages.csv')

data <- read_tsv('data.tsv') %>% 
  mutate(
    word_initial = as.factor(word_initial),
    utt_initial = as.factor(utt_initial)
  ) %>% 
  left_join(languages, by = join_by(Language==ID)) %>%
  select(Duration, Language, Latitude, Longitude) %>% 
  sample_frac(size)


matrix <- geodist(data, measure='geodesic')

row.names(matrix) <- data$Language
colnames(matrix) <- data$Language

plot <- moran_plot(data$Duration, matrix)

ggsave('mc_data.png', plot)


########################
library(sf)
library(sfheaders)


# Load model and compute residuals
# To-Do: Re-run for new model with all sound classes and full draw!
model <- readRDS(file="models/cl_gamma.rds")

res <- predictive_error(model, new_data=data, ndraws=4)
beep(2)
library(beepr)

# Need to fix how to combine the resulting stuff so that I can actually
# test the residuals

# rownames(data) <- NULL
r_data <- cbind(reduced_data, res)

# then proceed as above

