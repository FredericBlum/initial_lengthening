library(geostan)
library(ggplot2)
library(gridExtra)
library(geodist)
library(dplyr)
library(brms)

languages <- read_csv('languages.csv')

data <- read_tsv('data.tsv') %>% 
  mutate(
    word_initial = as.factor(word_initial),
    utt_initial = as.factor(utt_initial)
  ) %>% 
  left_join(languages, by = join_by(Language==ID))


# SAC with raw data: -0.002, no autocorrelation
# test <- data %>% filter(word_initial=='1') %>% 
#   select(Duration, Language, Latitude, Longitude) %>% 
#   sample_frac(.01)


test <- lang_params %>% filter(Parameter=='word-initial') %>% 
  select(Estimate, Language, Latitude, Longitude)

matrix <- geodist(test, measure='geodesic')

row.names(matrix) <- test$Language
colnames(matrix) <- test$Language

plot <- moran_plot(test$Estimate, matrix)

ggsave('mc_wordinitial.png', plot)



########################
library(sf)
library(sfheaders)


# Load model and compute residuals
# To-Do: Re-run for new model with all sound classes and full draw!
model <- readRDS(file="models/cl_gamma.rds")

reduced_data <- data %>% 
  filter(sound_class %in% c('fricative', 'stop')) %>% 
  sample_frac(0.001)

res <- predictive_error(model, new_data=reduced_data, ndraws=1)
beep(2)
library(beepr)

# Need to fix how to combine the resulting stuff so that I can actually
# test the residuals

# rownames(data) <- NULL
r_data <- cbind(reduced_data, res)

# then proceed as above

