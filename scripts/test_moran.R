library(geostan)
library(ggplot2)
library(gridExtra)
library(geodist)
library(dplyr)
library(readr)
library(brms)


languages <- read_csv('languages.csv')

data <- read_tsv('data.tsv') %>% 
  mutate(
    word_initial = as.factor(word_initial),
    utt_initial = as.factor(utt_initial)
  ) %>% 
  left_join(languages, by = join_by(Language==ID)) %>%
  select(Duration, Language, Latitude, Longitude)


matrix <- geodist(data, measure='geodesic')

row.names(matrix) <- data$Language
colnames(matrix) <- data$Language

png(filename='images/viz_mcData.png')
plot <- moran_plot(data$Duration, matrix)
plot
dev.off()

########################
# Load model and compute residuals
model <- readRDS(file="models/cl_bias_clusterMulti_parallel.rds")

resName <- 'model/res.rds'
if (file.exists(resName)) {
  res <- readRDS(file=resName)
} else{
  print("Sorry, the file does not yet exist. This may take some time.")
  res <- predictive_error(model, new_data=data, ndraws=1)
  saveRDS(res, file=resName)
}

# Combine data and residuals
comb_res <- cbind(data, res) %>% 
  rename(residual = res)
head(comb_res)

# Create Matrix
row.names(matrix) <- comb_res$Language
colnames(matrix) <- comb_res$Language

# Create plot
png(filename='images/viz_mcData.png')
plot <- moran_plot(comb_res$residual, matrix)
plot
dev.off()
