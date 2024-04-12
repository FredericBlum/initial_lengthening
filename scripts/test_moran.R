library(geostan)
library(geodist)
library(gridExtra)
library(ggplot2)
library(dplyr)
library(readr)
library(brms)


langs <- read_csv('languages.csv') %>% 
  select(Name, Macroarea, Latitude, Longitude, Glottocode, Family, phylo)
data <- read_tsv('data.tsv') %>% 
  left_join(langs, by = join_by(Language==Glottocode))

africa <- data %>% filter(Macroarea=='Africa')
australia <- data %>% filter(Macroarea=='Australia')
eurasia <- data %>% filter(Macroarea=='Eurasia')
north_america <- data %>% filter(Macroarea=='North America')
papunesia <- data %>% filter(Macroarea=='Papunesia')
south_america <- data %>% filter(Macroarea=='South America')

plot_data <- africa


matrix <- geodist(plot_data, measure='geodesic')

row.names(matrix) <- plot_data$Language
colnames(matrix) <- plot_data$Language

png(filename='images/viz_mcData_africa.png')
plot <- moran_plot(plot_data$Duration, matrix)
plot
dev.off()

########################
# Load model and compute residuals
model <- readRDS(file="models/cl_noCluster")

resName <- 'model/res.rds'
if (file.exists(resName)) {
  res <- readRDS(file=resName)
} else{
  print("Sorry, the file does not yet exist. This may take some time.")
  res <- predictive_error(model, new_data=data)
  saveRDS(res, file=resName)
}

# Combine data and residuals
comb_res <- cbind(data, res) %>% 
  rename(residual = res)
head(comb_res)

africa <- comb_res %>% filter(Macroarea=='Africa')
australia <- comb_res %>% filter(Macroarea=='Australia')
eurasia <- comb_res %>% filter(Macroarea=='Eurasia')
north_america <- comb_res %>% filter(Macroarea=='North America')
papunesia <- comb_res %>% filter(Macroarea=='Papunesia')
south_america <- comb_res %>% filter(Macroarea=='South America')

plot_residuals <- africa
# Create Matrix
matrix <- geodist(plot_residuals, measure='geodesic')
row.names(matrix) <- plot_residuals$Language
colnames(matrix) <- plot_residuals$Language

# Create plot
png(filename='images/viz_mcResiduals_africa.png')
plot <- moran_plot(plot_residuals$residual, matrix)
plot
dev.off()
