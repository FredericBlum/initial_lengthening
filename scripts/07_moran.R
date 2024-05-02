library(geostan)
library(geodist)
library(gridExtra)
library(ggplot2)
library(dplyr)
library(readr)
library(brms)

options(bitmapType="cairo")

langs <- read_csv('languages.csv') %>% 
  select(Macroarea, Latitude, Longitude, Glottocode)
data <- read_tsv('data.tsv') %>% 
  left_join(langs, by = join_by(Language==Glottocode)) %>% 
  rename(latitude=Latitude, longitude=Longitude) 

africa <- data %>% filter(Macroarea=='Africa')
australia <- data %>% filter(Macroarea=='Australia')
eurasia <- data %>% filter(Macroarea=='Eurasia')
north_america <- data %>% filter(Macroarea=='North America')
papunesia <- data %>% filter(Macroarea=='Papunesia')
south_america <- data %>% filter(Macroarea=='South America')

regions <- list(africa, australia, eurasia, north_america, papunesia, south_america)

for (region in regions) {
  name <- as.character(region[1, 14])
  region <- region %>% filter(word_initial==1)
  matrix <- geodist(region, measure='geodesic')
  
  row.names(matrix) <- region$Language
  colnames(matrix) <- region$Language
  
  plot <- moran_plot(region$Duration, matrix)
  ggsave(filename=paste0('images/viz_mcData_', name, '.png'), plot)
}


########################
# Load model and compute residuals
model <- readRDS(file="models/cl_speakerGamma.rds")

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
  rename(residual = res) %>%
  filter(word_initial==1)
head(comb_res)

africa <- comb_res %>% filter(Macroarea=='Africa')
australia <- comb_res %>% filter(Macroarea=='Australia')
eurasia <- comb_res %>% filter(Macroarea=='Eurasia')
north_america <- comb_res %>% filter(Macroarea=='North America')
papunesia <- comb_res %>% filter(Macroarea=='Papunesia')
south_america <- comb_res %>% filter(Macroarea=='South America')

regions <- list(africa, australia, eurasia, north_america, papunesia, south_america)

for (region in regions) {
  name <- as.character(region[1, 14])
  matrix <- geodist(region, measure='geodesic')
  
  row.names(matrix) <- region$Language
  colnames(matrix) <- region$Language
  
  plot <- moran_plot(region$Duration, matrix)
  ggsave(filename=paste0('images/viz_mcResiduals_', name, '.png'), plot)
}
