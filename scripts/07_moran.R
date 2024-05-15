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

regions <- unique(data$Macroarea)

for (region in regions) {
  name <- as.character(region)
  subdata <- data %>% filter(Macroarea==region, word_initial==1)
  matrix <- geodist(subdata, measure='geodesic')

  row.names(matrix) <- region$Language
  colnames(matrix) <- region$Language

  plot <- moran_plot(subdata$Duration, matrix)
  ggsave(filename=paste0('images/viz_mcData_', name, '.png'), plot)
}

########################
# Load model and compute residuals
# model <- readRDS(file="models/cl_max.rds")
# 
# resName <- 'model/residual_error.rds'
# if (file.exists(resName)) {
#   res <- readRDS(file=resName)
# } else{
#   print("Sorry, the file does not yet exist. This may take some time.")
#   res <- tibble()
#   for (region in regions){
#     subdata <- data %>% filter(Macroarea==region)
#     sub_res <- predictive_error(model, new_data=subdata, ndraws=100)
#     res <- rbind(res, sub_res)
#   }
#   saveRDS(res, file="models/residual_error.rds")  
# }
# 
# # Combine data and residuals
# comb_res <- cbind(data, res)
# 
# for (region in regions) {
#   sub_res <- comb_res %>% filter(Macroarea==region, word_initial==1)
#   name <- as.character(region)
#   matrix <- geodist(sub_res, measure='geodesic')
#   
#   row.names(matrix) <- sub_res$Language
#   colnames(matrix) <- sub_res$Language
#   
#   plot <- moran_plot(sub_res$res, matrix)
#   ggsave(filename=paste0('images/viz_mcResiduals_', name, '.png'), plot)
# }
