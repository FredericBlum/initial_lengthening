library(readr)
library(dplyr)
library(patchwork)
library(posterior)
library(bayesplot)
library(ggplot2)
library(viridis)
library(tidybayes)
library(brms)

draws <- 6e3
options(bitmapType="cairo")

color_scheme_set("purple")

data <- read_tsv('data.tsv') %>% 
  mutate(
    initial=as.factor(ifelse(
      utt_initial==1, "utterance-initial", ifelse(
        word_initial==1, "word-initial", "other"
      ))
  ))

model <- readRDS(file="models/cl_max.rds")
languages <- unique(data$Language)


#########################################
###     model convergence             ###
#########################################
lp_max <- log_posterior(model)
np_max <- nuts_params(model)
posterior_max <- as.array(model)

# mcmc_pairs: up to 8 parameters, divergent transitions and collinearity between parameters
pred_pairs <- mcmc_pairs(posterior_max, np=np_max,
                         pars=c(
                           'b_Intercept', 'b_z_word_freq', 'b_z_num_phones',
                           'b_word_initial', 'b_utt_initial', 'shape'
                           ),
                         off_diag_args=list(size=0.75))

zipfs_pairs <- mcmc_pairs(posterior_max, np=np_max, regex_pars=c("^b_z_"),
                         off_diag_args=list(size=0.75))

# ggsave('images/viz_pairsPred.pdf', pred_pairs, scale=1.3, width=3000, height=2800, units="px")
# ggsave('images/viz_pairsPredz.pdf', zipfs_pairs, scale=1.3, width=3000, height=2800, units="px")

# Plot divergent transitions for two parameters
diver_scatter <- mcmc_scatter(posterior_max, np=np_max, pars=c('b_Intercept', 'shape'))

collinearity_pred <- as_draws_df(model) %>%
  select(b_z_num_phones:b_z_word_freq) %>%
  cor()

print(paste(
  "The correlation between word form frequenzy and number of phones per word is",
  round(collinearity_pred[1,2], 2)
  ))

# trank plots for intercept and sigma
both_traces <- mcmc_trace(posterior_max, pars=c("b_Intercept", "shape"),
                          facet_args=list(ncol=2, strip.position="left")) +
  theme(legend.position="none")

both_ranks <- mcmc_rank_overlay(posterior_max, pars=c("b_Intercept", "shape"),
                                facet_args=list(ncol=2, strip.position="left")) +
  # coord_cartesian(ylim=c(0, 200)) +
  theme(legend.position="none")

eval_chains <- (both_traces /  both_ranks)

ggsave('images/eval_chains.png', eval_chains, scale=1)

########################################
rhats_max <- brms::rhat(model)
rhat_all <- mcmc_rhat(rhats_max) + xlab("R-hat value") +
  scale_x_continuous(breaks=c(1.0, 1.05, 1.01), limits=c(1, 1.01)) +
  theme(legend.position="none")

rhat_val <- rhats_max[lapply(rhats_max, as.numeric) > 1.025]
rhat_filter <- mcmc_rhat(rhat_val) + yaxis_text(hjust=1) +
  scale_x_continuous(breaks=c(1, 1.01, 1.02), limits=c(1, 1.027)) +
  theme(legend.position="none")

###########################################
neff_max <- neff_ratio(model)
neff_all_plot <- mcmc_neff(neff_max, size=2) +
  scale_x_continuous(breaks=c(0, 0.5, 1)) +
  xlab("Effective sample size")  +
  theme(legend.position="none")

neff_filter <- neff_max[lapply(neff_max, as.numeric) < 0.05]
neff_fil_plot <- mcmc_neff(neff_filter, size=2) + yaxis_text(hjust=1) +
  scale_x_continuous(breaks=c(0.05, 0.1), limits=c(0, 0.12))  +
  theme(legend.position="none")

rhat_neff_plot <- (rhat_all + rhat_filter) / (neff_all_plot + neff_fil_plot)
ggsave('images/eval_rhat_neff.png', rhat_neff_plot, scale=1)

#########################################
# Run model predictions
#########################################
### Using new data

new_data <- tibble(
  Language='NewLang',
  Family='NewFam',
  Speaker=sample(c('1', '2', '3'), 10000, replace=TRUE),
  word_initial=sample(c(0, 1), 10000, replace=TRUE),
  utt_initial=sample(c(0, 1), 10000, replace=TRUE),
  CLTS=sample(unique(data$CLTS), 10000, replace=TRUE),
  cluster_status=sample(unique(data$cluster_status), 10000, replace=TRUE),
  z_num_phones=rnorm(10000),
  z_word_freq=rnorm(10000),
  z_speech_rate=rnorm(10000)
) %>% mutate(initial=as.factor(ifelse(utt_initial==1, "utterance-initial", ifelse(
    word_initial==1, "word-initial", "other"
  ))))

### Using epreds
if (file.exists("models/pred_new.rds")) {
  new_epreds <- readRDS(file="models/pred_new.rds")
} else{
  print("Sorry, the file does not yet exist. This may take some time.")
  new_epreds <- new_data %>% 
    add_epred_draws(model, ndraws=draws, allow_new_levels=TRUE, seed=42) %>%
    ungroup() %>% select(Language, utt_initial, word_initial, initial, .epred)
  saveRDS(new_epreds, file="models/pred_new.rds")  
}

avg <- new_epreds %>% group_by(initial) %>% summarise(mean=mean(.epred))
print('#############################')
print("Average for expected draws:")
print(avg)
print('#############################')

new_epreds %>% group_by(initial) %>% mutate(mean=mean(.epred))
  
plot_newdata <- new_epreds %>% 
  ggplot(aes(y=.epred, x=initial)) +
  geom_violin(aes(fill=initial)) +
  geom_boxplot(width=0.5, outlier.size=0, outlier.color="black", outlier.alpha=0.3) +
  scale_fill_viridis(discrete=TRUE, end=0.7) +
  scale_y_log10(limits=c(25, 300), breaks=c(30, 50, 100, 170, 300), name="duration in ms (axis is log-scaled)") +
  scale_x_discrete(label=NULL, name=NULL) +
  theme_grey(base_size=11) +
  annotate('text', x=1, y=95, label='92.7ms') +
  annotate('text', x=2, y=95, label="92.4ms") +
  annotate('text', x=3, y=109, label="106ms") +
  theme(legend.position='bottom', legend.title=element_blank())

ggsave(plot_newdata, filename='images/viz_post_newdata.pdf', 
       width=2000, height=1500, units="px")

# #########################################
# # Using posterior_draws from tidybayes
# if (file.exists("models/pred_predicted.rds")) {
#  m_preds <- readRDS(file="models/pred_predicted.rds")
# } else{
#  print("Sorry, the file does not yet exist. This may take some time.")
#  # Loop through languages for posterior prediction
#  m_preds <- tibble()
#  for (lang in languages){
#  	  subdata <- data %>% filter(Language==lang)
#  	  
#  	  sub_preds <- subdata %>% add_predicted_draws(model, draws=draws) %>%
#  	    ungroup() %>% select(ID, Language, Duration, utt_initial, word_initial, initial, .prediction)
#  	  m_preds <- rbind(m_preds, sub_preds)
#  }
#  saveRDS(m_preds, file="models/pred_predicted.rds")
# }
# 
# avg <- m_preds %>% group_by(initial) %>% summarise(mean=mean(.prediction))
# print('#############################')
# print("Average for predicted draws:")
# print(avg)
# print('#############################')
# 
# 
# plot_preds <- m_preds %>%
#  ggplot(aes(y=.prediction, x=initial)) +
#  geom_violin(aes(fill=initial)) +
#  geom_boxplot(width=0.5,
#               outlier.size=1, outlier.color="black", outlier.alpha=0.3) +
#  # If you want to plot the distribution across all languages, uncomment the
#  # following line and set ncol=n according to your needs.
#  # facet_wrap(~Language, ncol=4) +
#  scale_fill_viridis(discrete=TRUE, end=0.7) +
#  scale_y_log10(limits=c(5, 500), breaks=c(10, 20, 30, 70, 150, 300, 500),
#                name="duration on log-axis") +
#  scale_x_discrete(label=NULL, name=NULL) +
#  theme_grey(base_size=11) +
#  theme(legend.position='bottom', legend.title=element_blank())
# 
# # ggsave(plot_preds, filename='images/viz_post_predicted.pdf',
# #       width=1600, height=2300, units="px")
# 
# # Using fitted
# if (file.exists("models/pred_fitted.rds")) {
#   m_fitted <- readRDS(file="models/pred_fitted.rds")
# } else{
#   print("Sorry, the file does not yet exist. This may take some time.")
#   
#   m_fitted <- tibble()
#   for (lang in languages){
#     subdata <- data %>% filter(Language==lang)
#     sub_fit <- fitted(model, summary=TRUE, newdata=subdata, ndraws=draws)
#     sub_bind <- cbind(subdata, sub_fit)
#     m_fitted <- rbind(m_fitted, sub_bind)
#   }
#   saveRDS(m_fitted, file="models/pred_fitted.rds")  
# }
# 
# avg <- m_fitted %>% group_by(initial) %>% summarise(mean=mean(.Estimate))
# print('#############################')
# print("Average for fitted draws:")
# print(avg)
# print('#############################')
# 
# 
# plot_fit <- comb %>% 
#   ggplot(aes(y=Estimate, x=initial)) +
#   geom_violin(aes(fill=initial)) +
#   geom_boxplot(width=0.5, 
#                outlier.size=1, outlier.color="black", outlier.alpha=0.3) +
#   # If you want to plot the distribution across all languages, uncomment the
#   # following line and set ncol=n according to your needs.
#   # facet_wrap(~Language, ncol=4) +
#   scale_fill_viridis(discrete=TRUE, end=0.7) +
#   scale_y_log10(limits=c(5, 500), breaks=c(10, 20, 30, 70, 150, 300, 500), 
#                 name="duration on log-axis") +
#   scale_x_discrete(label=NULL, name=NULL) +
#   theme_grey(base_size=11) +
#   theme(legend.position='bottom', legend.title=element_blank())
# 
# # ggsave(plot_fit, filename='images/viz_post_fit.pdf', 
# #        width=1600, height=2300, units="px")
# 
# 
# if (file.exists("models/pred_post.rds")) {
#   m_post <- readRDS(file="models/pred_post.rds")
# } else{
#   print("Sorry, the file does not yet exist. This may take some time.")
#   m_post <- tibble()
#   for (lang in languages){
#     subdata <- data %>% filter(Language==lang)
#     sub_post <- posterior_predict(model, newdata=subdata, ndraws=draws, cores=getOption("mc.cores", 4))
#     m_post <- rbind(m_post, sub_post)
#   }
#   saveRDS(m_post, file="models/pred_post.rds")  
# }
# 
# avg <- m_post %>% group_by(initial) %>% summarise(mean=mean(.Estimate))
# print('#############################')
# print("Average for posterior predicted draws:")
# print(avg)
# print('#############################')


# #########################################
# # Using posterior_predict for standard checks
if (file.exists("models/pred_sim.rds")) {
  sim_data <- readRDS(file="models/pred_sim.rds")
} else{
  print("Sorry, the file does not yet exist. This may take some time.")
  sim_data <- posterior_predict(model, ndraws=8, cores=getOption("mc.cores", 4))
  saveRDS(sim_data, file="models/pred_sim.rds")
} 

duration_vals <- data %>% pull(Duration)
group_init <- data %>% pull(initial)

box_comp <- ppc_boxplot(duration_vals, sim_data[1:4, ])  +
  coord_cartesian(ylim=c(5, 800), expand=F) +
  scale_y_log10(breaks=c(10, 30, 100, 300, 750)) +
  coord_cartesian(xlim=c(0.99, 6)) +
  #theme(legend.position='bottom') +
  ylab("Duration on log-axis") +
  ggtitle("A: Overall comparison with different simulation runs")

violin_comp <- ppc_violin_grouped(duration_vals, sim_data[1:4,], group_init,
                                  alpha=1, y_draw="violin") +
  coord_cartesian(ylim=c(5, 800), expand=F) +
  scale_y_log10(breaks=c(10, 30, 100, 300, 750)) +
  scale_x_discrete(labels=c("non-initial", "utterance-initial", "word-initial")) +
  coord_cartesian(xlim=c(0.99, 3)) +
  theme(legend.position = "none") +
  ylab("Duration on log-axis") +
  ggtitle("B: Grouped comparison with all simulated data")

# box_comp$scales$scales[[1]]$labels <- c("data", "simulated")
# box_comp$scales$scales[[2]]$labels <- c("data", "simulated")

ppc_sum <- (box_comp) / (violin_comp) + plot_layout(guides="collect")
ggsave('images/eval_ppcsum.png', ppc_sum, width=2000, height=1300, units="px")
