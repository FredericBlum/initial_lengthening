library(tidyverse)
library(patchwork)
library(posterior)
library(bayesplot)
library(ggplot2)
library(brms)

color_scheme_set("pink")

data <- read_tsv('data.tsv') %>% 
  mutate(sound_class = paste(voicing, sound_class),
         word_initial = as.factor(word_initial),
         utt_initial = as.factor(utt_initial),
		 initial = ifelse(
			utt_initial==1, "utt", ifelse(
      		word_initial==1, "word", "other"
  )))

model <- readRDS(file="models/cl_final.rds")
model <- readRDS(file="models/cl_gamma.rds")

#########################################
###     model convergence             ###
#########################################
lp_max <- log_posterior(model)
np_max <- nuts_params(model)
posterior_max <- as.array(model)

# mcmc_pairs: up to 8 parameters, divergent transitions and collinearity between parameters
pred_pairs <- mcmc_pairs(posterior_max, np=np_max, pars=c('b_Intercept', 'sigma', 'b_z_word_freq', 'b_word_initial1'), 
                         off_diag_args=list(size=0.75))
diver_scatter <- mcmc_scatter(posterior_max, np=np_max, regex_pars=c('b_Intercept', 'sigma'))
ggsave('images/viz_pairsPred.png', pred_pairs, scale=1.3,
       width=3000, height=2800, units="px")

zipfs_pairs <- mcmc_pairs(posterior_max, np=np_max, regex_pars=c("^b_z_"), 
                         off_diag_args=list(size=0.75))

ggsave('images/viz_pairsPredz.png', zipfs_pairs, scale=1.3,
       width=3000, height=2800, units="px")

collinearity_pred <- as_draws_df(model) %>% 
  select(b_z_speech_rate:b_z_word_freq) %>% 
  cor()

print(paste(
  "The correlation between word form frequenzy and number of phones per word is",
  round(collinearity_pred[2,3], 2)
  ))

# trank plots for intercept and sigma
both_traces <- mcmc_trace(posterior_max, pars=c("b_Intercept", "sigma"), 
                          facet_args=list(ncol=2, strip.position="left")) +
  theme(legend.position="none")

both_ranks <- mcmc_rank_overlay(posterior_max, pars=c("b_Intercept", "sigma"), 
                                facet_args=list(ncol=2, strip.position="left")) +
  # coord_cartesian(ylim=c(0, 200)) + 
  theme(legend.position="none")

eval_chains <- (both_traces /  both_ranks)

ggsave('images/eval_chains.png', eval_chains, scale=1)

########################################
rhats_max <- brms::rhat(model)
rhat_all <- mcmc_rhat(rhats_max) + xlab("R-hat value") +
  scale_x_continuous(breaks=c(1.0, 1.005, 1.01), limits=c(1, 1.01)) +
  theme(legend.position="none")

rhat_val <- rhats_max[lapply(rhats_max, as.numeric) > 1.004]
rhat_filter <- mcmc_rhat(rhat_val) + yaxis_text(hjust=1) +
  scale_x_continuous(breaks=c(1, 1.005, 1.01), limits=c(1, 1.015)) +
  theme(legend.position="none")

###########################################
neff_max <- neff_ratio(model)
neff_all_plot <- mcmc_neff(neff_max, size=2) + 
  scale_x_continuous(breaks=c(0, 0.5, 1)) +
  xlab("Effective sample size")  +
  theme(legend.position="none")

neff_filter <- neff_max[lapply(neff_max, as.numeric) < 0.15]
neff_fil_plot <- mcmc_neff(neff_filter, size=2) + yaxis_text(hjust=1) + 
  scale_x_continuous(breaks=c(0.1, 0.2), limits=c(0, 0.2))  +
  theme(legend.position="none")

rhat_neff_plot <- (rhat_all + rhat_filter) / (neff_all_plot + neff_fil_plot)
ggsave('images/eval_rhat_neff.png', rhat_neff_plot, scale=1)

#########################################
###     posterior predictive checks   ###
#########################################
if (file.exists("models/pred_post.rds")) {
  sim_data <- readRDS(file="models/pred_post.rds")
} else{
  print("Sorry, the file does not yet exist. This may take some time.")
  sim_data <- posterior_predict(model, ndraws=4, cores=getOption("mc.cores", 4))
  saveRDS(sim_data, file="models/pred_post.rds")  
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
