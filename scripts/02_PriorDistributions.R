library(readr)
library(dplyr)
library(ggplot2)
library(ggdist)
library(patchwork)
library(viridis)

set.seed(42)
n <- 1e5

#########################################
###     influence of predictors       ###
#########################################

predictors <- tibble(x=c(rnorm(n, 0, 0.3))) %>%
  mutate(group='beta%~% Normal(0, 0.3)') %>% 
  ggplot(aes(x=x)) +
  geom_density(aes(fill=group)) +
  scale_fill_viridis(discrete=T, alpha=0.7, end=0.7) +
  scale_y_continuous(breaks=NULL) +
  ylab("Density of values") +
  scale_x_continuous(
    name="Predictor values on log-scale",
    limits=c(-1.25, 1.25), 
    breaks=seq(from=-1, to=1, by=0.5)
    ) +
  theme(legend.position="none", plot.title=element_text(size=14)) +
  labs(title="β ~ Normal(0, 0.3)")

#########################################
###     Intercept priors              ###
#########################################
sample_ints <- tibble(x=c(exp(rnorm(n, mean=4.4, sd=0.05)))) %>%
  mutate(group='alpha%~% logn( Normal(4.5, 0.1), exp(12) )') %>% 
  ggplot(aes(fill=group)) +
  geom_density(aes(x=x)) +
  scale_x_log10(
    limits= c(15, 320),
    breaks=c(10, 20, 30, 50, 100, 200, 300),
    name="Prior distribution for the intercept"
    ) +
  scale_y_continuous(breaks=NULL, name="Density of values") +
  scale_fill_viridis(discrete=T, alpha=0.7, end=0.7) +
  theme(legend.position="none", plot.title=element_text(size=14)) +
  labs(title="α ~ Normal(4.4, 0.05)")

#########################################
###         sd_var                    ###
#########################################
sd_var <- rgamma(n, 3, 30) %>% 
  tibble() %>% 
  mutate(group='Gamma') %>% 
  ggplot(aes(x=.)) + 
  geom_density(aes(fill=group)) +
  scale_y_continuous(breaks=NULL, name="Density of values") +
  scale_x_continuous(name="Standard deviation of varying intercepts on log-scale") +
  scale_fill_viridis(discrete=T, alpha=0.7, end=0.7) +
  theme(legend.position="none", plot.title=element_text(size=14)) +  
  labs(title="γ ~ Gamma(2, 30)")

#########################################
###     varying slopes matrix         ###
#########################################
lkjcorr <- rlkjcorr_marginal(n, K=2, eta=5) %>% tibble(x=.) %>% 
  mutate(group='R%~% LKJcorr(5)') %>% 
  ggplot(aes(x=x, fill=group)) + 
  geom_density() +
  scale_y_continuous(breaks=NULL) +
  scale_x_continuous(name="Correlation of varying intercepts and slopes", breaks=c(-1, -0.5, 0, 0.5, 1)) +
  scale_fill_viridis(discrete=T, alpha=0.7, end=0.7) +
  theme(legend.position="none", plot.title=element_text(size=14)) +
  ylab("Density of values") +
  labs(title="R ~ LKJcorr(5)")

all_priors <- (sample_ints + predictors) / (sd_var + lkjcorr)
ggsave("images/prior_all.png", all_priors, scale=1, width=2500, height=2500, units='px')
