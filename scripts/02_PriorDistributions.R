library(readr)
library(dplyr)
library(ggplot2)
library(ggdist)
library(patchwork)
library(viridis)

n=1e5
set.seed(42)

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
  scale_x_continuous(name="Predictor values on log-scale", 
                     limits=c(-1.25, 1.25), 
                     breaks=seq(from=-1, to=1, by=0.5)) +
  theme(legend.position="none",
        plot.title=element_text(size=14)) +
  labs(title="β ~ Normal(0, 0.3)")

#########################################
###     Intercept priors              ###
#########################################

# values for intercept
int_vals <- c(rnorm(n, mean=4.5, sd=0.1)) 
int_vals %>% tibble() %>% ggplot(aes(x=.)) + geom_density()

# sigma for intercept
sigma1 <- rexp(n, rate=15)
sigma1 %>% tibble() %>% ggplot(aes(x=.))+ geom_density()

# combination of both
sample_ints <- tibble(x=c(rgamma(n, 
                                  meanlog=int_vals, 
                                  sdlog=sigma1))) %>%
  mutate(group='alpha%~% logn( Normal(4.5, 0.1), exp(12) )') %>% 
  ggplot(aes(fill=group)) +
  geom_density(aes(x=x)) +
  scale_x_log10(limits= c(15, 320),
                breaks=c(10, 20, 30, 50, 100, 200, 300),
                name="Prior distribution for the intercept") +
  scale_y_continuous(breaks=NULL,
                     name="Density of values") +
  scale_fill_viridis(discrete=T, alpha=0.7, end=0.7) +
  theme(legend.position="none",
        plot.title=element_text(size=14)) +
  labs(title="α ~ logn(Normal(4.5, 0.1), Exp(12))")

#########################################
###         sigma2                    ###
#########################################
sigma2 <- rexp(n, rate=15) %>% 
  tibble() %>% 
  mutate(group='sigma%~% exp(12)') %>% 
  ggplot(aes(x=.)) + 
  geom_density(aes(fill=group)) +
  scale_y_continuous(breaks=NULL,
                     name="Density of values") +
  scale_x_continuous(breaks=seq(from=0, to=1.2, by=0.2),
                     #limits=c(0, 1.2),
                     name="Standard deviation of varying intercepts on log-scale") +
  scale_fill_viridis(discrete=T, alpha=0.7, end=0.7) +
  theme(legend.position="none",
        plot.title=element_text(size=14)) +  
  labs(title="σ ~ Exp(12)")

rgamma(n, 3, 30) %>% 
  tibble() %>% 
  mutate(group='sigma%~% exp(12)') %>% 
  ggplot(aes(x=.)) + 
  geom_density(aes(fill=group)) +
  scale_y_continuous(breaks=NULL,
                     name="Density of values") +
  scale_x_continuous(breaks=seq(from=0, to=1.2, by=0.2),
                     #limits=c(0, 1.2),
                     name="Standard deviation of varying intercepts on log-scale") +
  scale_fill_viridis(discrete=T, alpha=0.7, end=0.7) +
  theme(legend.position="none",
        plot.title=element_text(size=14)) +  
  labs(title="σ ~ Exp(12)")

#########################################
###     varying slopes matrix         ###
#########################################

lkjcorr <- rlkjcorr_marginal(n, K=2, eta=5) %>% tibble(x=.) %>% 
  mutate(group='R%~% LKJcorr(5)') %>% 
  ggplot(aes(x=x, fill=group)) + 
  geom_density() +
  scale_y_continuous(breaks=NULL) +
  scale_x_continuous(name="Correlation of varying intercepts and slopes",
                     breaks=c(-1, -0.5, 0, 0.5, 1)) +
  scale_fill_viridis(discrete=T, alpha=0.7, end=0.7) +
  theme(legend.position="none",
        plot.title=element_text(size=14)) +
  ylab("Density of values") +
  labs(title="R ~ LKJcorr(5)")

all_priors <- (sample_ints + predictors) / (sigma2 + lkjcorr)
ggsave("images/prior_all.png", all_priors, scale=1)
