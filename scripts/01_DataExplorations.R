library(ggdist)
library(gghalves)
library(readr)
library(dplyr)
library(ggplot2)
library(patchwork)
library(viridis)

###################################
###       Data                  ###
###################################
data <- read_tsv('data.tsv') %>% mutate(
  initial=ifelse(
    utt_initial==1, "utterance-initial", ifelse(
      word_initial==1, "word-initial", "other"
      )))

langs <- data %>% group_by(Language) %>% count() %>% arrange(Language)
phons <- data %>% group_by(Language, Value) %>% count() %>% arrange(n)
cons <- data %>% group_by(sound_class, utt_initial) %>% count() %>% arrange(n)

################################################
#####       Distribution Plots             #####
################################################
tens <- data %>% 
  filter(Duration %% 10 == 0) %>% 
  group_by(Duration, initial) %>% count() %>%
  ggplot(aes(Duration, n, col=initial))+
  geom_point(size=2) +
  scale_y_log10() +
  theme_grey(base_size=11) +
  scale_color_viridis(discrete=TRUE, end=0.7) +
  scale_x_log10(breaks=c(30, 50, 70, 100, 150, 200, 300, 500),
                limits=c(30, 420)) +
  labs(title="Multiples of 10", col="") +
  ylab("Occurrences") + xlab("duration on log-axis")

non_tens <- data %>% 
  filter(Duration %% 10 != 0) %>% 
  group_by(Duration, initial) %>% count() %>%
  ggplot(aes(Duration, n, col=initial))+
  geom_point(size=2) +
  scale_y_log10() +
  theme_grey(base_size=11) +
  scale_color_viridis(discrete=TRUE, end=0.7) +
  scale_x_log10(breaks=c(30, 50, 70, 100, 150, 200, 300, 500),
                limits=c(30, 420)) +
  labs(title="Non-Multiples", col="") +
  ylab("Occurrences") + xlab("duration on log-axis")

distr <- (tens / non_tens) + plot_layout(guides="collect") & theme(legend.position="bottom")
ggsave("images/dataExpl_distr.png", distr, scale=1,
       width=2000, height=2000, units="px")

dens_all <- data %>%
  ggplot(aes(x=initial, y=Duration, color=initial, fill=initial)) +
  geom_boxplot(width=.2, fill="white", size=1, outlier.shape=NA) +
  geom_half_point(side="l", range_scale=.25, alpha=.5, size=0.1) +
  stat_halfeye(adjust=1, width=.5, color=NA, position=position_nudge(x=.15)) +
  coord_flip() +
  scale_fill_viridis(discrete=TRUE, end=0.7) +
  scale_color_viridis(discrete=TRUE, end=0.7) +
  scale_y_log10(limits=c(30, 500), breaks=c(30, 40, 50, 60, 80, 100, 150, 200, 300, 500), 
                name="duration in ms on log-axis") +
  scale_x_discrete(labels=c("non-initial", "utterance-initial", "word-initial"))+
  xlab("") + theme(legend.position="none")

ggsave("images/dataExpl_dens.png", dens_all, scale=1,
       width=2000, height=1450, units="px")

################################################
#####       Between-Languages Plots        #####
################################################
violin_init <- data %>% 
  ggplot(aes(y=Duration, x=initial)) +
  geom_violin(aes(fill=initial)) +
  geom_boxplot(width=0.5, 
               outlier.size=1, outlier.color="black", outlier.alpha=0.3) +
  # If you want to plot the distribution across all languages, uncomment the
  # following line and set ncol=n according to your needs.
  # facet_wrap(~Language, ncol=4) +
  scale_fill_viridis(discrete=TRUE, end=0.7) +
  scale_y_log10(limits=c(30, 420), breaks=c(30, 70, 150, 300, 500), 
                name="duration on log-axis") +
  scale_x_discrete(label=NULL, name=NULL) +
  theme_grey(base_size=11) +
  theme(legend.position='bottom', legend.title=element_blank())

ggsave("images/dataExpl_violinInit.png", violin_init, scale=1,
       width=1600, height=2300, units="px")
