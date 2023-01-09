library(ggridges)

# distribution of values between 0 and 50
# weird hole between 30 and 40
# multiple values coerced before that
data_sample %>% 
  filter(Duration <= 50, 
         Duration %% 10 != 0) %>% 
  group_by(Duration, Initial) %>% count() %>%
  
  ggplot(aes(Duration, n, col=Initial))+
  geom_point(size = 1.5) + #geom_line() +
  scale_y_log10() +
  theme_grey(base_size = 11) +
  scale_color_viridis(discrete = TRUE, end = 0.8) +
  labs(title = "Non-Multiples")

# density plot overlap
# nice plot idea, but not for this kind of overlapping data
data %>% 
  filter(Duration >= 40) %>% 
  ggplot(aes(x = logDuration, group = Initial, fill = Initial)) +
  geom_density(adjust = 1.5, alpha = 0.3) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  scale_x_continuous(limits = c(3, 6.5)) +
  facet_wrap(~SoundClass)+
  theme(legend.position="none") +
  ggtitle("Density distribution of consonants")

# plotting density between languages
# not really telling any information
data_sample %>% 
  filter(Duration >= 40) %>% 
  ggplot(aes(x = logDuration)) +
  geom_density(adjust = 1.5, alpha = 0.3) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  scale_x_continuous(limits = c(3.2, 6.5)) +
  facet_grid(Initial~Language) +
  theme(legend.position="none")+
  ggtitle("Density distribution of consonants")


# too much going on
#  not really telling, more confusing
data_sample %>% mutate(phon_class = paste(Initial, SoundClass)) %>% 
  filter(Duration>=40) %>% 
  ggplot(aes(y = logDuration, x = SoundClass)) +
  geom_violin(aes(fill = SoundClass)) +
  geom_boxplot(width = 0.4, 
               outlier.size = 2, outlier.color = "black", outlier.alpha = 0.5) +
  facet_grid(Initial~Language, scales = "free_x") +
  scale_fill_viridis(discrete = TRUE, option = "C") +
  scale_y_continuous(limits = c(log(38), 6.5), breaks = seq(from = 3, to = 7, by = 1)) +
  scale_x_discrete(label = NULL) +
  labs(x = "Consonants across languages",
       y = "Duration on log-scale") +
  theme_grey(base_size = 12)



#################################################################

data30 %>% filter(Duration >= 40) %>% 
  ggplot(aes(y = Language, x = logDuration, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Duration on log-scale", option = "A") +
  labs(title = 'Duration of consonants across languages') +
  #theme_ipsum() +
  theme(legend.position="none")+
  scale_x_continuous(limits=c(3.5, 5.9))


## patchwork try of with and without <40


violin_all | violin_40 +
  plot_layout(
    guides = "collect"
  ) +
  plot_annotation(
    title = "Distribution of consonants in different positions across languages"
  )

library(ggforce)

ggplot(data_sample) +
  geom_boxplot(aes(x = .panel_x, y = .panel_y, group = .panel_x)) +
  facet_matrix(rows = vars(logDuration),
               cols = vars(SoundClass, Initial))

ggplot(data_sample, aes(x = .panel_x, y = .panel_y, fill = Initial, colour = Initial)) + 
  geom_point(shape = 1, size = 0.5, position = 'auto') + 
  geom_autodensity(alpha = 0.3, colour = NA, position = 'identity') + 
  geom_smooth(aes(colour = NULL, fill = NULL)) + 
  facet_matrix(vars(logDuration, Initial), layer.diag = 2, layer.continuous = TRUE,
               layer.mixed = -3, layer.discrete = -3)

data_sample %>% 
  filter(Duration>=40) %>% 
  ggplot(aes(y = logDuration, x = Initial)) +
  geom_violin(aes(color = Initial, fill = Initial)) +
  facet_grid(Initial~Language) +
  scale_fill_viridis(discrete = TRUE) +
  scale_y_continuous(limits = c(log(38), 6.1)) +
  scale_x_discrete(label = NULL) +
  labs(x = "Consonants across languages",
       y = "Duration on log-scale") +
  theme_bw()




new <- data_sample %>% pivot_longer( cols = c(Initial, SoundClass), 
                                     names_to = "type", values_to = "type_value" )

new2 <- data_sample %>% mutate(Soundclass_num = as.numeric(factor(SoundClass)), 
                Initial_num = as.numeric(factor(Initial))) %>% 
  pivot_longer( cols = c(Initial_num, Soundclass_num), 
                names_to = "type", values_to = "type_value" ) 

# interesting, use!
new2 %>% 
  filter(Duration>=40) %>% 
  ggplot(aes(y = logDuration, x = factor(type_value))) +
  geom_violin(aes(fill = factor(type_value))) +
  facet_grid(type~Language, scales = "free_x") +
  scale_fill_viridis(discrete = TRUE) +
  scale_y_continuous(limits = c(log(38), 6.1)) +
  scale_x_discrete(label = NULL) +
  labs(x = "Consonants across languages",
       y = "Duration on log-scale") +
  theme_bw()

#######################################
###     Mean and SD stats           ###
#######################################
stat_graph <- ppc_stat_2d(duration_vals, sim_data) + legend_none()
stat_mean <- ppc_stat(duration_vals, sim_data[1:200,], stat = "mean", binwidth = 0.05) + legend_none()
stat_sd <- ppc_stat(duration_vals, sim_data[1:200,], stat = "sd", binwidth = 0.05) + legend_none()

stat_scatter <- ppc_scatter_avg(duration_vals, sim_data[1:10,], alpha = 0.5, size = 1) + 
  coord_cartesian(xlim = c(20, 320), ylim = c(20, 320), expand = F) +
  scale_y_continuous(name = "real values") +
  scale_x_continuous(name = "predicted values")

pred_error <- ppc_error_scatter_avg(duration_vals, sim_data[1:10,]) +
  scale_y_continuous(name = "real values")

combined_stats <- ((stat_mean / stat_sd + plot_layout(guides = 'collect')) | stat_graph) / 
  (stat_scatter + pred_error) + 
  plot_layout(widths = c(1, 1)) +
  plot_annotation(tag_levels = list(c('Mean', 'SD', '', '', ''), '1'))

ggsave('images/eval_combinedStats.png', combined_stats)
