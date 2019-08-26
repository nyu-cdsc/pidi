# getting summary of adult data for plot
expt1_adhoc_avg_adult_summary <- summarySE(
  data = expt1_adhoc_avg_adult,
  measurevar = "pragmatic_avg", 
  groupvars = "age_exact")

# plotting by age
prag_by_age <- ggplot(
  data = expt1_adhoc_avg_child,
  aes(x = age_exact,
      y = pragmatic_avg)) + 
  geom_point(
    position = position_jitter(w = 0, h = 0.02),
    alpha = .2) + 
  geom_smooth(method = "loess",
              color="black",
              fill="blue") + 
  geom_point(
    data = expt1_adhoc_avg_adult,
    aes(x = age_exact,
        y = pragmatic_avg),
    position = position_jitter(w = .2, h = 0.02),
    alpha = .2) +
  theme_classic() + 
  scale_x_continuous(limits = c(4,8.2),
                     breaks = c(4,5,6,7,8),
                     labels = c("4", "5", "6", "7", "Adults")) + 
  scale_y_continuous(limits = c(-.02,1.1),
                     breaks = c(0,.25,.5,.75,1)) +
  geom_errorbar(
    data = expt1_adhoc_avg_adult_summary,
    aes(x = age_exact,
        ymin = pragmatic_avg - ci,
        ymax = pragmatic_avg + ci),
    size = 1
  ) +
  geom_point(
    data = expt1_adhoc_avg_adult_summary,
    aes(x = age_exact,
        y = pragmatic_avg),
    fill = "blue",
    shape = 21,
    size = 6
  ) +
  labs(x = "Age",
       y = "Proportion of trials participants\nselected the pragmatic option")
