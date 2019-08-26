# plotting by age
prag3_by_age <- ggplot() + 
  geom_point(
    data = expt3_adhoc_avg,
    aes(x = age_exact,
        y = pragmatic_avg),
    position = position_jitter(w = 0, h = 0.02),
    alpha = .2) + 
  geom_smooth(data = expt3_adhoc_avg,
              aes(x = age_exact,
                  y = pragmatic_avg),
              method = "loess",
              color="black",
              fill="blue") + 
  theme_classic() + 
  scale_x_continuous(limits = c(4,7.3)) + 
  scale_y_continuous(limits = c(-.02,1.1),
                     breaks = c(0,.25,.5,.75,1)) +
  labs(x = "Age",
       y = "Proportion of trials participants\nselected the pragmatic option")
