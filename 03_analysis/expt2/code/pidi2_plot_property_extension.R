expt2_test_summary <- summarySEwithin(data = expt2_test_children,
                                      measurevar = "response_avg",
                                      betweenvars = "subjectGroup",
                                      withinvars = "stimulus",
                                      idvar = "id")


expt2_plot_prop_extension <-   ggplot(
  data = expt2_test_children,
  aes(
    y = response_avg,
    x = subjectGroup,
    color = stimulus,
    shape = stimulus,
    fill = stimulus)
  ) + 
  scale_y_continuous(expand = c(0, 0)
  ) +
  coord_cartesian(ylim=c(-.05, 1.05)
  ) + 
  theme(text         = element_text(size = 28),
        axis.title.x = element_text(size = 32, 
                                    face = "bold",
                                    margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 32, 
                                    face = "bold",
                                    margin = margin(t = 0, r = 20, b = 0, l = 0)),
        strip.text = element_text(size = 28),
        strip.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        panel.background = element_rect(fill = NA),
        legend.direction = "horizontal"
  ) + 
  geom_point(
    position = position_jitter(w = 0.2, h = 0.02),
    size = 2,
    alpha = .5,
    fill = NA,
    show.legend = FALSE
  ) +
  geom_errorbar(
    data = expt2_test_summary,
    aes(x = subjectGroup,
        ymin = response_avg - ci,
        ymax = response_avg + ci),
     width = .1,
    color = "black",
    show.legend = FALSE
  ) +
  geom_point(
    data = expt2_test_summary,
    aes(
      x = subjectGroup,
      y = response_avg,
      shape = stimulus,
      fill = stimulus),
    color = "black",
    size = 10
  ) +
  labs(x = "Condition",
       y = "Proportion of trials"
  ) +
  scale_shape_manual(name = "Group membership",
                     labels = c("Unmentioned", "Previously mentioned"), 
                     values = c(21,22)
  ) + 
  scale_color_manual(name = "Group membership",
                     labels = c("Unmentioned", "Previously mentioned"),
                     values = c("#58b947", "#d9be00")
  ) +
  scale_fill_manual(name = "Group membership",
                    labels = c("Unmentioned", "Previously mentioned"), 
                    values = c("#58b947", "#d9be00")
  ) +
  guides(fill = guide_legend(reverse = TRUE,
                             title.position = "top",
                             title.hjust = 0.5),
         shape = guide_legend(reverse = TRUE,
                              title.position = "top",
                              title.hjust = 0.5),
         color = FALSE
  ) 