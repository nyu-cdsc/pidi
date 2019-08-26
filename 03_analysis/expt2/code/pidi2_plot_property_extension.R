expt2_test_summary <- summarySEwithin(data = expt2_test,
                                      measurevar = "response",
                                      betweenvars = "condition",
                                      withinvars = "group",
                                      idvar = "id")


expt2_test_plot <- expt2_test %>%
  distinct(id, group, .keep_all = TRUE)

expt2_plot_prop_extension <-   ggplot() + 
  scale_y_continuous(expand = c(0, 0)
  ) +
  coord_cartesian(ylim=c(-.05, 1.05)
  ) + 
  theme(text         = element_text(size = 14),
        axis.title.x = element_text(size = 14, 
                                    margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 14,
                                    margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.text = element_text(size = 14), 
        strip.text = element_text(size = 14),
        strip.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        panel.background = element_rect(fill = NA),
        legend.title = element_text(size = 13)
  ) +
  geom_point(
    data = expt2_test_plot,
    aes(
      y = response_avg,
      x = condition,
      color = group,
      shape = group,
      fill = group),
    position = position_jitter(w = 0.2, h = 0.02),
    size = 2,
    alpha = .5,
    fill = NA,
    show.legend = FALSE
  ) +
  geom_errorbar(
    data = expt2_test_summary,
    aes(x = condition,
        ymin = response - ci,
        ymax = response + ci),
     width = .05,
    color = "black",
    show.legend = FALSE
  ) +
  geom_point(
    data = expt2_test_summary,
    aes(
      x = condition,
      y = response,
      shape = group,
      fill = group),
    color = "black",
    size = 7
  ) +
  labs(x = "Condition",
       y = "Prop. of trials participants\nextended target property"
  ) +
  scale_shape_manual(name = "Group membership",
                     labels = c("Unmentioned", "Mentioned"), 
                     values = c(21,22)
  ) + 
  scale_color_manual(name = "Group membership",
                     labels = c("Unmentioned", "Mentioned"),
                     values = c("#58b947", "#d9be00")
  ) +
  scale_fill_manual(name = "Group membership",
                    labels = c("Unmentioned", "Mentioned"), 
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
