# Filtering out just one row each for response average for both group types 
expt1_first_180_test_child_plot <- expt1_first_180_test_child %>%
  distinct(id, group, .keep_all = TRUE) %>% 
  filter(!is.na(age_exact))

expt1_first_180_test_adult_plot <- expt1_first_180_test_adult %>%
  distinct(id, group, .keep_all = TRUE) 

expt1_first_180_test_summary_adult <- summarySEwithin(
  data = expt1_first_180_test_adult,
  measurevar = "response",
  betweenvars = c("age_categorical", "condition"),
  withinvars = "group"
)

# Plotting proportion of trials participants extend 
# property to members of mentioned and unmentioned groups
expt1_first_180_plot_prop_extension <- ggplot() + 
  facet_wrap (
    . ~ condition,
    labeller = as_labeller(c(`generic` = "Generic", `specific` = "Specific"))
  ) +
  geom_point(
    data = expt1_first_180_test_child_plot,
    aes(x = age_exact,
        y = response_avg,
        color = group,
        shape = group),
    position = position_jitter(w = 0, h = 0.02),
    size = 1,
    alpha = .5,
    show.legend = FALSE
  ) +
  geom_smooth(
    data = expt1_first_180_test_child_gee,
    aes(x = age_exact,
        y = response,
        color = group,
        fill = group),
    method = "lm") + 
  geom_point(
    data = expt1_first_180_test_adult_plot,
    aes(x = as.numeric(as.character(age_categorical)),
        y = response_avg,
        color = group,
        shape = group),
    position = position_jitter(w = 0.2, h = 0.02),
    size = 1,
    alpha = .5,
    show.legend = FALSE
  ) + 
  geom_errorbar(
    data = expt1_first_180_test_summary_adult,
    aes(ymin = response - ci,
        ymax = response + ci,
        x = as.numeric(as.character(age_categorical))),
    width = .3
  ) +
  geom_point(
    data = expt1_first_180_test_summary_adult,
    aes(y = response,
        x = as.numeric(as.character(age_categorical)),
        fill = group,
        shape = group),
    color = "black",
    size = 7
  ) +
  scale_y_continuous(expand = c(0, 0)
  ) +
  scale_x_continuous(
    breaks = c(4, 5, 6, 7, 8),
    labels = c("4", "5", "6", "7", "Adults")
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
        # legend.direction = "horizontal",
        # legend.position = "bottom"
  ) +
  labs(x = "Age",
       y = "Prop. of trials participants\nextended target property"
  ) +
  scale_shape_manual(name = "Group membership",
                     labels = c("Mentioned", "Unmentioned"), 
                     values = c(22,21)
  ) + 
  scale_color_manual(name = "Group membership",
                     labels = c("Mentioned","Unmentioned"),
                     values = c("#d9be00", "#58b947")
  ) +
  scale_fill_manual(name = "Group membership",
                    labels = c("Mentioned","Unmentioned"),
                    values = c("#d9be00", "#58b947")
  ) +
  guides(fill = guide_legend(
    title.position = "top",
    title.hjust = 0.5),
    shape = guide_legend(
      title.position = "top",
      title.hjust = 0.5),
    color = guide_legend(
      # override.aes = list(fill = c("#58b947", "#d9be00")),
      title.position = "top",
      title.hjust = 0.5)
  ) 


expt1_first_180_plot_prop_extension
