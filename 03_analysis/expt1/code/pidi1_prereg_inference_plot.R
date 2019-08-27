# Filtering out just one row each for response average 
expt1_first_180_inference_child_plot <- expt1_first_180_inference_child %>%
  filter(!is.na(age_exact)) %>%
  distinct(id, .keep_all = TRUE)

expt1_first_180_inference_adult_plot <- expt1_first_180_inference_adult %>%
  distinct(id, .keep_all = TRUE)


expt1_first_180_inference_summary_adult = summarySE(data = expt1_first_180_inference_adult,
                                          measurevar = "inf",
                                          groupvars = c("condition",
                                                        "age_categorical"))
# Plotting number of inferences that participants make
expt1_first_180_plot_inference <- ggplot() +
  geom_point(
    data = expt1_first_180_inference_child_plot, 
    aes(x = age_exact,
        y = inf_avg, 
        color = condition,
        shape = condition),
    alpha = .5,
    size = 1,
    position = position_jitter(w = 0, h = 0.02)
  ) + 
  geom_smooth(
    data = expt1_first_180_inference_child_gee,
    aes(x = age_exact,
        y = inf,
        color = condition,
        fill = condition),
    method = "lm"
  ) +
  geom_point(
    data = expt1_first_180_inference_adult_plot,
    aes(x = as.numeric(as.character(age_categorical)),
        y = inf_avg,
        color = condition,
        shape = condition),
    position = position_jitter(w = 0.2, h = 0.02),
    size = 1,
    alpha = .5,
    show.legend = FALSE
  ) +
  geom_errorbar(
    data = expt1_first_180_inference_summary_adult,
    aes(ymin = inf - ci,
        ymax = inf + ci,
        x = as.numeric(as.character(age_categorical))),
    width = .3
  ) +
  geom_point(
    data = expt1_first_180_inference_summary_adult,
    aes(y = inf,
        x = as.numeric(as.character(age_categorical)),
        fill = condition,
        shape = condition),
    color = "black",
    size = 7
  ) +
  theme_classic() +
  labs(x = "Age",
       y = "Prop. of trials participants\nmade expected inference",
       fill = element_blank()) +
  scale_x_continuous(breaks = c(4, 5, 6, 7, 8), 
                     labels = c("4", "5", "6", "7", "Adults")
  ) +
  theme(text = element_text(size = 14),
        panel.grid.major = element_blank(),
        strip.background = element_blank(),
        plot.title = element_text(size = 14,
                                  margin = margin(t = 0, r = 0, b = 20, l = 0)),
        axis.title.x = element_text(size = 14, 
                                    margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 14,
                                    margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        strip.text = element_text(size = 14),
        legend.title = element_text(size = 13)
  ) +
  scale_shape_manual(name = "Condition",
                     labels = c("Generic", "Specific"),
                     values = c(21, 22)
  ) +
  scale_color_manual(name = "Condition",
                     labels = c("Generic", "Specific"),
                     values = c( "#e39b2b", "#619f97")
  ) +
  scale_fill_manual(name = "Condition",
                    labels = c("Generic", "Specific"),
                    values = c("#e39b2b", "#619f97")
  ) +
  guides(fill = guide_legend(title.position = "top",
                             title.hjust = 0.5),
         shape = guide_legend(title.position = "top",
                              title.hjust = 0.5),
         color = guide_legend(override.aes = list(fill = c("#e39b2b", "#619f97")),
                              title.position = "top",
                              title.hjust = 0.5)
  ) 

expt1_first_180_plot_inference
