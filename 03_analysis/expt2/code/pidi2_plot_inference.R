# Filtering out just one row each for response average 
expt2_inference_plot <- expt2_inference %>%
  distinct(id, .keep_all = TRUE)

expt2_inference_summary <- summarySE(data = expt2_inference,
                                      measurevar = "inf",
                                      groupvars = c("condition"))


expt2_plot_inference <-   ggplot() + 
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
        panel.background = element_rect(fill = NA)
  ) +
  geom_point(
    data = expt2_inference_plot,
    aes(
      y = inf_avg,
      x = condition,
      color = condition,
      shape = condition,
      fill = condition),
    position = position_jitter(w = 0.2, h = 0.02),
    size = 2,
    alpha = .5,
    fill = NA,
    show.legend = FALSE
  ) +
  geom_errorbar(
    data = expt2_inference_summary,
    aes(x = condition,
        ymin = inf - ci,
        ymax = inf + ci),
    width = .05,
    color = "black",
    show.legend = FALSE
  ) +
  geom_point(
    data = expt2_inference_summary,
    aes(
      x = condition,
      y = inf,
      shape = condition,
      fill = condition),
    color = "black",
    size = 7
  ) +
  labs(x = "Condition",
       y = "Prop. of trials participants\nmade expected inference"
  ) +
  scale_shape_manual(name = "Group membership",
                     labels = c("Unmentioned", "Previously mentioned"), 
                     values = c(21,22)
  ) + 
  scale_color_manual(name = "Group membership",
                     labels = c("Unmentioned", "Previously mentioned"),
                     values = c("#e39b2b", "#619f97")
  ) +
  scale_fill_manual(name = "Group membership",
                    labels = c("Unmentioned", "Previously mentioned"), 
                    values = c("#e39b2b", "#619f97")
  ) +
  guides(fill = FALSE,
         shape = FALSE,
         color = FALSE
  ) 

# 
# 
# expt2_plot_inference# Plotting number of inferences that participants make
# expt2_plot_inference <- ggplot() +
#   geom_point(
#     data = expt2_inference_child_plot, 
#     aes(x = age_exact,
#         y = inf_avg, 
#         color = condition,
#         shape = condition,
#         fill = condition),
#     alpha = .5,
#     size = 1.5,
#     position = position_jitter(w = 0, h = 0.02)
#   ) + 
#   geom_smooth(
#     data = expt2_inference,
#     aes(x = age_exact,
#         y = inf,
#         color = condition,
#         fill = condition),
#     method = "lm"
#   ) +
#   theme_classic() +
#   labs(x = "Age",
#        y = "Proportion of trials make\ninference about unmentioned group",
#        title = "",
#        fill = element_blank()) +
#   scale_x_continuous(breaks = c(4, 5, 6, 7, 8), 
#                      labels = c("4", "5", "6", "7", "Adults")
#   ) +
#   theme(panel.grid.major = element_blank(),
#         strip.background = element_blank(),
#         plot.title = element_text(size = 14, face = "bold",
#                                   margin = margin(t = 0, r = 0, b = 20, l = 0)),
#         axis.title.x = element_text(size = 14, face = "bold",
#                                     margin = margin(t = 20, r = 0, b = 0, l = 0)),
#         axis.title.y = element_text(size = 14, face = "bold",
#                                     margin = margin(t = 0, r = 20, b = 0, l = 0)),
#         axis.text.x = element_text(size = 14),
#         axis.text.y = element_text(size = 14),
#         strip.text = element_text(size = 14),
#         legend.text = element_text(size = 14),
#         legend.title = element_text(size = 14)
#   ) +
#   scale_shape_manual(name = "Condition:",
#                      labels = c("Generic", "Specific"),
#                      values = c(21, 22)
#   ) +
#   scale_color_manual(name = "Condition:",
#                      labels = c("Generic", "Specific"),
#                      values = c( "#e39b2b", "#619f97")
#   ) +
#   scale_fill_manual(name = "Condition:",
#                     labels = c("Generic", "Specific"),
#                     values = c("#e39b2b", "#619f97")
#   ) +
#   guides(fill = guide_legend(title.position = "top",
#                              title.hjust = 0.5),
#          shape = guide_legend(title.position = "top",
#                               title.hjust = 0.5),
#          color = guide_legend(override.aes = list(fill = c("#e39b2b", "#619f97")),
#                               title.position = "top",
#                               title.hjust = 0.5)
#   ) 
# 
# 
# 
# 
# 
# 
# expt2_inference_summary <- summarySE(
#   data = expt2_inference_children,
#   measurevar = "inf_avg",
#   groupvars = "condition")
# 
# expt2_inference_no4s <- expt2_inference_children %>%
#   filter(age_categorical != 4)
# 
# expt2_inference_no4s_summary <- summarySE(
#   data = expt2_inference_no4s,
#   measurevar = "inf_avg",
#   groupvars = "condition")
# 
# expt2_plot_inference <- ggplot(
#   expt2_inference_children,
#   aes(x = condition,
#       y = inf_avg,
#       fill = condition,
#       color = condition,
#       group = condition,
#       shape = condition)) +
#   theme_classic() +
#   geom_point(
#     alpha = .3,
#     size = 2,
#     position = position_jitter(w = 0.2, h = 0.02)
#   ) +
#   geom_errorbar(
#     data = expt2_inference_summary,
#     aes(x = condition,
#         ymin = inf_avg - ci,
#         ymax = inf_avg + ci),
#     width = .1,
#     color = "black",
#     show.legend = FALSE
#   ) +
#   geom_point(
#     data = expt2_inference_summary,
#     aes(
#       x = condition,
#       y = inf_avg,
#       shape = condition,
#       fill = condition),
#     color = "black",
#     size = 10
#   ) +
#   labs(x = "Condition",
#        y = "",
#        title = "Full sample",
#        fill = element_blank()) +
#   theme(panel.grid.major = element_blank(),
#         strip.background = element_blank(),
#         plot.title = element_text(size = 28,
#                                   margin = margin(t = 0, r = 0, b = 20, l = 0)),
#         axis.title.x = element_text(size = 30, face = "bold",
#                                     margin = margin(t = 20, r = 0, b = 0, l = 0)),
#         axis.title.y = element_text(size = 32, face = "bold",
#                                     margin = margin(t = 0, r = 20, b = 0, l = 0)),
#         axis.text.x = element_text(size = 28),
#         axis.text.y = element_text(size = 28),
#         strip.text = element_text(size = 28),
#         legend.text = element_text(size = 28),
#         legend.title = element_text(size = 28),
#         plot.margin = unit(c(2,0,0,0), "lines")
#   ) +
#   scale_shape_manual(name = "Condition:",
#                      labels = c("Generic", "Specific"),
#                      values = c(21, 22)
#   ) +
#   scale_color_manual(name = "Condition:",
#                      labels = c("Generic", "Specific"),
#                      values = c( "#e39b2b", "#619f97")
#   ) +
#   scale_fill_manual(name = "Condition:",
#                     labels = c("Generic", "Specific"),
#                     values = c("#e39b2b", "#619f97")
#   ) +
#   guides(fill = FALSE,
#          shape = FALSE,
#          color = FALSE
#   )
# 
# 
# expt2_plot_inference_no4s <- ggplot(
#   expt2_inference_no4s,
#   aes(x = condition,
#       y = inf_avg,
#       fill = condition,
#       color = condition,
#       group = condition,
#       shape = condition)) +
#   theme_classic() +
#   geom_point(
#     alpha = .3,
#     size = 2,
#     position = position_jitter(w = 0.2, h = 0.02)
#   ) +
#   geom_errorbar(
#     data = expt2_inference_no4s_summary,
#     aes(x = condition,
#         ymin = inf_avg - ci,
#         ymax = inf_avg + ci),
#     width = .1,
#     color = "black",
#     show.legend = FALSE
#   ) +
#   geom_point(
#     data = expt2_inference_no4s_summary,
#     aes(
#       x = condition,
#       y = inf_avg,
#       shape = condition,
#       fill = condition),
#     color = "black",
#     size = 10
#   ) +
#   labs(x = "Condition",
#        y = "",
#        title = "5s and 6s only",
#        fill = element_blank()) +
#   theme(panel.grid.major = element_blank(),
#         strip.background = element_blank(),
#         plot.title = element_text(size = 28,
#                                   margin = margin(t = 0, r = 0, b = 20, l = 0)),
#         axis.title.x = element_text(size = 30, face = "bold",
#                                     margin = margin(t = 20, r = 0, b = 0, l = 0)),
#         axis.title.y = element_text(size = 32, face = "bold",
#                                     margin = margin(t = 0, r = 20, b = 0, l = 0)),
#         axis.text.x = element_text(size = 28),
#         axis.text.y = element_text(size = 28),
#         strip.text = element_text(size = 28),
#         legend.text = element_text(size = 28),
#         legend.title = element_text(size = 28),
#         plot.margin = unit(c(2,0,0,0), "lines")
#   ) +
#   scale_shape_manual(name = "Condition:",
#                      labels = c("Generic", "Specific"),
#                      values = c(21, 22)
#   ) +
#   scale_color_manual(name = "Condition:",
#                      labels = c("Generic", "Specific"),
#                      values = c( "#e39b2b", "#619f97")
#   ) +
#   scale_fill_manual(name = "Condition:",
#                     labels = c("Generic", "Specific"),
#                     values = c("#e39b2b", "#619f97")
#   ) +
#   guides(fill = FALSE,
#          shape = FALSE,
#          color = FALSE
#   )
# 
# expt2_plots_inference <- ggarrange(
#   expt2_plot_inference, expt2_plot_inference_no4s
# )
# 
# expt2_plots_inference <-
#   annotate_figure(expt2_plots_inference,
#                 top = text_grob("Proportion of trials children inferred mentioned\ngroup has property AND unmentioned group does not",
#                                 size = 30,
#                                 face = "bold",
#                                 x = 0.1,
#                                 hjust = 0)
#                 )
