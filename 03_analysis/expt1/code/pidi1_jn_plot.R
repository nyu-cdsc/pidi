# Johnson-Neyman plot
expt1_jn_inference_plot <- 
  expt1_jnplot + 
  theme(legend.position = c(.8,.15)) +
  guides(linetype = FALSE) + 
  labs(x = "Age",
       y = "Simple slope of condition\npredicting expected inference") + 
  geom_segment(aes(x = (as.numeric(expt1_jn_value) + .6),
                   y = 5,
                   xend = (as.numeric(expt1_jn_value) + .1),
                   yend = 5),
               colour = 'black',
               size = .5,
               arrow = arrow(length = unit(0.3, "cm")))+
  annotate("label", 
           x = (as.numeric(expt1_jn_value) + 1.1), 
           y = 5, 
           label = paste(round(as.numeric(expt1_jn_value), 1), "years"),
           label.padding = unit(0.55, "lines"),
           fontface = "bold") 

expt1_jn_test_plot_unmentioned <- 
  expt1_jn_unmentioned$plot + 
  labs(x = "Age",
       y = "",
       title = "Unmentioned group") + 
  theme(legend.position = c(.85,.85),
        plot.title = element_text(face = "plain"),
        axis.title = element_text(size = 14)) +
  guides(linetype = FALSE) + 
  geom_segment(aes(x = (as.numeric(expt1_test_jn_unmentioned_value) + .6),
                   y = 1.5,
                   xend = (as.numeric(expt1_test_jn_unmentioned_value) + .2),
                   yend = 1.5),
               colour = 'black',
               size = .5,
               arrow = arrow(length = unit(0.3, "cm")))+
  annotate("label", 
           x = (as.numeric(expt1_test_jn_unmentioned_value) + 1), 
           y = 1.5, 
           label = paste(round(as.numeric(expt1_test_jn_unmentioned_value), 1), "years"),
           label.padding = unit(0.55, "lines"),
           fontface = "bold") 


expt1_jn_test_plot_mentioned <- 
  expt1_jn_mentioned$plot + 
  labs(x = "Age",
       y = "Simple slope of condition\npredicting extension of property",
       title = "Mentioned group") + 
  theme(legend.position = "none",
        plot.title = element_text(face = "plain"),
        axis.title = element_text(size = 14)) + 
  geom_segment(aes(x = (as.numeric(expt1_test_jn_mentioned_value) + .6),
                   y = 4,
                   xend = (as.numeric(expt1_test_jn_mentioned_value) + .2),
                   yend = 4),
               colour = 'black',
               size = .5,
               arrow = arrow(length = unit(0.3, "cm")))+
  annotate("label", 
           x = (as.numeric(expt1_test_jn_mentioned_value) + 1), 
           y = 4, 
           label = paste(round(as.numeric(expt1_test_jn_mentioned_value), 1), "years"),
           label.padding = unit(0.55, "lines"),
           fontface = "bold") 

test_plot <- ggdraw() +
  draw_plot(expt1_jn_test_plot_unmentioned, x = 0.5, y = 0, width = .5, height = 1) + 
  draw_plot(expt1_jn_test_plot_mentioned, x = 0, y = 0, width = .5, height = 1) 
