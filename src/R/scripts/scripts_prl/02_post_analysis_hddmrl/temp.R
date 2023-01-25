param_long <- pos_alpha_long
y_label <- expression(paste(Delta, alpha^"+") ["(neutral - food)"])
file_name <- here::here("src", "R", "figures", "pos_alpha_param_grp.png")


FLAG <- "param_hc"
COLOR <- "#56B4E9"
p1 <- draw_delta_par_fig_flag(
  param_long, y_label, file_name, 2.5, FLAG, COLOR
)

FLAG <- "param_an"
COLOR <- "#E69F00" 
p2 <- draw_delta_par_fig_flag(
  param_long, y_label, file_name, 5.5, FLAG, COLOR
)

figure <- ggarrange(p1, p2,
                    ncol = 1, nrow = 2)


annotate_figure(figure,
                top = text_grob("Domain-Specificity Effect", color = "black", face = "bold", size = 40),
                #bottom = text_grob("Data source: \n mtcars data set", color = "blue",
                #                  hjust = 1, x = 1, face = "italic", size = 10),
                left = text_grob(y_label, rot = 90, size = 60, vjust = 1.0, color = "gray30"),
                #right = "I'm done, thanks :-)!",
                fig.lab = "A", fig.lab.face = "bold", fig.lab.size = 70
)
