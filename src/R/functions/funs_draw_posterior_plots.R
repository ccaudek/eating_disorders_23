#' The function draw_delta_par_fig() takes as arguments 
#' - param_long: the vector obtained from 
#'   here::here("data", "processed", "prl", "output_hddm", "traces.Rda")
#'   with one of the a, v, t, alpha, pos_alpha parameters;
#' - y_label: a string used for the y-label of the figure;
#' - file_name: the name of the pdf file with the figure.
#' 
#' The script for generating the figure has been adapted from the script
#' used by Santangelo, Ludwig, Navajas, Sigman, & Leone (2022) for their
#' Figure 4.
draw_delta_par_fig <- function(param_long, y_label, file_name,
                               x_coord_1, x_coord_2) {
  
  # THRESHOLD (a) ====
  # marginal posterior median and CI95
  param_long %>% 
    group_by(diag_cat, stim, draw) %>% 
    summarize(M=mean(val)) %>% 
    tidybayes::median_qi(M)
  
  # compute contrasts
  param_contrasts =
    param_long %>% 
    group_by(diag_cat, stim, draw) %>% 
    summarize(M=mean(val)) %>% 
    pivot_wider(names_from = c(diag_cat, stim), values_from = M, names_prefix = "m") %>% 
    mutate(
      param_an = mAN_food - mAN_neutral,
      param_hc = mHC_food - mHC_neutral,
      param_ah = param_an - param_hc) %>% 
    pivot_longer(cols = c(param_an, param_hc, param_ah), names_to = "contrast", values_to = "delta")
  
  # keep just slow vs silence and fast vs silence contrasts
  param_contrasts2 =
    param_contrasts %>% 
    filter(contrast != "param_ah") %>% 
    select(delta, draw, contrast) %>% 
    mutate(
      contrast=factor(contrast, levels=c("param_an", "param_hc")))
  
  # compute posterior summaries
  param_contrasts_summ =
    param_contrasts %>% 
    group_by(contrast) %>% 
    median_qi(delta)
  
  # keep just slow vs silence and fast vs silence contrasts
  param_contrasts_summ2 =
    param_contrasts_summ %>% 
    filter(contrast!="param_ah") %>% 
    mutate(ys = c(5, 1),
           contrast=factor(contrast, levels=c("param_an", "param_hc")))
  
  # plot threshold contrasts (fig 4a)
  tempo_cols=c("#E69F00", "#56B4E9")
  hdiwd=5
  psz=15
  fsz=60
  alf=.8
  
  ggplot() +
    scale_fill_manual(values=tempo_cols) +
    scale_color_manual(values=tempo_cols) +
    
    # SILENCE ref line
    geom_segment(aes(x = 0, y =-0.2, xend = 0, yend = 56),
                 colour = "grey", lwd = 5 , lty=1, alpha =.8) +
    
    # DENSITIES
    stat_halfeye(data=param_contrasts2, aes(y=1,x=delta, fill=contrast),
                 .width=0, alpha=alf, lwd=0,scale = 56) +
    # HDI
    geom_segment(data=param_contrasts_summ2, 
                 aes( y=-1-ys,
                      yend=-1-ys,
                      x=.lower, 
                      xend=.upper),
                 lwd=hdiwd, lineend = "round") +
    #MEDIAN
    geom_point(data=param_contrasts_summ2, aes(x=delta,y=-1-ys), 
               size=psz,shape=21,color="black", fill="black")+
    geom_point(data=param_contrasts_summ2, aes(x=delta,y=-1-ys), 
               size=psz-4,shape=21, fill="white")+
    geom_point(data=param_contrasts_summ2, aes(x=delta,y=-1-ys, fill=contrast), 
               alpha=.75,size=psz-4,shape=21)+
    geom_hline(yintercept = -0.2,lwd=1, lty=1)+
    labs(y= "", x= y_label) +
    coord_flip() +
    theme_classic() +
    theme(panel.background = element_blank(), 
          panel.grid = element_blank(), 
          panel.spacing.x = unit(0,"line"),
          text = element_text(size=fsz),
          legend.position = "none",
          legend.title = element_blank(),
          axis.line.y = element_blank(),
          axis.line.x = element_blank(),
          axis.text.x= element_blank()) +
    annotate("text", x=x_coord_1, y=50, label= "AN", colour = "#E69F00", size = 20) +
    annotate("text", x=x_coord_2, y=50, label= "HC", colour = "#56B4E9", size = 20) 
    
  
  ggsave(file_name, width = 18, height = 10, dpi = 300)
}

