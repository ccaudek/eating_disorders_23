#' The function draw_delta_par_fig_paper() takes as arguments 
#' - param_long: the vector obtained from 
#'   here::here("data", "processed", "prl", "output_hddm", "traces.Rda")
#'   with one of the a, v, t, alpha, pos_alpha parameters;
#' - y_label: a string used for the y-label of the figure;
#' - file_name: the name of the pdf file with the figure.
#' 
#' The script for generating the figure has been adapted from the script
#' used by Santangelo, Ludwig, Navajas, Sigman, & Leone (2022) for their
#' Figure 4.
draw_delta_par_fig_flag <- function(
    param_long, y_label, file_name, x_coord_1, FLAG, COLOR
    ) {
  
  LEGEND <- ifelse(FLAG == "param_hc", "HC", "AN")
  
  # THRESHOLD (a) ====
  # marginal posterior median and CI95
  param_long %>% 
    group_by(diag_cat, stim, draw) %>% 
    summarize(M=mean(val)) %>% 
    tidybayes::median_qi(M)
  
  # compute contrasts
  param_contrasts <- param_long %>% 
    group_by(diag_cat, stim, draw) %>% 
    summarize(M=mean(val)) %>% 
    pivot_wider(names_from = c(diag_cat, stim), values_from = M, names_prefix = "m") %>% 
    mutate(
      param_an = mAN_neutral - mAN_food,
      param_hc = mHC_neutral - mHC_food,
      param_ah = param_an - param_hc) %>% 
    pivot_longer(
      cols = c(param_an, param_hc, param_ah), 
      names_to = "contrast", values_to = "delta"
      ) 
  
  # keep just slow vs silence and fast vs silence contrasts
  param_contrasts2 =
    param_contrasts %>% 
    filter(contrast != "param_ah") %>% 
    select(delta, draw, contrast) %>% 
    mutate(
      contrast=factor(contrast, levels=c("param_an", "param_hc"))
    ) |> 
    dplyr::filter(contrast == FLAG)
  
  # compute posterior summaries
  param_contrasts_summ =
    param_contrasts %>% 
    group_by(contrast) %>% 
    median_qi(delta)
  
  # keep just slow vs silence and fast vs silence contrasts
  param_contrasts_summ2 =
    param_contrasts_summ %>% 
    filter(contrast!="param_ah") %>% 
    mutate(ys = c(2, 2),
           contrast=factor(contrast, levels=c("param_an", "param_hc"))) |> 
    dplyr::filter(contrast == FLAG)
  
  # plot threshold contrasts (fig 4a)
  tempo_cols=c(COLOR, COLOR)
  hdiwd=5
  psz=15
  fsz=40
  alf=.8
  
  plt <- ggplot() +
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
    geom_hline(yintercept = -0.2, lwd=1, lty=1)+
    labs(y= "", x= "") + # labs(y= "", x= y_label) +
    xlim(-1, 7) + 
    coord_flip() +
    theme_classic() +
    theme(axis.title.x = element_blank()) +
    theme(panel.background = element_blank(), 
          panel.grid = element_blank(), 
          panel.spacing.x = unit(0,"line"),
          text = element_text(size=fsz),
          legend.position = "none",
          legend.title = element_blank(),
          axis.line.y = element_blank(),
          axis.line.x = element_blank(),
          axis.text.x= element_blank()) +
    annotate("text", x=x_coord_1, y=50, label= LEGEND, colour = COLOR, size = 20) 

  plt
}

