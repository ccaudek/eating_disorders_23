setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set wd to this file path

# load pkgs ====
library("tidybayes"); library("tidyverse")

# load data (extracted ddm model traces) ====
load("ddm/traces.Rda")

# pos_alpha_an_f <- pos_alpha_long %>% 
#   dplyr::filter(
#     diag_cat == "AN" & stim == "food"
#     )
# 
# pos_alpha_hc_f <- pos_alpha_long %>% 
#   dplyr::filter(
#     diag_cat == "HC" & stim == "food"
#   )
# 
# # This corresponds exactly to what I get in Python
# mean(pos_alpha_an_f$val > pos_alpha_hc_f$val)



# THRESHOLD (a) ====
# marginal posterior median and CI95
pos_alpha_long %>% 
  group_by(diag_cat, stim, draw) %>% 
  summarize(M=mean(val)) %>% 
  tidybayes::median_hdi(M)

# compute contrasts
pos_alpha_contrasts =
  pos_alpha_long %>% 
  group_by(diag_cat, stim, draw) %>% 
  summarize(M=mean(val)) %>% 
  pivot_wider(names_from = c(diag_cat, stim), values_from = M, names_prefix = "m") %>% 
  mutate(
    an0 = mAN_food - mAN_neutral,
    hc0 = mHC_food - mHC_neutral,
    ahf = mAN_food - mHC_food,
    ahn = mAN_neutral - mHC_neutral) %>% 
  pivot_longer(cols = c(an0, hc0, ahf, ahn), names_to = "contrast", values_to = "delta")

# keep just slow vs silence and fast vs silence contrasts
pos_alpha_contrasts2 =
  pos_alpha_contrasts %>% 
  filter(contrast != c("ahf", "ahn")) %>% 
  select(delta, draw, contrast) %>% 
  mutate(
    contrast=factor(contrast, levels=c("an0", "hc0")))

# compute posterior summaries
pos_alpha_contrasts_summ =
  pos_alpha_contrasts %>% 
  group_by(contrast) %>% 
  median_hdi(delta)

# keep just slow vs silence and fast vs silence contrasts
pos_alpha_contrasts_summ2 =
  pos_alpha_contrasts_summ %>% 
  filter(contrast!= c("ahf", "ahn")) %>% 
  mutate(ys = c(4, 2),
         contrast=factor(contrast, levels=c("an0", "hc0")))

# plot threshold contrasts (fig 4a)
tempo_cols=c("#E69F00", "#56B4E9")
hdiwd=5
psz=15
fsz=60
alf=.8

ggplot() +
  scale_fill_manual(values=tempo_cols) +
  scale_color_manual(values=tempo_cols) +
  # ref line
  geom_segment(aes(x = 0, y =-0.2, xend = 0, yend = 56),
               colour = "grey", lwd = 5 , lty=1, alpha =.8) +
  # DENSITIES
  stat_halfeye(data=pos_alpha_contrasts2, aes(y=1,x=delta, fill=contrast),
               .width=0, alpha=alf, lwd=0,scale = 56) +
  # HDI
  geom_segment(data=pos_alpha_contrasts_summ2, 
               aes( y=-1-ys,
                    yend=-1-ys,
                    x=.lower, 
                    xend=.upper),
               lwd=hdiwd, lineend = "round") +
  #MEDIAN
  geom_point(data=pos_alpha_contrasts_summ2, aes(x=delta,y=-1-ys), 
             size=psz,shape=21,color="black", fill="black")+
  geom_point(data=pos_alpha_contrasts_summ2, aes(x=delta,y=-1-ys), 
             size=psz-4,shape=21, fill="white")+
  geom_point(data=pos_alpha_contrasts_summ2, aes(x=delta,y=-1-ys, fill=contrast), 
             alpha=.75,size=psz-4,shape=21)+
  geom_hline(yintercept = -0.2,lwd=1, lty=1)+
  labs(y= "", x= "") +
  coord_flip() +
  theme_classic() +
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(), 
        panel.spacing.x = unit(0,"line"),
        text = element_text(size=fsz),
        legend.position="none",
        axis.line.y = element_blank(),
        axis.line.x = element_blank(),
        axis.text.x= element_blank())

# ggsave('figures/ddm/a_param_grp.png', width = 18, height = 10, dpi = 300)


# THRESHOLD (a) ====
# marginal posterior median and CI95

# keep just slow vs silence and fast vs silence contrasts
pos_alpha_contrasts2 =
  pos_alpha_contrasts %>% 
  filter(contrast != c("an0", "hc0")) %>% 
  select(delta, draw, contrast) %>% 
  mutate(
    contrast=factor(contrast, levels= c("ahf", "ahn")))

# compute posterior summaries
pos_alpha_contrasts_summ =
  pos_alpha_contrasts %>% 
  group_by(contrast) %>% 
  median_hdi(delta)

# keep just slow vs silence and fast vs silence contrasts
pos_alpha_contrasts_summ2 =
  pos_alpha_contrasts_summ %>% 
  filter(contrast!= c("an0", "hc0")) %>% 
  mutate(ys = c(4, 2),
         contrast=factor(contrast, levels= c("ahf", "ahn")))

# plot threshold contrasts (fig 4a)
tempo_cols=c("#E69F00", "#56B4E9")
hdiwd=5
psz=15
fsz=60
alf=.8

ggplot() +
  scale_fill_manual(values=tempo_cols) +
  scale_color_manual(values=tempo_cols) +
  # ref line
  geom_segment(aes(x = 0, y =-0.2, xend = 0, yend = 56),
               colour = "grey", lwd = 5 , lty=1, alpha =.8) +
  # DENSITIES
  stat_halfeye(data=pos_alpha_contrasts2, aes(y=1,x=delta, fill=contrast),
               .width=0, alpha=alf, lwd=0,scale = 56) +
  # HDI
  geom_segment(data=pos_alpha_contrasts_summ2, 
               aes( y=-1-ys,
                    yend=-1-ys,
                    x=.lower, 
                    xend=.upper),
               lwd=hdiwd, lineend = "round") +
  #MEDIAN
  geom_point(data=pos_alpha_contrasts_summ2, aes(x=delta,y=-1-ys), 
             size=psz,shape=21,color="black", fill="black")+
  geom_point(data=pos_alpha_contrasts_summ2, aes(x=delta,y=-1-ys), 
             size=psz-4,shape=21, fill="white")+
  geom_point(data=pos_alpha_contrasts_summ2, aes(x=delta,y=-1-ys, fill=contrast), 
             alpha=.75,size=psz-4,shape=21)+
  geom_hline(yintercept = -0.2,lwd=1, lty=1)+
  labs(y= "", x= "") +
  coord_flip() +
  theme_classic() +
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(), 
        panel.spacing.x = unit(0,"line"),
        text = element_text(size=fsz),
        legend.position="none",
        axis.line.y = element_blank(),
        axis.line.x = element_blank(),
        axis.text.x= element_blank())

# ggsave('figures/ddm/a_param_grp.png', width = 18, height = 10, dpi = 300)
































# fast vs slow music contrast  (figure-4b)
pos_alpha_contrasts3 =
  pos_alpha_contrasts %>% 
  filter(contrast=="ah") %>% 
  select(delta, draw, contrast)

pos_alpha_contrasts_summ3 =
  pos_alpha_contrasts_summ %>% 
  filter(contrast=="ah")

ggplot() +
  # ref line
  geom_vline (xintercept = 0,lwd=1.5, lty=2)+
  
  # #DENSITY
  stat_halfeyeh(data=pos_alpha_contrasts3,aes(y=0,x=delta), 
                fill=tempo_cols[1],.width=0, alpha=alf,lwd=0,scale = 1)+
  # HDI
  geom_segment(data=pos_alpha_contrasts_summ3, 
               aes( y=0,yend=0,
                    x=.lower,xend=.upper),
               lwd=6, lineend = "round")+
  #MEDIAN
  geom_point(data=pos_alpha_contrasts_summ3, aes(x=delta,y=0), 
             size=psz,shape=21,color="black", fill="black")+
  geom_point(data=pos_alpha_contrasts_summ3, aes(x=delta,y=0), 
             size=psz-4,shape=21, fill="white")+
  labs(y= "", x= "") +
  theme_classic() +
  coord_flip()+
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(), 
        panel.spacing.x = unit(0,"line"),
        text = element_text(size=fsz+10),
        legend.position="none",
        axis.line.y = element_blank(),
        axis.line.x = element_blank(),
        axis.text.x= element_blank())
# ggsave('figures/ddm/a-SvsF.png', width = 18, height = 10, dpi = 300)

