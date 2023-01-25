# Script name: 98_working_on_all_params.R
# Project: Eating disorders Montecatini
# Script purpose: Generate figures for HDDMrl parameters.
# @author: Corrado Caudek <corrado.caudek@unifi.it>
# Date Created: Thu Jun 16 07:14:08 2022
# Last Modified Date: Thu Jun 16 07:14:08 2022
#
# 👉 This scritp is a modified version of ddm_inference.R found at
#    https://osf.io/9v3sy/


suppressPackageStartupMessages({
  library("rprojroot")
  root <- has_file("01_TODO_ED_MONTECATINI.Rproj")$make_fix_file()
  library("tidyverse")
  library("tidybayes")
})

# Load data (extracted ddm model traces).
load(root("scripts/python/PRL/01_analysis/ddm", "traces.Rda"))

# Choose parameter to plot.
par_long <- pos_alpha_long

# Marginal posterior median and CI95.
par_long %>%
  group_by(diag_cat, stim, draw) %>%
  summarize(M = mean(val)) %>%
  tidybayes::median_hdi(M)

# compute contrasts
par_contrasts <-
  par_long %>%
  group_by(diag_cat, stim, draw) %>%
  summarize(M = mean(val)) %>%
  pivot_wider(names_from = c(diag_cat, stim), values_from = M, names_prefix = "m") %>%
  mutate(
    an0 = mAN_food - mAN_neutral,
    hc0 = mHC_food - mHC_neutral,
    ahf = mAN_food - mHC_food,
    ahn = mAN_neutral - mHC_neutral
  ) %>%
  pivot_longer(cols = c(an0, hc0, ahf, ahn), names_to = "contrast", values_to = "delta")

# keep just slow vs silence and fast vs silence contrasts
par_contrasts2 <-
  par_contrasts %>%
  filter(contrast != c("ahf", "ahn")) %>%
  select(delta, draw, contrast) %>%
  mutate(
    contrast = factor(contrast, levels = c("an0", "hc0"))
  )

# compute posterior summaries
par_contrasts_summ <-
  par_contrasts %>%
  group_by(contrast) %>%
  median_hdi(delta)

# keep just slow vs silence and fast vs silence contrasts
par_contrasts_summ2 <-
  par_contrasts_summ %>%
  filter(contrast != c("ahf", "ahn")) %>%
  mutate(
    ys = c(5, 2),
    contrast = factor(contrast, levels = c("an0", "hc0"))
  )

# plot threshold contrasts (fig 4a)

okabe <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

tempo_cols <- c("#E69F00", "#56B4E9")
hdiwd <- 5
psz <- 15
fsz <- 60
alf <- .8

ggplot() +
  scale_fill_manual(values = tempo_cols) +
  scale_color_manual(values = tempo_cols) +
  # ref line
  geom_segment(aes(x = 0, y = -0.2, xend = 0, yend = 56),
    colour = "grey", lwd = 5, lty = 1, alpha = .8
  ) +
  # DENSITIES
  stat_halfeye(
    data = par_contrasts2, aes(y = 1, x = delta, fill = contrast),
    .width = 0, alpha = alf, lwd = 0, scale = 56
  ) +
  # HDI
  geom_segment(
    data = par_contrasts_summ2,
    aes(
      y = -1 - ys,
      yend = -1 - ys,
      x = .lower,
      xend = .upper
    ),
    lwd = hdiwd, lineend = "round"
  ) +
  # MEDIAN
  geom_point(
    data = par_contrasts_summ2, aes(x = delta, y = -1 - ys),
    size = psz, shape = 21, color = "black", fill = "black"
  ) +
  geom_point(
    data = par_contrasts_summ2, aes(x = delta, y = -1 - ys),
    size = psz - 4, shape = 21, fill = "white"
  ) +
  geom_point(
    data = par_contrasts_summ2, aes(x = delta, y = -1 - ys, fill = contrast),
    alpha = .75, size = psz - 4, shape = 21
  ) +
  geom_hline(yintercept = -0.2, lwd = 1, lty = 1) +
  labs(y = "", x = "") +
  coord_flip() +
  theme_classic() +
  theme(
    panel.background = element_blank(),
    panel.grid = element_blank(),
    panel.spacing.x = unit(0, "line"),
    text = element_text(size = fsz),
    legend.position = "none",
    axis.line.y = element_blank(),
    axis.line.x = element_blank(),
    axis.text.x = element_blank()
  )

# ggsave('figures/ddm/a_param_grp.png', width = 18, height = 10, dpi = 300)
ggsave("v_param_grp_1.pdf", width = 12, height = 10, dpi = 300)


# THRESHOLD (a) ====
# marginal posterior median and CI95

# keep just slow vs silence and fast vs silence contrasts
par_contrasts2 <-
  par_contrasts %>%
  filter(contrast != c("an0", "hc0")) %>%
  select(delta, draw, contrast) %>%
  mutate(
    contrast = factor(contrast, levels = c("ahf", "ahn"))
  )

# compute posterior summaries
par_contrasts_summ <-
  par_contrasts %>%
  group_by(contrast) %>%
  median_hdi(delta)

# keep just slow vs silence and fast vs silence contrasts
par_contrasts_summ2 <-
  par_contrasts_summ %>%
  filter(contrast != c("an0", "hc0")) %>%
  mutate(
    ys = c(6, 2),
    contrast = factor(contrast, levels = c("ahf", "ahn"))
  )

# plot threshold contrasts (fig 4a)
tempo_cols <- c("#0072B2", "#D55E00")
hdiwd <- 5
psz <- 15
fsz <- 60
alf <- .8

ggplot() +
  scale_fill_manual(values = tempo_cols) +
  scale_color_manual(values = tempo_cols) +
  # ref line
  geom_segment(aes(x = 0, y = -0.2, xend = 0, yend = 56),
    colour = "grey", lwd = 5, lty = 1, alpha = .8
  ) +
  # DENSITIES
  stat_halfeye(
    data = par_contrasts2, aes(y = 1, x = delta, fill = contrast),
    .width = 0, alpha = alf, lwd = 0, scale = 56
  ) +
  # HDI
  geom_segment(
    data = par_contrasts_summ2,
    aes(
      y = -1 - ys,
      yend = -1 - ys,
      x = .lower,
      xend = .upper
    ),
    lwd = hdiwd, lineend = "round"
  ) +
  # MEDIAN
  geom_point(
    data = par_contrasts_summ2, aes(x = delta, y = -1 - ys),
    size = psz, shape = 21, color = "black", fill = "black"
  ) +
  geom_point(
    data = par_contrasts_summ2, aes(x = delta, y = -1 - ys),
    size = psz - 4, shape = 21, fill = "white"
  ) +
  geom_point(
    data = par_contrasts_summ2, aes(x = delta, y = -1 - ys, fill = contrast),
    alpha = .75, size = psz - 4, shape = 21
  ) +
  geom_hline(yintercept = -0.2, lwd = 1, lty = 1) +
  labs(y = "", x = "") +
  coord_flip() +
  theme_classic() +
  theme(
    panel.background = element_blank(),
    panel.grid = element_blank(),
    panel.spacing.x = unit(0, "line"),
    text = element_text(size = fsz),
    legend.position = "none",
    axis.line.y = element_blank(),
    axis.line.x = element_blank(),
    axis.text.x = element_blank()
  )

# ggsave('figures/ddm/a_param_grp.png', width = 18, height = 10, dpi = 300)
ggsave("v_param_grp_2.pdf", width = 12, height = 10, dpi = 300)
