## Plot mechanical properties vs score
library(tidyverse)
library(cowplot)

source('~/RProjects/cartilage_human_diffusion_study/import_data.R')
dir_fig_out <- "~/RProjects/cartilage_human_diffusion_study/results/figures"

## Plot modcomp
pa <- ggplot(df_sample, aes(x = score, y = modcomp)) +
  geom_jitter(width = 0.05, height = 0) +
  geom_smooth(method = "lm") +
  scale_x_continuous(breaks = seq(0, 2, by = 1)) +
  theme_bw() +
  ylab((Compressive~Modulus~(MPa))) +
  xlab("ICRS Score") 

## Plot modten
pb <- ggplot(df_sample, aes(x = score, y = modten)) +
  geom_jitter(width = 0.05, height = 0) +
  geom_smooth(method = "lm") +
  scale_x_continuous(breaks = seq(0, 2, by = 1)) +
  theme_bw() +
  ylab((Tensile~Modulus~(MPa))) +
  xlab("ICRS Score") 

## Plot perm_k0
pc <- ggplot(df_sample, aes(x = score, y = perm_k0)) +
  geom_jitter(width = 0.05, height = 0) +
  geom_smooth(method = "lm") +
  scale_x_continuous(breaks = seq(0, 2, by = 1)) +
  theme_bw() +
  ylab(expression(Permeability~k[0]~(mm^4~N/s))) +
  xlab("ICRS Score") 

## Plot perm_m
pd <- ggplot(df_sample, aes(x = score, y = perm_m)) +
  geom_jitter(width = 0.05, height = 0) +
  geom_smooth(method = "lm") +
  scale_x_continuous(breaks = seq(0, 2, by = 1)) +
  theme_bw() +
  ylab(expression(Permeability~m)) +
  xlab("ICRS Score") 

pgrid <- plot_grid(pa, pb, pc, pd, nrow = 2, labels="AUTO")
pth <- file.path(dir_fig_out, "mech_vs_score.pdf")
ggsave(pth, pgrid, width = 8, height = 6, units = "in")