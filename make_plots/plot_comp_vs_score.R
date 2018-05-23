## Plot compositional properties vs score
library(tidyverse)
library(cowplot)

source('~/RProjects/cartilage_human_diffusion_study/import_data.R')
dir_fig_out <- "~/RProjects/cartilage_human_diffusion_study/results/figures"

## Plot hydrox
pa <- ggplot(df_sample, aes(x = score, y = hydrox)) +
  geom_jitter(width = 0.05, height = 0) +
  geom_smooth(method = "lm") +
  scale_x_continuous(breaks = seq(0, 2, by = 1)) +
  theme_bw() +
  ylab((Hydroxyproline~Content~'(wt%)')) +
  xlab("ICRS Score") 

## Plot gag
pb <- ggplot(df_sample, aes(x = score, y = gag)) +
  geom_jitter(width = 0.05, height = 0) +
  geom_smooth(method = "lm") +
  scale_x_continuous(breaks = seq(0, 2, by = 1)) +
  theme_bw() +
  ylab((GAG~Content~'(wt%)')) +
  xlab("ICRS Score") 


pgrid <- plot_grid(pa, pb, nrow = 1, labels="AUTO")
pth <- file.path(dir_fig_out, "comp_vs_score.pdf")
ggsave(pth, pgrid, width = 8, height = 3, units = "in")