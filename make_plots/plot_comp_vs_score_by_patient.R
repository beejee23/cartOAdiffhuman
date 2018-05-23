## Plot compositional properties vs score by patient
library(tidyverse)
library(cowplot)

source('~/RProjects/cartilage_human_diffusion_study/import_data.R')
dir_fig_out <- "~/RProjects/cartilage_human_diffusion_study/results/figures"
df_sample$pat = fct_recode(df_sample$pat,"Patient 1" = "1","Patient 2" = "2","Patient 3" = "3","Patient 4" = "4","Patient 5" = "5")

p1 <- ggplot(df_sample, aes(x = score, y = hydrox,color = pat)) +
  geom_jitter(width = 0.025, height = 0, shape = 16, size = 1) +
  geom_smooth(method = "lm", size = 0.5) +
  facet_wrap(~pat,nrow = 1) + 
  scale_x_continuous(breaks = seq(0, 2, by = 1)) +
  scale_color_discrete(guide = FALSE) +
  theme_grey() + 
  ylab(Hydroxyproline~"(wt%)") +
  xlab("ICRS Score") 

p2 <- ggplot(df_sample, aes(x = score, y = gag,color = pat)) +
  geom_jitter(width = 0.025, height = 0, shape = 16, size = 1) +
  geom_smooth(method = "lm", size = 0.5) +
  facet_wrap(~pat,nrow = 1) + 
  scale_x_continuous(breaks = seq(0, 2, by = 1)) +
  scale_color_discrete(guide = FALSE) +
  theme_grey() + 
  ylab(GAG~"(wt%)") +
  xlab("ICRS Score")

pgrid <- plot_grid(p1,p2, ncol = 1, labels="AUTO")
pth <- file.path(dir_fig_out, "comp_vs_score_by_patient.pdf")
ggsave(pth, pgrid, width = 7, height = 4.5, units = "in")