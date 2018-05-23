## Plot mechanical properties vs score by patient
library(tidyverse)
library(cowplot)

source('~/RProjects/cartilage_human_diffusion_study/import_data.R')
dir_fig_out <- "results/figures"
df_sample$pat = fct_recode(df_sample$pat,"Patient 1" = "1","Patient 2" = "2","Patient 3" = "3","Patient 4" = "4","Patient 5" = "5")

p1 <- ggplot(df_sample, aes(x = score, y = modcomp,color = pat)) +
  geom_jitter(width = 0.025, height = 0, shape = 16, size = 1) +
  geom_smooth(method = "lm", size = 0.5) +
  facet_wrap(~pat,nrow = 1) + 
  scale_x_continuous(breaks = seq(0, 2, by = 1)) +
  scale_color_discrete(guide = FALSE) +
  theme_grey() + 
  ylab(Ey["-"]~(MPa)) +
  xlab("ICRS Score") 

p2 <- ggplot(df_sample, aes(x = score, y = modten,color = pat)) +
  geom_jitter(width = 0.025, height = 0, shape = 16, size = 1) +
  geom_smooth(method = "lm", size = 0.5) +
  facet_wrap(~pat,nrow = 1) + 
  scale_x_continuous(breaks = seq(0, 2, by = 1)) +
  scale_color_discrete(guide = FALSE) +
  theme_grey() + 
  ylab(Ey["+"]~(MPa)) +
  xlab("ICRS Score")

p3 <- ggplot(df_sample, aes(x = score, y = perm_k0,color = pat)) +
  geom_jitter(width = 0.025, height = 0, shape = 16, size = 1) +
  geom_smooth(method = "lm", size = 0.5) +
  facet_wrap(~pat,nrow = 1) + 
  scale_x_continuous(breaks = seq(0, 2, by = 1)) +
  scale_color_discrete(guide = FALSE) +
  theme_grey() + 
  ylab(expression(k[0]~(mm^4~N/s))) +
  xlab("ICRS Score")

p4 <- ggplot(df_sample, aes(x = score, y = perm_m,color = pat)) +
  geom_jitter(width = 0.025, height = 0, shape = 16, size = 1) +
  geom_smooth(method = "lm", size = 0.5) +
  facet_wrap(~pat,nrow = 1) + 
  scale_x_continuous(breaks = seq(0, 2, by = 1)) +
  scale_color_discrete(guide = FALSE) +
  theme_grey() + 
  ylab(expression(m)) +
  xlab("ICRS Score")

pgrid <- plot_grid(p1,p2,p3,p4, ncol = 1, labels="AUTO")
pth <- file.path(dir_fig_out, "mech_vs_score_by_patient.pdf")
ggsave(pth, pgrid, width = 7, height = 8, units = "in")
