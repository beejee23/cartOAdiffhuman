## Plot diffusive properties vs score by patient
library(tidyverse)
library(cowplot)
library(forcats)

source('~/RProjects/cartilage_human_diffusion_study/import_data.R')
dir_fig_out <- "~/RProjects/cartilage_human_diffusion_study/results/figures"
df_sample$pat = fct_recode(df_sample$pat,"Patient 1" = "1","Patient 2" = "2","Patient 3" = "3","Patient 4" = "4","Patient 5" = "5")

p1 <- ggplot(df_sample, aes(x = score, y = anom_FITC,color = pat)) +
  geom_jitter(width = 0.025, height = 0, shape = 16, size = 1) +
  geom_smooth(method = "lm", size = 0.5) +
  facet_wrap(~pat,nrow = 1) + 
  scale_x_continuous(breaks = seq(0, 2, by = 1)) +
  scale_color_discrete(guide = FALSE) +
  coord_cartesian(ylim = c(0.2, 0.9)) +
  theme_grey() + 
  ylab(('FITC - '~alpha)) +
  xlab("ICRS Score") 

p2 <- ggplot(df_sample, aes(x = score, y = FITC_FCS,color = pat)) +
  geom_jitter(width = 0.025, height = 0, shape = 16, size = 1) +
  geom_smooth(method = "lm", size = 0.5) +
  facet_wrap(~pat,nrow = 1) + 
  scale_x_continuous(breaks = seq(0, 2, by = 1)) +
  scale_color_discrete(guide = FALSE) +
  coord_cartesian(ylim = c(95, 170)) +
  theme_grey() + 
  ylab(('FITC - '~Gamma~(mm^{2}/s))) +
  xlab("ICRS Score")

p3 <- ggplot(df_sample, aes(x = score, y = FITC_RICS,color = pat)) +
  geom_jitter(width = 0.025, height = 0, shape = 16, size = 1) +
  geom_smooth(method = "lm", size = 0.5) +
  facet_wrap(~pat,nrow = 1) + 
  scale_x_continuous(breaks = seq(0, 2, by = 1)) +
  scale_color_discrete(guide = FALSE) +
  coord_cartesian(ylim = c(95, 170)) +
  theme_grey() + 
  ylab(('FITC - '~D~(mm^{2}/s))) +
  xlab("ICRS Score")

pgrid <- plot_grid(p1,p2,p3, ncol = 1, labels="AUTO")
pth <- file.path(dir_fig_out, "diff_vs_score_by_patient_FITC.pdf")
ggsave(pth, pgrid, width = 7, height = 6, units = "in")

p4 <- ggplot(df_sample, aes(x = score, y = anom_3k,color = pat)) +
  geom_jitter(width = 0.025, height = 0, shape = 17, size = 1) +
  geom_smooth(method = "lm", size = 0.5) +
  facet_wrap(~pat,nrow = 1) + 
  scale_x_continuous(breaks = seq(0, 2, by = 1)) +
  scale_color_discrete(guide = FALSE) +
  coord_cartesian(ylim = c(0.2, 0.9)) +
  theme_grey() + 
  ylab(('3k - '~alpha)) +
  xlab("ICRS Score") 

p5 <- ggplot(df_sample, aes(x = score, y = `3k_FCS`,color = pat)) +
  geom_jitter(width = 0.025, height = 0, shape = 17, size = 1) +
  geom_smooth(method = "lm", size = 0.5) +
  facet_wrap(~pat,nrow = 1) + 
  scale_x_continuous(breaks = seq(0, 2, by = 1)) +
  scale_color_discrete(guide = FALSE) +
  coord_cartesian(ylim = c(25, 65)) +
  theme_grey() + 
  ylab(('3k - '~Gamma~(mm^{2}/s))) +
  xlab("ICRS Score")

p6 <- ggplot(df_sample, aes(x = score, y = `3k_RICS`,color = pat)) +
  geom_jitter(width = 0.025, height = 0, shape = 17, size = 1) +
  geom_smooth(method = "lm", size = 0.5) +
  facet_wrap(~pat,nrow = 1) + 
  scale_x_continuous(breaks = seq(0, 2, by = 1)) +
  scale_color_discrete(guide = FALSE) +
  coord_cartesian(ylim = c(25, 65)) +
  theme_grey() + 
  ylab(('3k - '~D~(mm^{2}/s))) +
  xlab("ICRS Score") 

pgrid <- plot_grid(p4,p5,p6, ncol = 1, labels="AUTO")
pth <- file.path(dir_fig_out, "diff_vs_score_by_patient_3k.pdf")
ggsave(pth, pgrid, width = 7, height = 6, units = "in")