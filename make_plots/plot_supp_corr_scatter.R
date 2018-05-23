## Plot mechanical properties vs score
library(tidyverse)
library(cowplot)

source('~/RProjects/cartilage_human_diffusion_study/import_data.R')
dir_fig_out <- "~/RProjects/cartilage_human_diffusion_study/results/figures"

corr_eqn <- function(x,y, digits = 3) {
  sum_stats <- cor.test(x, y, method = "pearson",conf.level = 0.95)
  corr_val <- round(sum_stats$estimate, digits = digits)
  p_val <- round(sum_stats$p.value, digits = digits)
  if (p_val < 0.001) {
    textlabel <- paste("R = ", corr_val,"   p < 0.001")
  }
  else {
    textlabel <- paste("R = ", corr_val,"   p = ", p_val)
  }
  return(textlabel)
}

## Plot modcomp
pa <- ggplot(df_sample, aes(x = modcomp, y = modten)) +
  geom_point() +
  geom_smooth(method="lm") +
  theme_bw() +
  ylab(Tensile~Modulus~(MPa)) +
  xlab(Compressive~Modulus~(MPa)) +
  labs(title = corr_eqn(df_sample$modcomp,df_sample$modten)) +
  theme(plot.title = element_text(hjust = 0.5, size = 10)) 
  
pb <- ggplot(df_sample, aes(x = `3k_FCS`, y = FITC_FCS)) +
  geom_point() +
  geom_smooth(method="lm") +
  theme_bw() +
  ylab(FITC~-~FCS~-~Gamma~-~(mm^2/s)) +
  xlab(`3k`~-~FCS~-~Gamma~-~(mm^2/s)) +
  labs(title = corr_eqn(df_sample$`3k_FCS`,df_sample$FITC_FCS)) +
  theme(plot.title = element_text(hjust = 0.5, size = 10)) 

pc <- ggplot(df_sample, aes(x = perm_k0, y = FITC_RICS)) +
  geom_point() +
  geom_smooth(method="lm") +
  theme_bw() +
  ylab(FITC~-~RICS~-~D~-~(mm^2/s)) +
  xlab(Permeability~k[0]~(mm^4~N/s)) +
  labs(title = corr_eqn(df_sample$perm_k0,df_sample$FITC_RICS)) +
  theme(plot.title = element_text(hjust = 0.5, size = 10))  

pd <- ggplot(df_sample, aes(x = gag, y = FITC_FCS)) +
  geom_point() +
  geom_smooth(method="lm") +
  theme_bw() +
  ylab(FITC~-~FCS~-~Gamma~-~(mm^2/s)) +
  xlab(GAG~Content~'(wt%)') +
  labs(title = corr_eqn(df_sample$gag,df_sample$FITC_FCS)) +
  theme(plot.title = element_text(hjust = 0.5, size = 10))  

pe <- ggplot(df_sample, aes(x = hydrox, y = FITC_FCS)) +
  geom_point() +
  geom_smooth(method="lm") +
  theme_bw() +
  ylab(FITC~-~FCS~-~Gamma~-~(mm^2/s)) +
  xlab(Hydroxyproline~Content~'(wt%)') +
  labs(title = corr_eqn(df_sample$hydrox,df_sample$FITC_FCS)) +
  theme(plot.title = element_text(hjust = 0.5, size = 10))  

pf <- ggplot(df_sample, aes(x = hydrox, y = gag)) +
  geom_point() +
  geom_smooth(method="lm") +
  theme_bw() +
  ylab(Hydroxyproline~Content~'(wt%)') +
  xlab(GAG~Content~'(wt%)') +
  labs(title = corr_eqn(df_sample$hydrox,df_sample$gag)) +
  theme(plot.title = element_text(hjust = 0.5, size = 10))  

pgrid <- plot_grid(pa, pb, pc, pd, pe, pf, nrow = 3, labels="AUTO")
pth <- file.path(dir_fig_out, "supplemental_correlation_scatterplots.pdf")
ggsave(pth, pgrid, width = 8, height = 9, units = "in")
