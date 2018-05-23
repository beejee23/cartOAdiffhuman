## Plot Diffusion coefficients vs ICRS score, grouped by ICS and Solute
library(tidyverse)
library(forcats)
library(plyr)

source('~/RProjects/cartilage_human_diffusion_study/import_data.R')
dir_fig_out <- "results/figures"

df_plot <- df_sample

# Rename columns
df_plot <- rename(df_plot, c("3k_FCS"="FCS_3k","3k_RICS"="RICS_3k","FITC_FCS"="FCS_FITC","FITC_RICS"="RICS_FITC"))

# Gather diffusion measures into one column
df_plot <- df_plot %>%
  gather("anom_FITC","anom_3k","FCS_3k","RICS_3k","FCS_FITC","RICS_FITC", key = "measure", value = "diff")
# Separate into ICS measurement and solute type as factors
df_plot <- df_plot %>% 
  separate(measure, into = c("ICS","sol"))
df_plot$ICS <- as_factor(df_plot$ICS)
df_plot$sol <- as_factor(df_plot$sol)
levels(df_plot$sol) <- c(levels(df_plot$sol)[1],"'3k Dextran'")
levels(df_plot$ICS) <- c("FCS~(alpha)", "FCS~(Gamma)", "RICS~(D)")

# Build Figure
p_diff_vs_score <- ggplot(df_plot, aes(x = score, y = diff,shape = ICS,color = sol)) + 
  geom_jitter(width = 0.05, height = 0, size = 1) +
  geom_smooth(method = "lm", size = 0.5) +
  scale_x_continuous(breaks = seq(0, 2, by = 1)) +
  facet_wrap(sol ~ ICS, scales = "free",labeller=label_parsed) +
  theme_bw() +
  theme(
    text = element_text(size = 10),
    strip.text = element_text(face = "bold"),
    strip.background = element_rect(fill = "white"),
    legend.position = "right",
    legend.box.background = element_rect(fill = "white"),
    legend.title.align = 0.5,
    legend.text.align = 0,
    legend.key = element_rect(fill = "white"),
    panel.spacing = unit(2, "lines")
  ) +
  guides(color=guide_legend(override.aes=list(fill=NA))) +
  scale_colour_brewer(palette = "Dark2", name = "Solute",
                      labels = c("FITC","3k Dextran")) +
  scale_shape_discrete(labels = c(expression(FCS~(alpha)), expression(FCS~(Gamma)), expression(RICS~(D)))) +
  ylab((Diffusion~Coefficient~(mm^2/s))) +
  xlab("ICRS Score")

# Save Figure
pth <- file.path(dir_fig_out, "diff_vs_score_anom.pdf")
ggsave(pth, p_diff_vs_score, width = 8, height = 4, units = "in")