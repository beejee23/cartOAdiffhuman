## Plot Diffusion coefficients vs ICRS score, grouped by ICS and Solute
library(tidyverse)

source('~/RProjects/cartilage_human_diffusion_study/import_data.R')
dir_fig_out <- "results/figures"

# Rename levels for plotting
levels(df$sol) <- c(levels(df$sol)[1],"3k Dextran")

# Build Figure
p_diff_vs_score <- ggplot(df, aes(x = score, y = diff,shape = ICS,color = sol)) + 
  geom_jitter(width = 0.05, height = 0) +
  geom_smooth(method = "lm") +
  scale_x_continuous(breaks = seq(0, 2, by = 1)) +
  facet_wrap(sol ~ ICS, scales = "free") +
  theme_bw() +
  theme(
    strip.text = element_text(face = "bold"),
    strip.background = element_rect(fill = "white"),
    legend.position = "top",
    legend.box.background = element_rect(),
    legend.title.align = 0.5,
    panel.spacing = unit(2, "lines")
  ) +
  scale_colour_brewer(palette = "Dark2", name = "Solute") +
  ylab((Diffusion~Coefficient~(mm^2/s))) +
  xlab("ICRS Score") 

# Save Figure
pth <- file.path(dir_fig_out, "diff_vs_score.pdf")
ggsave(pth, p_diff_vs_score, width = 6, height = 5, units = "in")