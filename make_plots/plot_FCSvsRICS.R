## Plot Diffusion coefficients measured by FCS vs RICS
library(tidyverse)
library(stringr)
library(car)


source('~/RProjects/cartilage_human_diffusion_study/import_data.R')
dir_fig_out <- "~/RProjects/cartilage_human_diffusion_study/results/figures"

# Rename levels for plotting
levels(df_ICS$sol) <- c(levels(df_ICS$sol)[1],"3k Dextran")

# Stats combined ICS
fit_ICS <- lm(RICS ~ FCS, data=df_ICS)
slope_ICS <- fit_ICS$coefficients[[2]]
int_ICS <- slope <- fit_ICS$coefficients[[1]]
pval_ICS <- anova(fit_ICS)$'Pr(>F)'[[1]]
rsquare_ICS <- summary(fit_ICS)$r.square
ICS_stats <- tibble(slope_ICS,int_ICS,pval_ICS,rsquare_ICS)

# Stats combined FITC
fit_FITC <- lm(RICS ~ FCS, data=filter(df_ICS, sol == 'FITC'))
slope_FITC <- fit_FITC$coefficients[[2]]
int_FITC <- slope <- fit_FITC$coefficients[[1]]
pval_FITC <- anova(fit_FITC)$'Pr(>F)'[[1]]
rsquare_FITC <- summary(fit_FITC)$r.square
FITC_stats <- tibble(slope_FITC,int_FITC,pval_FITC,rsquare_FITC)

# Stats combined 3k
fit_3k <- lm(RICS ~ FCS, data=filter(df_ICS, sol == '3k Dextran'))
slope_3k <- fit_3k$coefficients[[2]]
int_3k <- slope <- fit_3k$coefficients[[1]]
pval_3k <- anova(fit_3k)$'Pr(>F)'[[1]]
rsquare_3k <- summary(fit_3k)$r.square
dex3k_stats <- tibble(slope_3k,int_3k,pval_3k,rsquare_3k)

# Function to plot stats on the graph
lm_eqn = function(m) {
  
  line1 <- as.character(
    as.expression(
      substitute(
        italic(y) == b %.% italic(x) + a,
        list(a = format(coef(m)[1], digits = 2),
             b = format(abs(coef(m)[2]), digits = 2)))
    )
  )
  
  line2 <- as.character(
    as.expression(
      substitute(
        italic(r)^2~"="~r2,
        list(r2 = format(summary(m)$r.squared, digits = 3)))
    )
  )
  
  line3 <- as.character(
    as.expression(
      substitute(
        "p = "~p,
        list(p = format(anova(m)$'Pr(>F)'[[1]], digits = 3)))
    )
  )
  
  c(line1,line2,line3)
  
}

## Build figure
p <- ggplot(df_ICS, aes(x = FCS, y = RICS)) +
  geom_abline(intercept = 0, slope = 1, alpha = 0.2) +
  stat_smooth(method = "lm",formula = y ~ x) +
  geom_point(aes(shape = sol, color = sol),size = 3,alpha = 0.8) +
  theme_bw() +
  theme(
    legend.position = c(.2,.8),
    legend.box.background = element_rect(),
    legend.title.align = 0.5
    ) +
  scale_shape_discrete(name  ="Solute") +
  coord_cartesian(xlim = c(0, 170), ylim = c(0, 170)) +
  stat_smooth(aes(group = sol, color = sol),method = "lm",formula = y ~ x,fullrange = FALSE) +
  scale_colour_brewer(palette = "Dark2", name = "Solute") + 
  ylab((RICS~-~D~-~(mm^2/s))) + 
  xlab((FCS~-~Gamma~-~(mm^2/s)))

## Save Figure
pth <- file.path(dir_fig_out, "FCSvsRICS_two_fits.pdf")
ggsave(pth, p, width = 6, height = 5, units = "in")
p

ICS_stats
FITC_stats
dex3k_stats

linearHypothesis(fit_ICS,matrix(c(0,1),nrow=1),rhs=c(1))$'Pr(>F)'[[2]]
linearHypothesis(fit_FITC,matrix(c(0,1),nrow=1),rhs=c(1))$'Pr(>F)'[[2]]
linearHypothesis(fit_3k,matrix(c(0,1),nrow=1),rhs=c(1))$'Pr(>F)'[[2]]