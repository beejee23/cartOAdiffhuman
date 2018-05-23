## Plot mechanical properties vs score
library(tidyverse)
library(RColorBrewer)
library(corrplot)

source('~/RProjects/cartilage_human_diffusion_study/import_data.R')
dir_fig_out <- "~/RProjects/cartilage_human_diffusion_study/results/figures"

# get only double values
df_dbonly <- select(df_sample,modcomp:FITC_RICS,-mechr2)

# Rename columns
names(df_dbonly) <- c("Compressive Modulus","Tensile Modulus","Permeability M","Permeability k0",
                      "GAG","Hydroxyproline","Anom. FITC / FCS","Anom 3k / FCS",
                      "Diff. 3k / FCS","Diff. 3k / RICS","Diff. FITC / FCS","Diff. FITC / RICS")

# Reorder columns for plotting
df_dbonly <- df_dbonly[,c(7,11,12,8,9,10,1,2,3,4,6,5)]

# Correlation matrix and p values
M <- cor(df_dbonly)
Mtest <- cor.mtest(df_dbonly, conf.level = .95)

# Create graphics file
pth <- file.path(dir_fig_out, "correlation_matrix.pdf")
pdf(pth)

# Generate plot
corrplot(M,type = "upper",method = "color",order ="original",col = brewer.pal(n = 8, name = "RdBu"),
         tl.col = "black", tl.cex = .7,number.cex = .7,addCoef.col = "black",
          p.mat = Mtest$p, sig.level = 0.05, insig = "blank", pch.col = "black",tl.pos = "lt")
# corrplot(M,add = TRUE, type = "lower",method = "ellipse",order ="hclust",
#          tl.col = "black", tl.cex = .7,number.cex = .7,tl.pos = "lt",
#           p.mat = Mtest$p, sig.level = 0.05, insig = "p-value", pch.col = "black",pch.cex = .5)
corrplot(M,add = TRUE, type = "lower",method = "ellipse",order ="original",col = brewer.pal(n = 8, name = "RdBu"),
         tl.col = "black", tl.cex = .7,number.cex = .7,tl.pos = "lt")

# Save/close 
dev.off()
