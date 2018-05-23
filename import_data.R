# Import and Tidy the data for the paper

# load dependencies
library(tidyverse)
library(readxl)
library(forcats)

# Import raw data file
fullpath <- "~/Dropbox/Manuscripts/2018 FCS and RICS/data_and_stats/HumanCartilageStudy_Patients1_5.xlsx"
df <- read_excel(fullpath)

# Rename columns
namestr <- c("pat","plug","score","FCS_FITC","RICS_FITC","FCS_3k","RICS_3k",
             "modcomp","modten","perm_m","perm_k0","mechr2",
             "gag","hydrox","anom_FITC","anom_3k")
names(df) <- namestr

# Convert to factors
df$pat <-as_factor(df$pat)
df$plug <- as.character(df$plug) %>% as_factor()
#df$score <- as.character(df$score) %>% ordered()

# Reorder patient levels
levels(df$pat) <- levels(df$pat)[c(5,3,4,1,2)]
df$pat <- fct_recode(df$pat,"1" = "0614.1","2" = "0614.2","3" = "0614.4","4" = "0621.1","5" = "0621.3")

# Gather diffusion measurements in a single column
df_anom <- df %>% gather("anom_FITC","anom_3k", key = "ICS_molecule",value = "anom")
df <- df %>% gather("FCS_FITC","RICS_FITC","FCS_3k","RICS_3k", key = "ICS_molecule",value = "diff")

#df %>% spread(key = ICS,value = diff)

# Separate ICS technique and solute. Covnert to factors
df <- df %>% 
  separate(ICS_molecule, into = c("ICS","sol"))
df$ICS <-as_factor(df$ICS)
df$sol <-as_factor(df$sol)

# rearrange column order
df <- df %>%
  select(pat:score,ICS:diff,everything())

# Get data frame used to compare values obtained via the two ICS techniques
df_ICS <- spread(df, key = ICS, value = diff);

# Spread data so each sample has one entry
df_sample <-  df %>% 
  unite("sol_ICS",c("sol","ICS")) %>% 
  spread(key = sol_ICS, value = diff)