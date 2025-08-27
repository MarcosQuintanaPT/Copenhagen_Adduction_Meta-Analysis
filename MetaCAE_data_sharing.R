# Project: Systematic Review and Meta-Analysis of the Effect of the
#  Copenhagen Adduction Exercise on Sport Performance and Injury Prevention
# Author: Marcos Quintana Cepedal
# Contact: marcosquintana99@gmail.com
# Last updated: 27-08-2025
# When loading the data, please check that the path to the data file is correct

# Meta-analysis of groin injury prevention---- 
# Load packages and data
library(meta)
library(readxl)

esinjury <- read_excel("effect_sizes_injury.xlsx")

# Initial meta-analysis, the study by Harøy is included 
t1 <- metabin(iginj, igtotal, cginj, cgtotal, studlab = Study, data = esinjury,
              method = "MH", sm = "RR", common = FALSE)

print(t1)

# Corrected meta-analysis, the study by Harøy et al., was removed
esinjb <- esinjury[-c(2), ]

# Run the meta-analysis again
t2 <- metabin(iginj, igtotal, cginj, cgtotal, studlab = Study, data = esinjb,
              method = "MH", sm = "RR", common = FALSE)

print(t2)


# Remove all objects from environment
rm(list = ls())


# Meta-analysis of adduction strength----
# Load packages
library(metafor)
library(dplyr)
library(meta)
library(readxl)

es <- read_excel("effect_sizes_add.xlsx")

# Calculate standard errors for meta-analysis with the metagen function
es$se <- sqrt(es$vi)

# Part df to separate between-groups and within-groups meta-analyses
eswithin <- filter(es, ma2 == "within")
esbetween <- filter(es, ma2 == "between")

# Overall within-group dataframe for publication bias
pubiaswithin <- filter(eswithin, MA == "Overall")

# Perform meta-analysis for within-group comparison
mawithin <- metagen(TE = yi, 
                    seTE = se,
                    data = eswithin, 
                    studlab = Study, 
                    sm = "SMD",
                    method.tau = "REML",
                    common = FALSE,
                    subgroup = MA)

print(mawithin)

# Perform meta-analysis for between-group comparison
mabetween <- metagen(TE = yi, 
                    seTE = se,
                    data = esbetween, 
                    studlab = Study, 
                    sm = "SMD",
                    method.tau = "REML",
                    common = FALSE,
                    subgroup = MA)

print(mabetween)

# Publication bias
# Overall meta-analysis for publication bias (within-group adduction)
pubiasmeta <- metagen(TE = yi,
                      seTE = se,
                      data = pubiaswithin,
                      studlab = Study,
                      sm = "SMD",
                      method.tau = "REML",
                      common = FALSE)

print(pubiasmeta)

# Test for publication bias
metabias(pubiasmeta, method.bias = "linreg", k.min = 10)

# Run MA with metafor to create the funnel plot
res <- rma(yi, sei = se, data = pubiaswithin, method = "REML")

print(res)


# Remove all objects from environment
rm(list = ls())


# Meta-analysis of abduction strength----
# Load packages
library(metafor)
library(dplyr)
library(meta)
library(readxl)

esabd <- read_excel("effect_sizes_abd.xlsx")

# Calculate standard errors for meta-analysis with the metagen function
esabd$se <- sqrt(esabd$vi)

# Part df to separate between-groups and within-groups meta-analyses
esabdwithin <- filter(esabd, ma2 == "within")
esabdbetween <- filter(esabd, ma2 == "between")

# Perform meta-analysis for within-group comparison
mawithinabd <- metagen(TE = yi, 
                    seTE = se,
                    data = esabdwithin, 
                    studlab = Study, 
                    sm = "SMD",
                    method.tau = "REML",
                    common = FALSE)
                  
print(mawithinabd)

# Perform meta-analysis for between-group comparison
mabetweenabd <- metagen(TE = yi, 
                     seTE = se,
                     data = esabdbetween, 
                     studlab = Study, 
                     sm = "SMD",
                     method.tau = "REML",
                     common = FALSE)

print(mabetweenabd)


# Remove all objects from environment
rm(list = ls())


# Meta-regression for adduction strength----
# Load packages and data
library(ggplot2)
library(readxl)
library(dplyr)
library(metafor)

metaregs <- read_excel("effect_sizes_add.xlsx")

# Pre-process to pick overall ESs only
# Part df for between-groups and within-group comparisons
metaregbetween <- filter(metaregs, ma2 =="between", MA == "Overall")
metaregwithin <- filter(metaregs, ma2 =="within", MA == "Overall")

# Meta-regressions with CAE volume as moderator
mregVolwithin <- rma(yi, vi, mods = ~ volume, data = metaregwithin)

print(mregVolwithin)

mregVolbetween <- rma(yi, vi, mods = ~ volume, data = metaregbetween)

print(mregVolbetween)

# Meta-regressions with age as moderator
mregAgewithin <- rma(yi, vi, mods = ~ age, data = metaregwithin)

print(mregAgewithin)

mregAgebetween <- rma(yi, vi, mods = ~ age, data = metaregbetween)

print(mregAgebetween)

# Meta-regressions with compliance as moderator
mregCompliancewithin <- rma(yi, vi, mods = ~ volpliance, data = metaregwithin)

print(mregCompliancewithin)

mregCompliancebetween <- rma(yi, vi, mods = ~ volpliance, data = metaregbetween)

print(mregCompliancebetween)

# Meta-regressions with weekly volume (adjusted) as moderator
mregWeeklywithin <- rma(yi, vi, mods = ~ weekvoladj, data = metaregwithin)

print(mregWeeklywithin)

mregWeeklybetween <- rma(yi, vi, mods = ~ weekvoladj, data = metaregbetween)

print(mregWeeklybetween)


# Remove all objects from environment
rm(list = ls())


# Meta-analysis for sprint performance----
# Load packages
library(metafor)
library(dplyr)
library(meta)
library(readxl)

esprint <- read_excel("effect_sizes_sprint.xlsx")

# Calculate standard errors for meta-analysis with the metagen function
esprint$se <- sqrt(esprint$vi)

# Perform meta-analysis for within-group comparison
masprint <- metagen(TE = yi, 
                       seTE = se,
                       data = esprint, 
                       studlab = Study, 
                       sm = "SMD",
                       method.tau = "REML",
                       common = FALSE,
                    subgroup = MA)

print(masprint)


# Remove all objects from environment
rm(list = ls())


# A priori power analysis----
# The a priori power analysis is based on the potential adductor strength
# enhancement (primary outcome) obtained in the intervention group after performing 
# the Copenhagen Adduction Exercise protocol. Various studies exist in this field obtaining 
# different results, mainly due to differences in exercise protocols. 
# Thus, a conservative 0.8 effect size (Cohen´s _d_) is used for power analysis.

# Code for power analysis
library(dmetar)
power.analysis(d = 0.8, # Effect size 
               k = 6, # Number of studies
               n1 = 10, # Sample size at pre- intervention
               n2 = 10, # Sample size at post- intervention
               p = 0.05, # Significance level
               heterogeneity = "moderate") # Expected heterogeneity











