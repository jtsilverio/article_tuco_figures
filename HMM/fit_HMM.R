library(momentuHMM)
library(data.table)
library(dplyr)
library(ggplot2)
set.seed(53)

tuco = readRDS("data/activity_processed/tuco_processed.rds")
stateNames = c("Low","Medium","High")

# prep HMM data ------------------------------------------------------------
tuco_hmm =  prepData(tuco, 
                 coordNames = NULL, 
                 covNames = c("sex","season"))

# Initial Parameters. Obtained from findpar0.R
vedba_Par0 = c(0.0181281110, 0.1163954, 0.3268385,
               0.0098276827, 0.05098034, 0.08657868,
               0.0008149793, 1.223916e-05, 3.159508e-13)

# Fit Models --------------------------------------------------------------
m1 = fitHMM(data = tuco_hmm, 
            formula = ~1,
            nbStates = 3, 
            dist = list(vedba = "gamma"), 
            Par0 = list(vedba = vedba_Par0),
            stateNames =  stateNames)

m2 = fitHMM(data = tuco_hmm, 
            formula = ~season,
            nbStates = 3, 
            dist = list(vedba = "gamma"), 
            Par0 = list(vedba = getPar0(m1)$Par$vedba),
            stateNames =  stateNames)

# Pseudo-residuals -------------------------------------------------------
png(filename = "04_figures/residuals/m1_PR.png", width = 1200, height = 1000)
plotPR(m1, lag.max = 1500, ncores = 6)
dev.off()

png(filename = "04_figures/residuals/m2_PR.png", width = 1200, height = 1000)
plotPR(m2, lag.max = 1500, ncores = 6)
dev.off()

# Save Models ------------------------------------------------------------
saveRDS(m1, "03_analysis/hmm/m1.rds")
saveRDS(m2, "03_analysis/hmm/m2.rds")


