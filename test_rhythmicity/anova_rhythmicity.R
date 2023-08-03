library(dplyr)
library(ggplot2)
library(gghalves)

acf_peaks = readRDS("03_analysis/rhythmicity/rhythmicity_classified.rds")
rhythimicity = acf_peaks %>% filter(rhythmic == T)

(k = kruskal.test(acf ~ state, data = rhythimicity))
pairwise.wilcox.test(x = rhythimicity$acf, g = rhythimicity$state, p.adjust.method = "bonferroni") 
