library(dplyr)
library(ggplot2)
library(gghalves)

acf_peaks = readRDS("suplemental_info/09_rhythmicity_index/rhythmicity_classified.rds")
rhythimicity = acf_peaks %>% filter(rhythmic == T)

(k = kruskal.test(acf ~ state, data = rhythimicity))
pairwise.wilcox.test(x = rhythimicity$acf, g = rhythimicity$state, p.adjust.method = "bonferroni") 
