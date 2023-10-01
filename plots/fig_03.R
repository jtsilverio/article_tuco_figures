library(patchwork)
require(egg)
library(dplyr)
library(showtext)
library(lubridate)
library(scales)
library(showtext)
library(momentuHMM)
library(momentuHMM)
source("utils/tuco_theme.R")
source("utils/sunriset.R")


# Read Data ---------------------------------------------------------------
tuco = readRDS("data/tuco_processed.rds")
tuco$season = dplyr::recode(tuco$season,
                            March = "Autumn",
                            July = "Winter",
                            October = "Spring",
                            February = "Summer")

levels(tuco$ID) = paste0("ID:", levels(tuco$ID))
tuco$ID = droplevels(tuco$ID)
sunriset_season = calc_sunriset(tuco)


# Extract Parameters from model object ------------------------------------
m2 = readRDS("HMM/m2.rds")
params = list()
params[["mean"]] = unname(m2$mle$vedba[1,])
params[["sd"]] = unname(m2$mle$vedba[2,])
params[["weight"]] = as.numeric(momentuHMM::timeInStates(m2)[1,])

# Plot Histogram + density curves. Gamma density curves are weighted by
# the time spent in each viterbi state sequence. 
# This is is accordance with what is done in the momentuHMM:::plot. I just wanted to plot things in ggplot2.

for(i in 0:3){
    x = seq(from = 0, to = round(max(tuco$vedba),2), length.out = 1000)
    d = data.frame(
        mapply(
            function(rate, shape, weight){
                dgamma(x, rate, shape) * weight
            },
            rate   = (params$mean/params$sd)^2,
            shape  = params$mean/(params$sd^2),
            weight = params$weight))
    
    if(i == 3){
        d = data.frame(d)
        names(d) = c("Low","Medium","High")
        d$Marginal = d$Low + d$Medium + d$High
        d$x = x
        d = tidyr::pivot_longer(d, 1:4, names_to = "state", values_to = "density")
    }
}

fig03 = ggplot(tuco, aes(x = vedba)) +
    geom_histogram(data = tuco, 
                   aes(x = vedba, y = ..density..), 
                   alpha = 0.2, 
                   binwidth = 0.015, 
                   color = "white") +
    scale_colour_manual(name = "States", values = c(tuco_pal[1:3], Marginal = "grey40")) +
    #theme_article() +
    xlab("VeDBA (g)") +
    ylab("Density") +
    geom_line(data = d[d$state != "Marginal",], aes(x = x, y = density, color = state), size = 1.2) +
    geom_line(data = d[d$state == "Marginal",], aes(x = x, y = density, color = state), size = 0.5, linetype = "dashed")


ggsave(filename = "plots/fig_03.png",
       plot = fig03,
       device = "png",
       dpi = 132,
       width = 1200,
       height = 600,
       units = "px",
       bg = "white")
