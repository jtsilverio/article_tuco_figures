library(momentuHMM)
library(patchwork)
require(egg)
library(dplyr)
library(showtext)
library(lubridate)
library(scales)
source("stat-bar-tile-etho.R") # From ggetho
source("stat-tile-etho.R") # From ggetho 
source(file = "function_actogram.R")
tuco = readRDS("data/tuco_processed.rds")
m2 = readRDS("model/m2.rds")

decoded = viterbi(m2)
tuco$state = factor(decoded, labels = c("Low","Medium","High"))

# Set Theme ----------------------------------------------------------
font_add_google("Roboto Condensed", "roboto")
showtext_auto()

tuco_pal = c("Low" = "#66C2A5",
             "Medium" = "#5a69af",
             "High" = "#c25b67",
             "General Activity" = "#393939")

theme_tuco =
    egg::theme_article() +
    theme(text = element_text(size = 14,
                              family = "roboto")
    )

theme_set(theme_tuco)

# Transformation function to invert datetime in ggplot
c_trans <- function(a, b, breaks = b$breaks, format = b$format) {
    a <- scales::as.trans(a)
    b <- scales::as.trans(b)
    
    name <- paste(a$name, b$name, sep = "-")
    
    trans <- function(x) a$trans(b$trans(x))
    inv <- function(x) b$inverse(a$inverse(x))
    
    scales::trans_new(name, trans, inverse = inv, breaks = breaks, format=format)
}
rev_date <- c_trans("reverse", "date")

# Calculate Sunrise and Sunset Times -------------------------------------------
anillaco = matrix(c(-66.95, -28.8), nrow = 1) 
sunriset = tuco %>% dplyr::select(ID, season, datetime) %>%
    group_by(ID, season) %>% 
    summarise(datetime = median(datetime))

sunriset$dawn = maptools::crepuscule(crds = anillaco,
                                     dateTime = sunriset$datetime,
                                     solarDep = 6, 
                                     direction = "dawn", 
                                     POSIXct.out=TRUE)$day_frac  * 1440

sunriset$dusk = maptools::crepuscule(crds = anillaco,
                                     dateTime = sunriset$datetime,
                                     solarDep = 6,
                                     direction = "dusk",
                                     POSIXct.out=TRUE)$day_frac  * 1440


# Plot and Save Actograms ------------------------------------------------------
actograms_vedba = plot_actogram(tuco, plot_days = 5)
#actograms_high = plot_actogram(tuco, height = "High", plot_days = 5)
#actograms_medium = plot_actogram(tuco, height = "Medium", plot_days = 5)
#actograms_low = plot_actogram(tuco, height = "Low", plot_days = 5)


ggsave(filename = "images/fig_01.jpeg",
       plot = actograms_vedba,
       device = "jpeg",
       dpi = 132,
       width = 210,
       height = 290,
       units = "mm",
       bg = "white")
