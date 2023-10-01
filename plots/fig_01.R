library(momentuHMM)
library(patchwork)
require(egg)
library(dplyr)
library(showtext)
library(lubridate)
library(scales)
library(showtext)
source("utils/function_actogram.R")
source("utils/stat-bar-tile-etho.R") # From ggetho
source("utils/stat-tile-etho.R") # From ggetho 
source("utils/tuco_theme.R")
source("utils/sunriset.R")

# Read Data and Model -----------------------------------------------------
tuco = readRDS("data/tuco_processed.rds")
tuco$season = dplyr::recode(tuco$season,
                            March = "Autumn",
                            July = "Winter",
                            October = "Spring",
                            February = "Summer")

levels(tuco$ID) = paste0("ID:", levels(tuco$ID))
tuco$ID = droplevels(tuco$ID)
sunriset_season = calc_sunriset(tuco)

m2 = readRDS("HMM/m2.rds")


# Decode states from model ------------------------------------------------
decoded = viterbi(m2)
tuco$state = factor(decoded, labels = c("Low","Medium","High"))


# Transformation function to invert datetime in ggplot -------------------------
c_trans <- function(a, b, breaks = b$breaks, format = b$format) {
    a <- scales::as.trans(a)
    b <- scales::as.trans(b)
    
    name <- paste(a$name, b$name, sep = "-")
    
    trans <- function(x) a$trans(b$trans(x))
    inv <- function(x) b$inverse(a$inverse(x))
    
    scales::trans_new(name, trans, inverse = inv, breaks = breaks, format=format)
}
rev_date <- c_trans("reverse", "date")


# Plot and Save Actograms ------------------------------------------------------
actograms_autumn = plot_actogram(tuco %>% filter(season == "Autumn"), plot_days = 5)
actograms_autumn = actograms_autumn + ggtitle("Autumn")

actograms_winter = plot_actogram(tuco %>% filter(season == "Winter"), plot_days = 5)
actograms_winter = actograms_winter + ggtitle("Winter")

actograms_spring = plot_actogram(tuco %>% filter(season == "Spring"), plot_days = 5)
actograms_spring = actograms_spring + ggtitle("Spring")

actograms_summer = plot_actogram(tuco %>% filter(season == "Summer"), plot_days = 5)
actograms_summer = actograms_summer + ggtitle("Summer")

ggsave(filename = "plots/fig_01_autumn.png",
       plot = actograms_autumn,
       device = "png",
       dpi = 132,
       width = 900, # column = 450px; row = 500px
       height = 350,
       units = "px",
       bg = "white")

ggsave(filename = "plots/fig_01_winter.png",
       plot = actograms_winter,
       device = "png",
       dpi = 132,
       width = 1350,
       height = 900,
       units = "px",
       bg = "white")

ggsave(filename = "plots/fig_01_spring.png",
       plot = actograms_spring,
       device = "png",
       dpi = 132,
       width = 1350,
       height = 600,
       units = "px",
       bg = "white")

ggsave(filename = "plots/fig_01_summer.png",
       plot = actograms_summer,
       device = "png",
       dpi = 132,
       width = 1350,
       height = 600,
       units = "px",
       bg = "white")
