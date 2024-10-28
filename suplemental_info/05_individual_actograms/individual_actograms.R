library(momentuHMM)
library(patchwork)
require(egg)
library(dplyr)
library(showtext)
library(lubridate)
library(scales)
library(suntools)
source("utils/function_actogram.R")
source("utils/stat-bar-tile-etho.R") # From ggetho
source("utils/stat-tile-etho.R") # From ggetho 
source("utils/tuco_theme.R")
source("utils/sunriset.R")

tuco = readRDS("data/tuco_processed.rds")
m2 = readRDS("HMM/m2.rds")

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

# Calculate Sunrise and Sunset Times -------------------------------------------
anillaco = matrix(c(-66.95, -28.8), nrow = 1) 
sunriset = tuco %>% dplyr::select(ID, season, datetime) %>%
    group_by(ID, season) %>% 
    summarise(datetime = median(datetime))

sunriset$dawn = suntools::crepuscule(crds = anillaco,
                                     dateTime = sunriset$datetime,
                                     solarDep = 6, 
                                     direction = "dawn", 
                                     POSIXct.out=TRUE)$day_frac  * 1440

sunriset$dusk = suntools::crepuscule(crds = anillaco,
                                     dateTime = sunriset$datetime,
                                     solarDep = 6,
                                     direction = "dusk",
                                     POSIXct.out=TRUE)$day_frac  * 1440

tucos_lux = c("JUL16","JUL17","JUL18","JUL20","JUL21","JUL23","OCT01","OCT10","FEV01","FEV02","FEV03","FEV05","FEV06")
actograms_lux = tuco %>% 
    filter(ID %in% tucos_lux) %>%
    mutate(date = date(datetime), 
           aboveground = as.numeric(aboveground)) %>% 
    mutate(aboveground = if_else(is.na(aboveground), 0, aboveground)) %>% 
    ggplot(aes(x = time, y = date, group = ID)) +
    geom_vline(data = sunriset,
              aes(xintercept = dawn), color = "grey60", linetype = 2, size = 0.5) +
    geom_vline(data = sunriset,
              aes(xintercept = dusk),  color = "grey60", linetype = 2, size = 0.5) +
    geom_bar_tile(mapping = aes(height = aboveground), fill = "orange", alpha = 0.9, width = 5) +
    scale_x_continuous(limits = c(0, 1440), breaks = c(0,360,720,1080,1440), labels = c(0,6,12,18,24)) +
    scale_y_continuous(trans = c("date", "reverse")) + 
    facet_wrap(~ID, scales = "free_y", ncol = 3) +
    xlab("") +
    ylab("") + 
    #theme_article() +
    theme(panel.grid.major.y = element_line(color = "grey95"))

sexlabels = tuco %>% 
    filter(ID %in% tucos_lux) %>% 
    select(sex, ID) %>%
    group_by(ID) %>% 
    summarise(sex = first(sex)) %>% 
    mutate(sex = if_else(sex == "m", "M", "F"))

actograms_lux = actograms_lux + geom_text(x = Inf, y = Inf, 
                         aes(label = sex), 
                         data = sexlabels, vjust = 1.5, hjust = 1.6, 
                         size = 3, family = "roboto") + 
    theme(legend.position = "none")

actograms_vedba = plot_actogram(tuco, plot_days = 5)
actograms_high = plot_actogram(tuco, height = "High", plot_days = 5)
actograms_medium = plot_actogram(tuco, height = "Medium", plot_days = 5)
actograms_low = plot_actogram(tuco, height = "Low", plot_days = 5)

ggsave(filename = "suplemental_info/05_individual_actograms/actograms_vedba.png", plot = actograms_vedba, device = "png", dpi = 132, width = 210, height = 290, units = "mm", bg = "white")
ggsave(filename = "suplemental_info/05_individual_actograms/actograms_high.png",  plot = actograms_high, device = "png", dpi = 132, width = 210, height = 290, units = "mm", bg = "white")
ggsave(filename = "suplemental_info/05_individual_actograms/actograms_medium.png",plot = actograms_medium, device = "png", dpi = 132, width = 210, height = 290, units = "mm", bg = "white")
ggsave(filename = "suplemental_info/05_individual_actograms/actograms_low.png",plot = actograms_low, device = "png", dpi = 132, width = 210, height = 290, units = "mm", bg = "white")
ggsave(filename = "suplemental_info/05_individual_actograms/actograms_lux.png",plot = actograms_lux, device = "png", dpi = 132, width = 210, height = 290, units = "mm", bg = "white")
