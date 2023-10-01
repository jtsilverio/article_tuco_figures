library(momentuHMM)
library(patchwork)
require(egg)
library(dplyr)
library(showtext)
library(lubridate)
library(scales)
library(kableExtra)
source("utils/function_actogram.R")
source("utils/stat-bar-tile-etho.R") # From ggetho
source("utils/stat-tile-etho.R") # From ggetho 
source("utils/tuco_theme.R")
source("utils/sunriset.R")


tuco = readRDS("data/tuco_processed.rds")
range = tuco %>%
    group_by(ID, season, sex) %>%
    summarise(Mean = mean(vedba),
              Median = median(vedba),
              Max = max(vedba),
              Min = min(vedba),
              Range = Max - Min)

median_tuco = tuco %>% 
    group_by(season, sex) %>% 
    summarise(median = median(vedba),
              mean = mean(vedba),
              max = max(vedba),
              n.animals = length(unique(ID)),
              n.points = n())

kable(range,
      caption = "Summary table of individual VeDBA data.") %>% 
    save_kable("suplemental_info/07_vedba_distribution/table_vedba_individual_summary.html")

kable(median_tuco,
      caption = "Summary table of VeDBA data grouped by season.") %>% 
    save_kable("suplemental_info/07_vedba_distribution/table_vedba_seasonal_summary.html")

vedba_hist_individual = ggplot(tuco) +
    geom_histogram(aes(x = vedba, fill = season, y=..density..), bins = 40 ) +
    facet_wrap(~ID, ncol = 3) +
    theme(legend.position = "bottom") +
    theme(panel.grid.major.y = element_line(color = "grey95"))


sexlabels = unique(tuco %>% select(ID, sex) %>% mutate(sex = if_else(sex == "m", "♂", "♀"))) 
vedba_hist_individual = vedba_hist_individual +
    geom_text(x = Inf, y = Inf, 
              aes(label = sex), 
              data = sexlabels, vjust = 1.3, hjust = 1.3, 
              fontface = "bold", size = 5, color = "grey50") + 
    theme(legend.position = "none")+
    xlab("VeDBA") +
    ylab("Density")

vedba_density_sex = ggplot(tuco) +
    geom_density(aes(x = vedba, fill = sex, col = sex), alpha = 0.5)+
    facet_grid(season~.) +
    xlab("VeDBA")+
    ylab("Density")

ggsave("suplemental_info/07_vedba_distribution/vedba_distibution_individual.png",
       vedba_hist_individual, dpi = 150, width = 900, height = 1000, unit = "px")

ggsave("suplemental_info/07_vedba_distribution/vedba_distibution_sex.png",
       vedba_density_sex, dpi = 150, width = 900, height = 1000, unit = "px")
