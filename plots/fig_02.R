library(patchwork)
require(egg)
library(dplyr)
library(showtext)
library(lubridate)
library(scales)
library(showtext)
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


# Daily VeDBA Patterns ----------------------------------
tuco_hourly = tuco %>%
    mutate(time = floor_date(datetime,
                             unit = "hour")) %>%
    mutate(time = lubridate::hour(time)*60+lubridate::minute(time)) %>% 
    group_by(ID, season, time, date = date(datetime)) %>% 
    summarize(vedba = sum(vedba)) %>% 
    group_by(ID, season, time) %>% 
    summarize(mean_vedba = mean(vedba)) %>% 
    ungroup()

tuco_hourly_mean = tuco_hourly %>%
    group_by(season, time) %>% 
    summarize(mean_vedba = mean(mean_vedba)) %>% 
    ungroup()

vedba_hourly = 
    ggplot(data = tuco_hourly_mean,
           aes(x = time,y = mean_vedba)) +
    geom_line(data = tuco_hourly,
              aes(x = time, y = mean_vedba, fill = ID),
              size = 0.4,
              alpha = 0.1) +
    geom_point(size = 0.9) +
    geom_line(size = 0.9) +
    geom_vline(data = sunriset_season,
               aes(xintercept = dawn), 
               linetype = "dotted",
               color = "grey70") +
    geom_vline(data = sunriset_season,
               aes(xintercept = dusk),
               linetype = "dotted",
               color = "grey70") +
    geom_text(data = data.frame(x = 65, y = 0.29),
              aes(x,y, label = c("n = 2","n = 8","n = 6","n = 5")),
              size = 3,
              color = "grey60") +
    scale_x_continuous(breaks = c(0,360,720,1080,1440),
                       labels = c(0,6,12,18,24))+
    facet_wrap(~season, ncol = 2) +
    theme(legend.position = "none") +
    xlab("") +
    ylab("Hourly mean VeDBA (g)")

# BOXPLOT 01 -----------------------------------
vedba_daily = tuco %>% 
    group_by(ID, season, date = lubridate::date(datetime)) %>% 
    summarise(vedba = sum(vedba)) %>% 
    group_by(ID, season) %>% 
    summarise(vedba = mean(vedba))

bx_daily_vedba = ggplot(data = vedba_daily,
                        aes(x = season,
                            y = vedba)) +
    gghalves::geom_half_point() +
    gghalves::geom_half_boxplot(nudge = 0.05,
                                outlier.color = NA) +
    xlab("Time (h)") +
    ylab("Mean daily VeDBA (g)") +
    theme(legend.position = "none")

df1 = data.frame(x = c(1.9, 3), y = c(230,230))
df2 = data.frame(x = c(1.9, 4), y = c(240, 240))

bx_daily_vedba = bx_daily_vedba +
    geom_line(data = df1, mapping = aes(x,y, group = 1), color = "grey60") +
    annotate("text", x = 2.5, y = 232, label = "*", size = 4, color = "grey60") +
    geom_line(data = df2, mapping = aes(x,y, group = 1), color = "grey60") +
    annotate("text", x = 3, y = 242, label = "*", size = 4, color = "grey60")


# BOXPLOT 02 -----------------------------------
vedba_daytime = tuco %>%
    group_by(ID, season, date = lubridate::date(datetime)) %>% 
    summarise(vedba = sum(vedba[daytime])/sum(vedba)) %>% 
    group_by(ID, season) %>% 
    summarise(vedba = mean(vedba))

bx_daytime_vedba = ggplot(data = vedba_daytime,
                          aes(x = season,
                              y = vedba)) +
    gghalves::geom_half_point() +
    gghalves::geom_half_boxplot(nudge = 0.05,
                                outlier.color = NA) +
    scale_y_continuous(breaks = seq(0.5, 1, 0.1), labels = scales::percent, limits = c(0.5, 0.85)) +
    xlab("") +
    ylab("Mean daytime VeDBA (%)") +
    theme(legend.position = "none")


df1 = data.frame(x = c(1.9, 3), y = c(0.8,0.8))
df2 = data.frame(x = c(1.9, 4), y = c(0.825, 0.825))
bx_daytime_vedba = bx_daytime_vedba +
    geom_line(data = df1, mapping = aes(x,y, group = 1), color = "grey60") +
    annotate("text", x = 2.5, y = 0.809, label = "*", size = 4, color = "grey60") +
    geom_line(data = df2, mapping = aes(x,y, group = 1), color = "grey60") +
    annotate("text", x = 3, y = 0.835, label = "*", size = 4, color = "grey60")


# Compose Plot
fig02 = vedba_hourly / (bx_daily_vedba + bx_daytime_vedba) +
    plot_annotation(tag_levels = "A") +
    plot_layout(ncol = 1, height = c(2,1))


ggsave(filename = "plots/fig_02.jpeg",
       plot = fig02,
       device = "jpeg",
       dpi = 132,
       width = 1200,
       height = 960,
       units = "px",
       bg = "white")
