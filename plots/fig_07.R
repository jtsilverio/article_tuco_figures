library(patchwork)
require(egg)
library(dplyr)
library(showtext)
library(lubridate)
library(scales)
library(showtext)
library(grid)
library(momentuHMM)
library(patchwork)
library(activity)
library(zoo)
source("utils/tuco_theme.R")

# Read Data ---------------------------------------------------------------
tuco.metadata = fread("data/animals/animal_metadata.csv")
tuco.metadata$ID = paste0("ID:", tuco.metadata$ID)
tuco.metadata = droplevels(tuco.metadata)

tuco = readRDS("data/tuco_processed.rds")
tuco$season = dplyr::recode(tuco$season,
                            March = "Autumn",
                            July = "Winter",
                            October = "Spring",
                            February = "Summer")

levels(tuco$ID) = paste0("ID:", levels(tuco$ID))
tuco$ID = droplevels(tuco$ID)

sunriset_season = calc_sunriset(tuco)

# calculate daylength ----------------------------------------------------------
anillaco = matrix(c(-66.95, -28.8), nrow = 1) 
daylength = tuco[, .(datetime = median(datetime)), by = ID]

daylength$dawn = suntools::crepuscule(crds = anillaco,
                                      dateTime = daylength$datetime,
                                      solarDep = 6,
                                      direction = "dawn",
                                      POSIXct.out=TRUE)$day_frac  * 1440

daylength$dusk = suntools::crepuscule(crds = anillaco,
                                      dateTime = daylength$datetime,
                                      solarDep = 6,
                                      direction = "dusk",
                                      POSIXct.out=TRUE)$day_frac  * 1440

daylength = daylength[, .(daylength = dusk - dawn), by = ID]
tuco = left_join(tuco, daylength, by = "ID")


# Calculate time on surface -----------------------------------------------
tuco_luximeter = tuco.metadata %>% filter(lux, recaptured, collar_recovered) %>% dplyr::select(ID)    

# calculate percentage of daylenght seen per animal
time_aboveground = tuco %>% 
    filter(ID %in% tuco_luximeter$ID) %>% 
    group_by(ID, season, date = lubridate::date(datetime)) %>% 
    summarise(daylength = sum(daytime),
              time_aboveground = sum(aboveground, na.rm = T) * 5,
              perc_aboveground = time_aboveground/daylength) %>% 
    group_by(ID, season) %>% 
    summarise(mean_t_aboveground = mean(time_aboveground),
              sd_t_aboveground = sd(time_aboveground),
              mean_p_aboveground = mean(perc_aboveground),
              sd_p_aboveground = sd(perc_aboveground),
              daylength = median(daylength)) %>% 
    ungroup()


# Plot --------------------------------------------------------------------

tuco_hourly = tuco %>%
    filter(ID %in% tuco_luximeter$ID) %>%
    mutate(time = floor_date(datetime,
                             unit = "hour")) %>%
    mutate(time = lubridate::hour(time)*60+lubridate::minute(time)) %>% 
    group_by(ID, season, time, date = date(datetime)) %>% 
    summarize(sum = sum(aboveground, na.rm = T)) %>% 
    group_by(ID, season, time) %>% 
    summarise(sum = mean(sum))

tuco_hourly_mean = tuco_hourly %>%
    group_by(season, time) %>% 
    summarize(mean = mean(sum)) %>% 
    ungroup()

ann_text = data.frame(
    season = "Autumn",
    time = 700,
    mean = 2.7
)

light_patterns = ggplot(data = tuco_hourly_mean,
                        aes(x = time,y = mean)) +
    geom_text(data = ann_text,
              label = "NO DATA",
              color="grey70") +
    geom_line(data = tuco_hourly,
              aes(x = time, y = sum, fill = ID),
              size = 0.4,
              alpha = 0.1) +
    geom_point(size = 0.9, color = "orange") +
    geom_line(size = 0.9, color = "orange") +
    geom_vline(data = sunriset_season,
               aes(xintercept = dawn), 
               linetype = "dotted",
               color = "grey70") +
    geom_vline(data = sunriset_season,
               aes(xintercept = dusk),
               linetype = "dotted",
               color = "grey70") +
    scale_x_continuous(breaks = c(0,360,720,1080,1440),
                       labels = c(0,6,12,18,24))+
    facet_wrap(~season, ncol = 2) +
    theme(legend.position = "none") +
    xlab("Time (h)") +
    ylab("Mean number of\nsurface emergences")
light_patterns

# Second Half
graph_t_aboveground = ggplot(data = time_aboveground, aes(x = season, y = mean_t_aboveground)) +
    gghalves::geom_half_point(color = "orange") +
    gghalves::geom_half_boxplot(nudge = 0.05, outlier.alpha = 0, color = "orange") +
    xlab("") +
    ylab("Mean daily\ntime on surface (min)")

graph_p_aboveground = ggplot(data = time_aboveground, aes(x = season, y = mean_p_aboveground)) +
    gghalves::geom_half_point(color = "orange") +
    gghalves::geom_half_boxplot(nudge = 0.05, outlier.alpha = 0, color = "orange") +
    xlab("") +
    ylab("Mean time on surface\nrelative to daylength (%)") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                       breaks = c(0, 0.05, 0.1, 0.15, 0.20, 0.25),
                       limits = c(0, 0.25))

df1 = data.frame(x = c("Summer", "Winter"), y = c(0.21,0.21))
graph_p_aboveground = graph_p_aboveground +
    geom_line(data = df1, mapping = aes(x,y, group = 1), color = "grey60") +
    annotate("text", x = "Spring", y = 0.22, label = "*", size = 4, color = "grey60")


# Compose -----------------------------------------------------------------

fig_07 = light_patterns / (graph_t_aboveground + graph_p_aboveground) + 
    plot_annotation(tag_levels = "A") + 
    plot_layout(heights = c(1.8,1))

ggsave(filename = "plots/fig_07.png",
       plot = fig_07,
       device = "png",
       dpi = 150,
       width = 210,
       height = 180,
       units = "mm",
       bg = "white")


