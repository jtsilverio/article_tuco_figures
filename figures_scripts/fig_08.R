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

tuco = readRDS("data/tuco_processed.rds")
tuco$season = dplyr::recode(tuco$season,
                            March = "Autumn",
                            July = "Winter",
                            October = "Spring",
                            February = "Summer")

levels(tuco$ID) = paste0("ID:", levels(tuco$ID))
tuco$ID = droplevels(tuco$ID)

source("utils/sunriset.R")

# calculate daylength ----------------------------------------------------------
anillaco = matrix(c(-66.95, -28.8), nrow = 1) 
daylength = tuco[, .(datetime = median(datetime)), by = ID]

daylength$dawn = maptools::crepuscule(crds = anillaco,
                                      dateTime = daylength$datetime,
                                      solarDep = 6,
                                      direction = "dawn",
                                      POSIXct.out=TRUE)$day_frac  * 1440

daylength$dusk = maptools::crepuscule(crds = anillaco,
                                      dateTime = daylength$datetime,
                                      solarDep = 6,
                                      direction = "dusk",
                                      POSIXct.out=TRUE)$day_frac  * 1440

daylength = daylength[, .(daylength = dusk - dawn), by = ID]
tuco = left_join(tuco, daylength, by = "ID")

# Decode HMM state -------------------------------------------------------------
m2 = readRDS("model/m2.rds")
decoded = viterbi(m2)
tuco$state = factor(decoded, labels = c("Low","Medium","High"))


# Calculate time on surface -----------------------------------------------
tuco_luximeter = tuco.metadata %>% 
    filter(lux, recaptured, collar_recovered) %>% 
    dplyr::select(ID)

tuco_abv_act = tuco %>% 
    filter(ID %in% tuco_luximeter$ID, daytime == T) %>%
    group_by(ID) %>% 
    mutate(lux_filled = zoo::na.locf(lux, fromLast = T, na.rm = F),
           aboveground = ifelse(lux_filled >= 2, T, F)) %>% 
    ungroup()

aboveground_activity = tuco_abv_act %>%
    group_by(ID, date = lubridate::date(datetime), season) %>% 
    mutate(total_t_aboveground = sum(aboveground, na.rm = T)) %>%
    group_by(ID, date = lubridate::date(datetime), season, state) %>% 
    summarise(time = sum(aboveground, na.rm = T),
              total_t_aboveground = total_t_aboveground,
              perc = time/total_t_aboveground) %>% 
    unique() %>% 
    group_by(ID, season, state) %>% 
    summarise(
        time = mean(time),
        perc = mean(perc, na.rm = T)
    ) %>% 
    ungroup()


# Plot --------------------------------------------------------------------
abvg_bar = ggplot(aboveground_activity, aes(perc, ID, fill = state, group = season)) +
    geom_bar(position = "stack",
             stat = "identity") +
    geom_text(aes(label = round(perc, 3) * 100), 
              position = position_stack(vjust = 0.5), 
              color = "grey98", size = 2.8) +
    scale_fill_manual(values = tuco_pal[1:3]) +
    #scale_x_continuous(expand = c(0.02, 0)) +
    facet_grid(season~.,
               scales = "free",
               space = "free_y") +
    theme(panel.border = element_rect(colour = NA, fill=NA),
          axis.ticks.y = element_blank()) +
    labs(fill = "State") +
    scale_x_continuous(labels = scales::percent_format()) +
    ylab("") +
    xlab("Mean percentage of time on surface")

plot_abvact_time = ggplot(aboveground_activity, aes(season, time, color = state)) +
    gghalves::geom_half_point() +
    gghalves::geom_half_boxplot(nudge = 0.05, outlier.alpha = 0) +
    ylab("Mean daily time in\naboveground activity (min)") +
    xlab("") +
    scale_color_manual(values = tuco_pal[1:3]) +
    theme(legend.position = "none") + 
    facet_wrap(vars(state))

# plot_abvact_perc = ggplot(aboveground_activity, aes(state, perc, color = state)) +
#     gghalves::geom_half_point() +
#     gghalves::geom_half_boxplot(nudge = 0.05, outlier.alpha = 0) +
#     ylab("Percentage of\nAboveground Activity") +
#     xlab("") +
#     scale_color_manual(values = tuco_pal[1:3]) +
#     theme(legend.position = "none") +
#     scale_y_continuous(labels = scales::percent_format())

fig_08 = abvg_bar + plot_abvact_time+
    plot_layout(ncol = 1,
                heights = c(1.8,1)) +
    plot_annotation(tag_levels = "A")

ggsave(filename = "images/fig_08.png",
       plot = fig_08,
       device = "png",
       dpi = 150,
       width = 210,
       height = 180,
       units = "mm",
       bg = "white")

