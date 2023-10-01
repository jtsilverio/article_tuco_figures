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
m2 = readRDS("HMM/m2.rds")
decoded = viterbi(m2)
tuco$state = factor(decoded, labels = c("Low","Medium","High"))



# Calculate time-activity budget ------------------------------------------
daily_budget_id = tuco %>% 
    group_by(season, ID, date = lubridate::date(datetime), state) %>%
    summarise(
        time = n(),
        perc = n()/1440 
    ) %>% 
    group_by(ID, season, state) %>% 
    summarise(
        time = mean(time),
        perc = mean(perc)
    )



# Plot --------------------------------------------------------------------
bar_time = ggplot(daily_budget_id, aes(perc, ID, fill = state, group = season)) +
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
    xlab("Mean percentage of the day in state") +
    ylab("") +
    theme(panel.border = element_rect(colour = NA, fill=NA),
          axis.ticks.y = element_blank()) +
    labs(fill = "State") +
    scale_x_continuous(labels = scales::percent_format())


bxp_time = ggplot(daily_budget_id,
                  aes(x = season,
                      y = time,
                      color = state)) +
    gghalves::geom_half_boxplot(nudge = 0.05,
                                outlier.color = NA) +
    gghalves::geom_half_point() +
    scale_color_manual(values = tuco_pal) +
    scale_x_discrete(labels = c("Autumn","Winter","Spring","Summer")) +
    facet_wrap(~state) +
    xlab("") +
    ylab("Mean daily\ntime in state (min)") +
    theme(legend.position = "none")

df1 = data.frame(x = c(1.9, 3), 
                 y = c(750, 750), 
                 state = factor("Medium", "Medium"))
df2 = data.frame(x = c(1.9, 3), 
                 y = c(750, 750), 
                 state = factor("High", "High"))
df3 = data.frame(x = c(1.9, 4), 
                 y = c(800, 800), 
                 state = factor("High", "High"))

bxp_time = bxp_time +
    geom_line(data = df1,
              mapping = aes(x,y, group = 1),
              color = "grey60") +
    geom_line(data = df2,
              mapping = aes(x,y, group = 1),
              color = "grey60") +
    geom_line(data = df3,
              mapping = aes(x,y, group = 1),
              color = "grey60") +
    geom_text(data = data.frame(x = c(2.5, 2.5, 3),
                                y = c(760,760,820),
                                state = factor(c("Medium", "High", "High"),),
                                label = rep("*",3)),
              aes(x,y,label = label),
              color = "grey60")

fig_06 = bar_time + bxp_time +
    plot_layout(ncol = 1,
                heights = c(2.5,1)) +
    plot_annotation(tag_levels = "A")


ggsave(filename = "plots/fig_06.png",
       plot = fig_06,
       device = "png",
       dpi = 150,
       width = 210,
       height = 180,
       units = "mm",
       bg = "white")

