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
library(ggforce)
source("utils/function_actogram.R")
source("utils/stat-bar-tile-etho.R") # From ggetho
source("utils/stat-tile-etho.R") # From ggetho 
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


# Decode HMM state -------------------------------------------------------------
m2 = readRDS("HMM/m2.rds")
decoded = viterbi(m2)
tuco$state = factor(decoded, labels = c("Low","Medium","High"))


# VeDBA Actograms ---------------------------------------------------------
actograms_vedba = tuco %>% filter(ID == "ID:OCT09" & day_number <= 5) %>% 
    ggplot(aes(x = time, y = day_number)) +
    geom_vline(data = sunriset %>% filter(ID == "ID:OCT09"), 
               aes(xintercept = dawn), color = "grey60", linetype = 2, size = 0.5) +
    geom_vline(data = sunriset %>% filter(ID == "ID:OCT09"),
               aes(xintercept = dusk),  color = "grey60", linetype = 2, size = 0.5) +
    geom_bar_tile(mapping = aes(height = vedba), width = 1, color = "black") +
    scale_x_continuous(limits = c(0, 1440), breaks = c(0,360,720,1080,1440), labels = c(0,6,12,18,24)) +
    scale_y_continuous(trans = "reverse", breaks = 1:5) +
    facet_wrap(~ID, scales = "free_y", ncol = 3) +
    ggtitle("General Activity") +
    xlab("Time (h)") +
    ylab("")+
    theme(panel.grid.major.y = element_line(color = "grey95"))



# Data Processing for State Actograms ----------------------------------------------
# For plotting states actograms we use a ggplot::linerange that creates a thick
# line between representing the duration of the behavior.
# In order to do that we first need to get start and end times of the states
# using RLE.

# Do Running Length Decoding to get Start and End of Activity
act_rle = tuco %>%
    group_by(ID) %>%
    summarise(status = rle(as.vector(state))$values,
              length = rle(as.vector(state))$lengths) %>%
    ungroup() %>% 
    mutate(end = cumsum(length),
           start = end - length + 1)

act_rle = act_rle %>% 
    group_by(ID) %>% 
    summarise(
        status = status,
        start = start,
        end = end,
        start_datetime = (tuco$datetime[start]),
        end_datetime = (tuco$datetime[end]))

# For plotting segments that cross the boundary between days we need to split
# these segments into two smaller segments, one for each day. 
across_index = which(as_date(act_rle$start_datetime) != as_date(act_rle$end_datetime),)

first_segment = 
    act_rle[across_index,] %>% 
    mutate(old_end_datetime = end_datetime,
           end_datetime = as_datetime(paste0(as_date(start_datetime), "23:59:59"),
                                      tz = "America/Argentina/La_Rioja"),
           old_end_datetime = NULL)

second_segment = 
    act_rle[across_index,] %>% 
    mutate(old_start_datetime = start_datetime,
           start_datetime = as_datetime(paste0(as_date(end_datetime), "00:00:00"),
                                        tz = "America/Argentina/La_Rioja"),
           old_start_datetime = NULL)

# Join new segments into previous dataframe
# Create new columns splitting date and time
act_rle = act_rle[-across_index,] %>% 
    rbind(first_segment) %>%
    rbind(second_segment) %>%
    group_by(ID) %>%
    mutate(start_date = lubridate::as_date(start_datetime),
           end_date = lubridate::as_date(end_datetime),
           start_time = lubridate::hour(start_datetime) * 60 + lubridate::minute(start_datetime),
           end_time =  lubridate::hour(end_datetime) * 60 + lubridate::minute(end_datetime),
           day_number = data.table::frank(start_date, ties.method = "dense")
    ) %>% 
    ungroup()



# Plot State Actograms ----------------------------------------------------
actograms_low = act_rle %>%
    filter(ID == "ID:OCT09" & status == "Low" & day_number <= 5) %>% 
    ggplot() +
    geom_vline(data = sunriset %>% filter(ID == "ID:OCT09"), 
               aes(xintercept = dawn), color = "grey60", linetype = 2, size = 0.5) +
    geom_vline(data = sunriset %>% filter(ID == "ID:OCT09"),
               aes(xintercept = dusk),  color = "grey60", linetype = 2, size = 0.5) +
    geom_linerange(aes(y = day_number,
                       xmin = start_time,
                       xmax = end_time,
                       color = status),
                   size = 7) +
    scale_y_continuous(trans = "reverse", breaks = 1:5, expand = c(0.1, 0.1)) +
    scale_x_continuous(breaks = c(0, 360, 720, 1080, 1440), 
                       labels = c(0, 6, 12, 18, 24)) +
    scale_color_manual(values = tuco_pal) +
    #facet_wrap(vars(ID), ncol = 4) +
    theme(legend.position = "none") +
    ggtitle("Low Activity") +
    xlab("Time (h)") +
    ylab("")

actograms_medium = act_rle %>%
    filter(ID == "ID:OCT09" & status == "Medium" & day_number <= 5) %>% 
    ggplot() +
    geom_vline(data = sunriset %>% filter(ID == "ID:OCT09"), 
               aes(xintercept = dawn), color = "grey60", linetype = 2, size = 0.5) +
    geom_vline(data = sunriset %>% filter(ID == "ID:OCT09"),
               aes(xintercept = dusk),  color = "grey60", linetype = 2, size = 0.5) +
    geom_linerange(aes(y = day_number,
                       xmin = start_time,
                       xmax = end_time,
                       color = status),
                   size = 7) +
    scale_y_continuous(trans = "reverse", breaks = 1:5, expand = c(0.1, 0.1)) +
    scale_x_continuous(breaks = c(0, 360, 720, 1080, 1440), 
                       labels = c(0, 6, 12, 18, 24)) +
    scale_color_manual(values = tuco_pal) +
    #facet_wrap(vars(ID), ncol = 4) +
    theme(legend.position = "none") +
    ggtitle("Medium Activity") +
    xlab("Time (h)") +
    ylab("")

actograms_high = act_rle %>%
    filter(ID == "ID:OCT09" & status == "High" & day_number <= 5) %>% 
    ggplot() +
    geom_vline(data = sunriset %>% filter(ID == "ID:OCT09"), 
               aes(xintercept = dawn), color = "grey60", linetype = 2, size = 0.5) +
    geom_vline(data = sunriset %>% filter(ID == "ID:OCT09"),
               aes(xintercept = dusk),  color = "grey60", linetype = 2, size = 0.5) +
    geom_linerange(aes(y = day_number,
                       xmin = start_time,
                       xmax = end_time,
                       color = status),
                   size = 7) +
    scale_y_continuous(trans = "reverse", breaks = 1:5, expand = c(0.1, 0.1)) +
    scale_x_continuous(breaks = c(0, 360, 720, 1080, 1440), 
                       labels = c(0, 6, 12, 18, 24)) +
    scale_color_manual(values = tuco_pal) +
    #facet_wrap(vars(ID), ncol = 4) +
    theme(legend.position = "none") +
    ggtitle("High Activity") +
    xlab("Time (h)") +
    ylab("")



# Select one tuco for a closer TS example ---------------------------------
tuco_selected = tuco %>% filter(ID == "ID:OCT09")
remove_facet = theme(strip.text.x = element_blank(),
                     plot.title = element_text(size=12))

# Time Series
tuco_selected = tuco_selected[day_number <= 5]
sunriset_oct09 = data.frame(date = unique(as_date(tuco_selected$datetime))) %>% 
    mutate(dawn = sunriset %>% filter(ID == "ID:OCT09") %>% pull(dawn),
           dusk = sunriset %>% filter(ID == "ID:OCT09") %>% pull(dusk),
           dawn = lubridate::ymd_hm(paste0(date,
                                           " ",
                                           stringr::str_pad(dawn %/% 60, 2, pad = "0"),
                                           ":",
                                           stringr::str_pad(as.integer(dawn %% 60), 2, pad = "0"))
           ),
           dusk = lubridate::ymd_hm(paste0(date,
                                           " ",
                                           stringr::str_pad(dusk %/% 60, 2, pad = "0"),
                                           ":",
                                           stringr::str_pad(as.integer(dusk %% 60), 2, pad = "0"))
           )
    )

ts_vedba_zoom =
    ggplot(tuco_selected) +
    geom_rect(data = sunriset_oct09,
              aes(xmin = dusk,
                  xmax = dawn,
                  ymin = -Inf,
                  ymax = Inf),
              fill = "orange",
              alpha = 0.09) +
    geom_point(aes(datetime, vedba, color = state), size = 0.3) + 
    xlab("") +
    ylab("VeDBA (g)") +
    scale_x_datetime(date_breaks = "1 day") +
    scale_color_manual(values = tuco_pal[1:3],
                       labels = c("Low", "Medium", "High")) +
    labs(color = "State") +
    guides(colour = guide_legend(override.aes = list(size=1.5))) +
    ggforce::facet_zoom(zoom.size = 1,
                        show.area = F,
                        horizontal = FALSE,
                        xlim = c(ymd_hm("2019-10-24 00:00"),
                                 ymd_hm("2019-10-24 23:59"))) +
    theme_bw() +
    theme(text = element_text(size = 16,
                              family = "roboto")) + 
    theme(strip.background = element_rect(fill = "white"))


# Create Mosaic Plot -----------------------------------------------------------
design = "
112334455
666666666
666666666
666666666
"

fig_04 = actograms_vedba +
    wrap_elements(grid::textGrob(" ⇒",
                                 gp = gpar(fontsize=35, col="#2F4F4F", 
                                           fontfamily = "Sans")), 
                  ignore_tag = T) +
    actograms_low +
    actograms_medium + 
    actograms_high +
    wrap_elements(ts_vedba_zoom) +                 
    plot_layout(design = design, heights = c(3, 1.8)) + 
    plot_annotation(tag_levels = 'A') +
    theme(plot.tag.position = c(0, 1),
          plot.tag = element_text(hjust = 0, vjust = 0))

ggsave(filename = "plots/fig_04.png",
       plot = fig_04,
       device = "png",
       dpi = 132,
       width = 1200,
       height = 800,
       units = "px",
       bg = "white")

