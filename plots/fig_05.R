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
daylength = tuco %>% 
    group_by(ID) %>% 
    summarise(datetime = median(datetime))

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

daylength = daylength %>% 
    group_by(ID) %>% 
    mutate(daylength = dusk - dawn) %>% 
    select(-datetime)

tuco = left_join(tuco, daylength, by = "ID")

# Decode HMM state -------------------------------------------------------------
m2 = readRDS("HMM/m2.rds")
decoded = viterbi(m2)
tuco$state = factor(decoded, labels = c("Low","Medium","High"))

# 01: Plot Density patterns per state ------------------------------------------
get_pdf = function(x){
    m = fitact(x$radians, adj = 0.3)
    data = data.frame(m@data)
    pdf  = data.frame(m@pdf)
    return(list(data, pdf))
}

tuco_fit = tuco %>% 
    dplyr::select(ID, sex, season, time, state) %>% 
    mutate(radians = time * ((2*pi)/1440)) %>% 
    tidyr::nest(cols = -c(season, state)) %>% 
    mutate(fit = purrr::map(cols, get_pdf)) %>% 
    dplyr::select(-cols)

kernel_data = tuco_fit %>% 
    mutate(data = purrr::map(fit, function(x) x[[1]])) %>% 
    select(-fit) %>% 
    tidyr::unnest(data)

kernel_pdf = tuco_fit %>% 
    mutate(pdf = purrr::map(fit, function(x) x[[2]])) %>% 
    select(-fit) %>% 
    tidyr::unnest(pdf)

density_plot = ggplot() +
    geom_histogram(data = kernel_data,
                   aes(x = m.data,
                       y = ..density..,
                       fill = state),
                   color = "white",
                   alpha = 0.2,
                   bins = 24) +
    #geom_ribbon(aes(x, ymin = lcl, ymax = ucl), fill = "grey70") +
    geom_density(data = kernel_pdf,
                 aes(x, y, color = state),
                 stat = "identity",
                 size = 0.8) +
    scale_x_continuous(limits = c(0, 2*pi),
                       breaks = c(0, pi/3, 2*pi/3, pi, 4*pi/3, 5*pi/3, 2*pi),
                       labels = c(0, 4, 8, 12, 16, 20, 24)) +
    facet_grid(season~state) +
    scale_color_manual(values = tuco_pal) +
    scale_fill_manual(values = tuco_pal) +
    theme(legend.position = "none")+
    xlab("Time (h)") +
    ylab("Density") +
    geom_vline(data = sunriset_season,
               aes(xintercept = dawn * ((2*pi)/1440)), 
               linetype = "dotted",
               color = "grey70") +
    geom_vline(data = sunriset_season,
               aes(xintercept = dusk * ((2*pi)/1440)),
               linetype = "dotted",
               color = "grey70") 


# 02: Plot Diurnality boxplots ------------------------------------------------
diurnality_vedba = tuco %>% 
    group_by(ID, season, daylength, daytime) %>%
    summarise(vedba_sum = sum(vedba)) %>% 
    group_by(ID, season) %>% 
    summarise(diurnality = (vedba_sum[daytime]/daylength)/
                  (vedba_sum[daytime]/daylength + vedba_sum[!daytime]/(1440 - daylength)) ) %>% 
    unique()
diurnality_vedba$state = "General Activity"


diurnality = tuco %>% 
    group_by(ID, date(datetime), season) %>% 
    mutate(daylength = sum(daytime)) %>% 
    group_by(ID, date(datetime), season, state) %>% 
    summarise(daylength = median(daylength),
              nighttime = sum(!daytime),
              daytime = sum(daytime),
              diurnality = (daytime/daylength)/
                  (daytime/daylength + nighttime/(1440 - daylength))) %>% 
    group_by(ID, season, state) %>% 
    summarise(diurnality = mean(diurnality))

diurnality = dplyr::full_join(diurnality, diurnality_vedba)
diurnality$state = factor(diurnality$state, levels = c("Low","Medium","High","General Activity"))

diurnality_plot = ggplot(diurnality,
                         aes(x = season,
                             y = diurnality,
                             color = state)) +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey90") +
    gghalves::geom_half_boxplot(nudge = 0.05,
                                outlier.color = NA) +
    gghalves::geom_half_point() +
    scale_color_manual(values = tuco_pal) +
    scale_x_discrete(labels = c("Autumn","Winter","Spring","Summer")) +
    scale_y_continuous(limits = c(0,1))+
    facet_wrap(~state, ncol = 4) +
    xlab("") +
    ylab("Diurnality Index (DI)") +
    theme(legend.position = "none")


# 03: Rhithmicity Plot
acf_peaks = readRDS("suplemental_info/09_rhythmicity_index/rhythmicity_classified.rds") %>% ungroup()

levels(acf_peaks$state) = c("Low", "Medium", "High", "General Activity")
#levels(period_peaks$state) = c("Low", "Medium", "High", "General Activity")

acf_dist_plot = ggplot(acf_peaks %>% filter(rhythmic == T),
                       aes(x = state,
                           y = acf,
                           color = state)) +
    gghalves::geom_half_boxplot() +
    gghalves::geom_half_point(transformation = position_jitter(width = 0.05, height = 0)) +
    scale_y_continuous(n.breaks = 8) +
    ylab("Rhythmicity Index (RI)") +
    xlab("") +
    scale_color_manual(values = tuco_pal) +
    geom_text(data = data.frame(state = "Medium", acf = 0.3), aes(state, acf), label = "*", color = "grey60", size = 5) +
    theme(legend.position = "none")



# Compose Figure
fig_05 = density_plot / diurnality_plot / acf_dist_plot + patchwork::plot_annotation(tag_levels = "A")  + patchwork::plot_layout(heights = c(3,1,1))

ggsave(filename = "plots/fig_05.png",
       plot = fig_05,
       device = "png",
       dpi = 132,
       width = 210,
       height = 297,
       units = "mm",
       bg = "white")
