library(dplyr)
library(ggplot2)
library(momentuHMM)
library(data.table)

tuco = readRDS("01_data/activity_processed/tuco_processed.rds")
tuco.metadata = fread("01_data/animals/animal_metadata.csv")

# Read Models ------------------------------------------------------------------
m2 = readRDS("03_analysis/hmm/m2.rds") 

# Viterbi Decoding -------------------------------------------------------------
decoded = viterbi(m2)
tuco$state = factor(decoded, labels = c("Rest","Medium","High"))

# Calculate Daylenght ----------------------------------------------------------
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
        total_t_aboveground = mean(total_t_aboveground),
        time = mean(time),
        perc = mean(perc, na.rm = T)
    ) %>% 
    ungroup()

(k = kruskal.test(time ~ season, data = aboveground_activity %>% filter(state == "Rest")))
(k = kruskal.test(time ~ season, data = aboveground_activity %>% filter(state == "Medium")))
(k = kruskal.test(time ~ season, data = aboveground_activity %>% filter(state == "High")))

