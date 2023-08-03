library(dplyr)
library(ggplot2)
library(momentuHMM)
library(lubridate)

tuco = readRDS("01_data/activity_processed/tuco_processed.rds")

# Read Models ------------------------------------------------------------------
m2 = readRDS("03_analysis/hmm/m2.rds") # modelo com 

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

# Calculate Diurnality -----------------------------------------------------
diurnality_vedba = tuco %>% 
    group_by(ID, season, daylength, daytime) %>%
    summarise(vedba_sum = sum(vedba)) %>% 
    group_by(ID, season) %>% 
    summarise(diurnality = (vedba_sum[daytime]/daylength)/
                  (vedba_sum[daytime]/daylength + vedba_sum[!daytime]/(1440 - daylength)) ) %>% 
    unique()
diurnality_vedba$state = "General Activity"


diurnality = tuco %>% 
    group_by(ID, date = lubridate::date(datetime), season) %>% 
    mutate(daylength = sum(daytime)) %>% 
    group_by(ID, date = lubridate::date(datetime), season, state) %>% 
    summarise(daylength = median(daylength),
              nighttime = sum(!daytime),
              daytime = sum(daytime),
              diurnality = (daytime/daylength)/
                  (daytime/daylength + nighttime/(1440 - daylength))) %>% 
    group_by(ID, season, state) %>% 
    summarise(diurnality = mean(diurnality))

diurnality = dplyr::full_join(diurnality, diurnality_vedba)
diurnality$state = factor(diurnality$state, levels = c("Rest","Medium","High","General Activity"))



shapiro.test(diurnality %>% 
                 filter(state == "Rest") %>% 
                 pluck("diurnality"))
# shapiro.test(diurnality %>%
#                  filter(state == "Rest" & season == "March") %>%
#                  pluck("diurnality"))
# shapiro.test(diurnality %>%
#                  filter(state == "Rest" & season == "July") %>%
#                  pluck("diurnality"))
# shapiro.test(diurnality %>%
#                  filter(state == "Rest" & season == "October") %>%
#                  pluck("diurnality"))
# shapiro.test(diurnality %>%
#                  filter(state == "Rest" & season == "February") %>%
#                  pluck("diurnality"))

shapiro.test(diurnality %>% filter(state == "Medium") %>% pluck("diurnality"))
# shapiro.test(diurnality %>%
#                  filter(state == "Medium" & season == "March") %>%
#                  pluck("diurnality"))
# shapiro.test(diurnality %>%
#                  filter(state == "Medium" & season == "July") %>%
#                  pluck("diurnality"))
# shapiro.test(diurnality %>%
#                  filter(state == "Medium" & season == "October") %>%
#                  pluck("diurnality"))
# shapiro.test(diurnality %>%
#                  filter(state == "Medium" & season == "February") %>%
#                  pluck("diurnality"))

shapiro.test(diurnality %>% filter(state == "High") %>% pluck("diurnality"))
# shapiro.test(diurnality %>%
#                  filter(state == "High" & season == "March") %>%
#                  pluck("diurnality"))
# shapiro.test(diurnality %>%
#                  filter(state == "High" & season == "July") %>%
#                  pluck("diurnality"))
# shapiro.test(diurnality %>%
#                  filter(state == "High" & season == "October") %>%
#                  pluck("diurnality"))
# shapiro.test(diurnality %>%
#                  filter(state == "High" & season == "February") %>%
#                  pluck("diurnality"))


# ANOVA TESTS ------------------------------------------------------------------

# AOV HIGH
aov_high = aov(diurnality %>% filter(state == "High"), formula = diurnality~season)
summary(aov_high)

# AOV MEDIUM
aov_medium = aov(diurnality %>% filter(state == "Medium"), formula = diurnality~season)
summary(aov_medium)

# AOV REST
aov_rest = aov(diurnality %>% filter(state == "Rest"), formula = diurnality~season)
summary(aov_rest)
