library(dplyr)
library(ggplot2)
library(purrr)
tuco = readRDS("01_data/activity_processed/tuco_processed.rds")

 # Daily Mean VeDBA ANOVA ------------------------------------------------------
vedba_daily = tuco %>% 
    group_by(ID, season, date = lubridate::date(datetime)) %>% 
    summarise(vedba = sum(vedba)) %>% 
    group_by(ID, season) %>% 
    summarise(vedba = mean(vedba))

# normality check
ggplot(vedba_daily, aes(sample = vedba)) +
    ggplot2::geom_qq() +
    stat_qq_line() +
    scale_x_continuous()

shapiro.test(vedba_daily %>% pluck("vedba"))
# shapiro.test(vedba_daily %>% filter(season == "July") %>% pluck("vedba"))
# shapiro.test(vedba_daily %>% filter(season == "October") %>% pluck("vedba"))
# shapiro.test(vedba_daily %>% filter(season == "February") %>% pluck("vedba"))

# AOV
aov_vedba_daily = aov(data = vedba_daily, formula = vedba~season)
summary(aov_vedba_daily)
TukeyHSD(aov_vedba_daily) # compare groups

# Daytime Mean VeDBA Anova ----------------------------------------------------------
vedba_daytime = tuco %>%
    group_by(ID, season, date = lubridate::date(datetime)) %>% 
    summarise(vedba = sum(vedba[daytime])/sum(vedba)) %>% 
    group_by(ID, season) %>% 
    summarise(vedba = mean(vedba))

# AOV
aov_vedba_daytime = aov(data = vedba_daytime, formula = vedba~season)
summary(aov_vedba_daytime)
TukeyHSD(aov_vedba_daytime) # compare groups

# Daily Mean VeDBA ANOVA ------------------------------------------------------
vedba_daily = tuco %>% 
    group_by(ID, season, date = lubridate::date(datetime)) %>% 
    summarise(vedba = mean(vedba)) %>% 
    group_by(ID, season) %>% 
    summarise(vedba = mean(vedba))

# normality check
ggplot(vedba_daily, aes(sample = vedba)) +
    ggplot2::geom_qq() +
    stat_qq_line() +
    scale_x_continuous()

shapiro.test(vedba_daily %>% pluck("vedba"))
# shapiro.test(vedba_daily %>% filter(season == "July") %>% pluck("vedba"))
# shapiro.test(vedba_daily %>% filter(season == "October") %>% pluck("vedba"))
# shapiro.test(vedba_daily %>% filter(season == "February") %>% pluck("vedba"))

