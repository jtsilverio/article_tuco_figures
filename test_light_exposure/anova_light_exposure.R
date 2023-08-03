# Calculate Above ground bouts of activity
#source("../03_analysis/light_exposure/function_bouts.R")
tuco = readRDS("01_data/activity_processed/tuco_processed.rds")
tuco.metadata = read.csv("01_data/animals/animal_metadata.csv")

# list of animals that had luximeter
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
              season = unique(season),
              daylength = median(daylength)) %>% 
    ungroup()

# normality checking ----------------------------------------------------------
# Time aboveground
ggplot(time_aboveground, aes(sample = mean_t_aboveground)) +
    ggplot2::geom_qq() +
    stat_qq_line() +
    scale_x_continuous()

shapiro.test(time_aboveground %>% pluck("mean_t_aboveground"))
#shapiro.test(time_aboveground %>% filter(season == "July") %>% pluck("mean_t_aboveground"))
#shapiro.test(time_aboveground %>% filter(season == "October") %>% pluck("mean_t_aboveground"))
#shapiro.test(time_aboveground %>% filter(season == "February") %>% pluck("mean_t_aboveground"))

# Perc aboveground
ggplot(time_aboveground, aes(sample = mean_p_aboveground)) +
    ggplot2::geom_qq() +
    stat_qq_line() +
    scale_x_continuous()

shapiro.test(time_aboveground %>% pluck("mean_p_aboveground"))
# shapiro.test(time_aboveground %>% filter(season == "July") %>% pluck("mean_p_aboveground"))
# shapiro.test(time_aboveground %>% filter(season == "October") %>% pluck("mean_p_aboveground"))
# shapiro.test(time_aboveground %>% filter(season == "February") %>% pluck("mean_p_aboveground"))

# ANOVA TESTS ------------------------------------------------------------------
# AOV DAILY TIME OF LIGHT EXPOSURE
aov_time_light = aov(data = time_aboveground, formula = mean_t_aboveground~season)
summary(aov_time_light)
TukeyHSD(aov_time_light)

# AOV DAILY PERCENTEGE OF LIGHT EXPOSURE
aov_perc_light = aov(data = time_aboveground, formula = mean_p_aboveground~season)
summary(aov_perc_light)
TukeyHSD(aov_perc_light)

