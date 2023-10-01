library(readxl)
library(lubridate)
library(dplyr)
library(egg)
library(patchwork)
library(maptools)

source("utils/tuco_theme.R")



# 2017 --------------------------------------------------------------------
weather.2017 = readxl::read_xlsx(
    path = "data/weather/2017 - 04JAN-31DEZ.xlsx",
    col_names = TRUE,
    na = "---",
    col_types = "guess"
)
datetime = ymd_hms(paste(strftime(weather.2017[[1]], format="%Y-%m-%d", tz = "UTC"), strftime(weather.2017[[2]], format="%H:%M:%S", tz = "UTC"), sep = " "))
weather.2017 = cbind(datetime, weather.2017)


# 2019 --------------------------------------------------------------------
weather.2019 = readxl::read_xlsx(
    path = "data/weather/2019 - 01JAN-31DEZ.xlsx",
    col_names = TRUE,
    na = "---",
    col_types = "guess"
)
datetime = ymd_hms(paste(strftime(weather.2019[[1]], format="%Y-%m-%d", tz = "UTC"), strftime(weather.2019[[2]], format="%H:%M:%S", tz = "UTC"), sep = " "))
weather.2019 = cbind(datetime, weather.2019)

# Join data ---------------------------------------------------------------
weather = full_join(weather.2017,weather.2019)
weather$rain = as.numeric(weather$rain)
weather$datetime = as_datetime(weather$datetime, tz="America/Argentina/La_Rioja")
weather$temp.out = as.numeric(weather$temp.out)
weather$hi.temp = as.numeric(weather$hi.temp)
weather$low.temp = as.numeric(weather$low.temp)

anillaco = matrix(c(-66.95, -28.8), nrow = 1)
weather$sunrise = sunriset(anillaco, weather$datetime, direction="sunrise", POSIXct.out=TRUE)$time
weather$sunset = sunriset(anillaco, weather$datetime, direction="sunset", POSIXct.out=TRUE)$time

weather.daily = weather %>%
    mutate(day = cut(datetime, breaks="day")) %>%
    mutate(daytime = ifelse(hour(datetime) > hour(sunrise) & hour(datetime) < hour(sunset), "Day", "Night")) %>% 
    group_by(day, daytime) %>% 
    summarise(rain = sum(rain), 
              max.temp = max(hi.temp), 
              min.temp = min(low.temp), 
              avg.temp = mean(temp.out),
              month = lubridate::month(datetime, label = T),
              year = lubridate::year(datetime)) %>% 
    distinct()
weather.daily$day = as_date(weather.daily$day)

weather.monthly = weather.daily %>% 
    group_by(month) %>% 
    summarise(rain = sum(rain), avg.temp = mean(avg.temp), mean_rain =  mean(rain)) 

# PLOT --------------------------------------------------------------------
temp = 
    ggplot(data = weather.daily) +
    geom_jitter(aes(x =  month, y = avg.temp), color = "grey95") +  
    geom_boxplot(aes(x = month, y = avg.temp, color = daytime), alpha = 1) +
    theme(panel.grid.major.y = element_line(color = "grey85", linetype = 3)) +
    xlab("") +
    ylab("Daily Mean Temperature [\U00B0 C]") +
    scale_color_manual(values = c("#edae49","#2e4057"))

rain = ggplot(data = weather.monthly) +
    geom_bar(aes(x = month, y = rain), stat = "identity", fill = "lightblue") +
    theme(panel.grid.major.y = element_line(color = "grey85", linetype = 3)) +
    xlab("") +
    ylab("Precipitation [mm]")

climograph = temp / rain + plot_annotation(tag_levels = 'A')

ggsave("suplemental_info/03_weather/plot_weather.png", climograph, "png", width = 9, height = 10, dpi = 150)


