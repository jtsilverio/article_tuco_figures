library(dplyr)
library(ggplot2)
library(momentuHMM)

tuco = readRDS("01_data/activity_processed/tuco_processed.rds")

# Read Models ------------------------------------------------------------------
m2 = readRDS("03_analysis/hmm/m2.rds") # modelo com 

# Viterbi State Decoding -------------------------------------------------------
decoded = viterbi(m2)
tuco$state = factor(decoded, labels = c("Rest","Medium","High"))

# Calculate Daily Budgets ------------------------------------------------------
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

shapiro.test(daily_budget_id %>% 
                 filter(state == "Rest") %>% 
                 pluck("time"))
# shapiro.test(daily_budget_id %>%
#                  filter(state == "Rest" & season == "March") %>%
#                  pluck("time"))
# shapiro.test(daily_budget_id %>%
#                  filter(state == "Rest" & season == "July") %>%
#                  pluck("time"))
# shapiro.test(daily_budget_id %>%
#                  filter(state == "Rest" & season == "October") %>%
#                  pluck("time"))
# shapiro.test(daily_budget_id %>%
#                  filter(state == "Rest" & season == "February") %>%
#                  pluck("time"))

shapiro.test(daily_budget_id %>% filter(state == "Medium") %>% pluck("time"))
# shapiro.test(daily_budget_id %>%
#                  filter(state == "Medium" & season == "March") %>%
#                  pluck("time"))
# shapiro.test(daily_budget_id %>%
#                  filter(state == "Medium" & season == "July") %>%
#                  pluck("time"))
# shapiro.test(daily_budget_id %>%
#                  filter(state == "Medium" & season == "October") %>%
#                  pluck("time"))
# shapiro.test(daily_budget_id %>%
#                  filter(state == "Medium" & season == "February") %>%
#                  pluck("time"))

shapiro.test(daily_budget_id %>% filter(state == "High") %>% pluck("time"))
# shapiro.test(daily_budget_id %>%
#                  filter(state == "High" & season == "March") %>%
#                  pluck("time"))
# shapiro.test(daily_budget_id %>%
#                  filter(state == "High" & season == "July") %>%
#                  pluck("time"))
# shapiro.test(daily_budget_id %>%
#                  filter(state == "High" & season == "October") %>%
#                  pluck("time"))
# shapiro.test(daily_budget_id %>%
#                  filter(state == "High" & season == "February") %>%
#                  pluck("time"))


# ANOVA TESTS ------------------------------------------------------------------
# AOV HIGH
aov_high = aov(data = daily_budget_id %>% filter(state == "High"), formula = time~season)
summary(aov_high)
#plot(aov_high)
TukeyHSD(aov_high)

# AOV MEDIUM
aov_medium = aov(data = daily_budget_id %>% filter(state == "Medium"), formula = time~season)
summary(aov_medium)
#plot(aov_medium)
plot(TukeyHSD(aov_medium))

# AOV REST
aov_rest = aov(data = daily_budget_id %>% filter(state == "Rest"), formula = time~season)
summary(aov_rest)
#plot(aov_rest)
plot(TukeyHSD(aov_rest))


