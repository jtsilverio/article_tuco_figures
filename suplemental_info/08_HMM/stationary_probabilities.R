library(momentuHMM)
m1 = readRDS("HMM/m1.rds")
m2 = readRDS("HMM/m2.rds")


# get stationary probabilities
df = data.frame(
    March = momentuHMM::stationary(m2, covs = data.frame(season = "March")),
    July = momentuHMM::stationary(m2, covs = data.frame(season = "July")),
    October = momentuHMM::stationary(m2, covs = data.frame(season = "October")),
    February = momentuHMM::stationary(m2, covs = data.frame(season = "February"))
)
df = tidyr::pivot_longer(df, 1:ncol(df), names_to = c("season", "state"), values_to = "prob", names_sep = "([.])")
df$season = factor(df$season, levels = c("March", "July", "October", "February"))

# plot stationary prababilities
stationary_probabilities = ggplot(df) +
    geom_point(aes(x = season, y = prob, color = state, group=state)) +
    geom_line(aes(x = season, y = prob, color = state, group=state)) +
    scale_y_continuous(limits = c(0, 0.5)) +
    ylab("Stationary Probabilities") +
    xlab("") +
    scale_color_manual(values = tuco_pal[1:3])

ggsave("suplemental_info/08_HMM/stationary_probabilities.png", stationary_probabilities, dpi = 150, width = 900, height = 700, units = "px")

