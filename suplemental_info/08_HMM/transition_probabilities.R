library(momentuHMM)
m1 = readRDS("HMM/m1.rds")
m2 = readRDS("HMM/m2.rds")


cov.index = 
    c(which(tuco$season == "March")[1],
      which(tuco$season == "July")[1],
      which(tuco$season == "October")[1],
      which(tuco$season == "February")[1])


df = as.data.frame(
    momentuHMM::getTrProbs(m2, covIndex = cov.index),
)


df$to_state = row.names(df)
df = tidyr::pivot_longer(df, 1:ncol(df)-1, names_to = c("from_state", "season"), values_to = "prob", names_sep = "([.])")
df$season = factor(df$season, levels = 1:4, labels = c("March", "July", "October", "February"))

df$to_state = factor(paste0("To:", df$to_state),
                     levels = c("To:Low","To:Medium","To:High"))
df$from_state = factor(paste0("From:", df$from_state),
                       levels = c("From:Low","From:Medium","From:High"))

transition_probabilities = df %>% 
    ggplot() +
    geom_point(aes(x = season, 
                   y = prob, 
    )) + 
    geom_line(aes(x = season, 
                  y = prob,
                  group = from_state)) +
    facet_grid(from_state ~ to_state, switch = "y") +
    xlab("") +
    ylab("Transition Probabilities") +
    scale_y_continuous(position = "right") + 
    theme(panel.grid.major.y = element_line(color = "grey90"))


ggsave("suplemental_info/08_HMM/transition_probabilities.png", transition_probabilities, dpi = 150)


