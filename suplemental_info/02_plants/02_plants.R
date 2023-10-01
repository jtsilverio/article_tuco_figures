library(ggplot2)
library(egg)
library(patchwork)
source("utils/tuco_theme.R")

plants = read.csv("data/plants/plant_species.csv")
plants = data.frame(table(plants))
plants = plants[order(plants$Freq, decreasing = T),]
n = sum(plants$Freq)


plot_family = ggplot(plants, aes(reorder(family, -(Freq)), Freq)) +
    geom_bar(stat = "identity", fill = "darkolivegreen", color = "darkolivegreen") +
    xlab("") +
    ylab("Frequency") +
    theme(panel.grid.major.y = element_line(color = "grey85", linetype = 3))


plot_species = ggplot(plants, aes(reorder(Species, -(Freq/sum(Freq))), Freq)) +
    geom_bar(stat = "identity", fill = "darkolivegreen", color = "darkolivegreen") +
    xlab("") +
    ylab("Frequency") +
    scale_x_discrete() +
    theme(panel.grid.major.y = element_line(color = "grey85",
                                            linetype = 3),
          axis.text.x = element_text(angle = 45,
                                     vjust = 0.9,
                                     hjust=1)
        )

plot_plants = plot_family / plot_species + plot_annotation(tag_levels = 'A')

ggsave("suplemental_info/02_plants/plot_plants.png", plot_plants, "png", dpi=150, width = 1200, height = 1200, units = "px", bg = "white")
