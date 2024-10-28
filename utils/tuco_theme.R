library(sysfonts)
library(showtext)
library(jsonlite)
library(curl)

# Set Theme --------------------------------------------------------------------
font_add_google("Roboto Condensed", "roboto")
showtext_auto()

tuco_pal = c("Low" = "#66C2A5",
             "Medium" = "#5a69af",
             "High" = "#c25b67",
             "General Activity" = "#393939")

theme_tuco =
    egg::theme_article() +
    theme(
        text = element_text(size = 16, family = "roboto"),
        plot.background = element_rect(fill = 'white')  
    )

theme_set(theme_tuco)