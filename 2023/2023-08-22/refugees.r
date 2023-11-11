library(tidyverse)
library(showtext)
library(htmltools)

source("functions/caption.r")

font_add(family = "fb", regular =
           "assets/fontAwesome.otf")
showtext_auto()
showtext_opts(dpi = 300)
font_add_google(name = "Roboto", family = "Roboto")
font_1 <- "Roboto"

cap <- create_caption(2023, 1, background_color = "#000000", font = font_1)

ggplot() +
    labs(caption = cap) +
    theme(plot.caption = ggtext::element_textbox_simple(
        margin = margin(0, 0, 0, 0, "mm"),
        halign = 1,
        color = "black",
        size = 10,
    ))
print(cap)
