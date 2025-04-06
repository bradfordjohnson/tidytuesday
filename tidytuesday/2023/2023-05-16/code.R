pacman::p_load(
  tidyverse,
  ggmap,
  showtext,
  htmltools
)

showtext_auto()
showtext_opts(dpi = 300)

tornados <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-16/tornados.csv") |>
  drop_na()

tor_mag_5 <- tornados |>
  filter(mag == 5) |>
  mutate(post = case_when(
    yr >= 2000 ~ "Post-2000",
    yr < 2000 ~ "Pre-2000"
  ))

bbox <- c(bottom = 25.75, top = 49, right = -67, left = -125)

usmap <- get_stamenmap(bbox = bbox, zoom = 6, maptype = "toner-lines")

font_add(
  family = "fb", regular = "C:/Users/Bradf/AppData/Local/Microsoft/Windows/Fonts/Font Awesome 6 Brands-Regular-400.otf"
)

font_add_google(name = "Roboto Slab", family = "Roboto Slab")
font_add_google(name = "Roboto", family = "Roboto")
font_1 <- "Roboto Slab"

caption <- paste0(
  "<span style='font-family:fb;'>&#xf09b;</span>",
  "<span style='font-family:sans;color:#FFFFFF;'>.</span>",
  "<span style='font-family:Roboto;'>bradfordjohnson | TidyTuesday - 2023 Week 20</span>"
)

ggmap(usmap) +
  geom_segment(
    data = tornados,
    aes(
      x = slon,
      y = slat,
      xend = elon,
      yend = elat,
      # alpha = mag,
      colour = factor(mag)
    )
  ) +
  theme_void() +
  scale_colour_manual(
    values = c(
      "#FFF5DC",
      "#F6E3BB",
      "#E8C584",
      "#DDAD58",
      "#90651C",
      "#382301"
    )
  ) +
  labs(
    colour = "Magnitude (Fujita Scale)",
    title = "Linear paths of Tornados in the US",
    subtitle = "1950-2022",
    caption = caption
  ) +
  theme(
    plot.caption = ggtext::element_textbox_simple(
      margin = margin(3, 0, 0, 0, "mm"),
      halign = 0, color = "gray10", size = 5
    ),
    plot.title = element_text(family = font_1, size = 12, hjust = .5, margin = margin(0, 0, 2, 0, "mm")),
    plot.subtitle = element_text(family = font_1, size = 10, hjust = .5, margin = margin(0, 0, 2, 0, "mm")),
    legend.title = element_text(family = font_1, size = 8, margin = margin(0, 2, 0, 2, "mm")),
    legend.text = element_text(family = font_1, size = 8),
    plot.margin = unit(c(2, 2, 2, 2), "mm"),
    plot.background = element_rect(fill = "white", colour = "white"),
    panel.background = element_rect(fill = "white", colour = "white")
  )

ggsave("tornados.png")
