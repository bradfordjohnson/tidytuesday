pacman::p_load(
  tidyverse,
  showtext,
  htmltools
)

showtext_auto()
showtext_opts(dpi = 300)

font_add(
  family = "fb", regular =
    "C:/Users/Bradf/AppData/Local/Microsoft/Windows/Fonts/Font Awesome 6 Brands-Regular-400.otf"
)

font_add_google(name = "Roboto Slab", family = "Roboto Slab")
font_1 <- "Roboto Slab"

font_add_google(name = "Roboto", family = "Roboto")
font_2 <- "Roboto"

caption <- paste0(
  "<span style='font-family:fb;'>&#xf09b;</span>",
  "<span style='font-family:sans;color:#3B4252;'>.</span>",
  "<span style='font-family:Roboto;'>bradfordjohnson | TidyTuesday - 2023 Week 32</span>"
)

title <- paste0(
  "<span>An Increase in Heat: Hot Ones</span>"
)

subtitle <-
  paste0("<span style='color:#BF616A;'> Seasons 1-3 </span><span style='font-family:Roboto;'>Not as Hot as Following Seasons</span>")

sauces <-
  readr::read_csv(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-08-08/sauces.csv"
  )
episodes <-
  readr::read_csv(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-08-08/episodes.csv"
  )

lower_heat <- sauces |>
  filter(sauce_number == 10) |>
  arrange(scoville) |>
  head(5)

colors <- c(
  "#BF616A", "#BF616A", "#BF616A", "#E5E9F0", "#E5E9F0", "#E5E9F0", "#E5E9F0", "#E5E9F0", "#E5E9F0", "#E5E9F0",
  "#E5E9F0", "#E5E9F0", "#E5E9F0", "#E5E9F0", "#E5E9F0", "#E5E9F0", "#E5E9F0", "#E5E9F0", "#E5E9F0", "#E5E9F0", "#E5E9F0"
)

bg_color <- "#3B4252"
text_color <- "#E5E9F0"
text_color_2 <- "#BF616A"

sauces %>%
  ggplot(aes(x = sauce_number, y = scoville, color = factor(season))) +
  geom_line(alpha = 0.5) +
  scale_color_manual(values = colors) +
  scale_x_continuous(breaks = 1:10) +
  scale_y_continuous(labels = function(x) ifelse(x >= 1000, paste0(x / 1000, "k"), x)) + # Format labels to thousands with 'k'
  theme_void() +
  labs(x = "Sauce Number", y = "Scoville Scale", title = title, caption = caption, subtitle = subtitle) +
  geom_text(
    data = subset(sauces, sauce_number == 10 & season %in% c(1, 2)),
    aes(label = ifelse(season == 2, "2 & 3", "1"), y = ifelse(season == 1, scoville, scoville)),
    hjust = 1, vjust = -.5, size = 2, color = text_color_2
  ) +
  theme(
    plot.background = element_rect(fill = bg_color, color = bg_color),
    panel.background = element_rect(fill = bg_color, color = bg_color),
    axis.text.x = element_text(color = text_color, size = 5),
    axis.title.x = element_text(color = text_color, margin = margin(10, 0, 0, 0, "mm"), size = 6),
    axis.text.y = element_text(color = text_color, size = 5),
    axis.title.y = element_text(color = text_color, angle = 90, margin = margin(0, 10, 0, 0, "mm"), size = 6),
    legend.position = "none",
    plot.margin = margin(10, 10, 10, 10, "mm"),
    plot.caption = ggtext::element_textbox_simple(
      margin = margin(6, 0, 0, 0, "mm"),
      halign = 1,
      color = text_color,
      size = 5,
    ),
    plot.title = ggtext::element_textbox_simple(
      family = font_1,
      margin = margin(0, 0, 2, 0, "mm"),
      halign = .1,
      size = 12,
      color = text_color
    ),
    plot.subtitle = ggtext::element_textbox_simple(
      family = font_1,
      margin = margin(0, 0, 5, 0, "mm"),
      halign = .1,
      size = 10,
      color = text_color
    )
  )
