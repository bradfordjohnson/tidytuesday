pacman::p_load(
  tidyverse,
  ggmap,
  showtext,
  htmltools
)

showtext_auto()
showtext_opts(dpi = 300)

font_add(
  family = "fb", regular = "C:/Users/Bradf/AppData/Local/Microsoft/Windows/Fonts/Font Awesome 6 Brands-Regular-400.otf"
)

font_add_google(name = "Roboto Slab", family = "Roboto Slab")
font_add_google(name = "Roboto", family = "Roboto")
font_1 <- "Roboto Slab"

caption <- paste0(
  "<span style='font-family:fb;'>&#xf09b;</span>",
  "<span style='font-family:sans;color:#FFFFFF;'>.</span>",
  "<span style='font-family:Roboto;'>bradfordjohnson | TidyTuesday - 2023 Week 21</span>"
)

squirrel_data <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-23/squirrel_data.csv") |>
  janitor::clean_names()

top_locations <- squirrel_data |>
  group_by(hectare) |>
  summarise(avg_per_sighting_session = mean(hectare_squirrel_number), n = n()) |>
  arrange(desc(avg_per_sighting_session))

squirrel_data <- squirrel_data |>
  mutate(highlight = case_when(hectare == "14E" ~ "highlight",
    hectare == "14D" ~ "highlight",
    hectare == "32E" ~ "highlight",
    hectare == "07B" ~ "highlight",
    default_value = NULL
  ))

squirrel_data |>
  ggplot(aes(x, y)) +
  geom_point(aes(alpha = hectare_squirrel_number)) +
  theme_void() +
  facet_wrap(~shift) +
  labs(caption = caption, title = "Number of squirrel sightings in one sighting session", subtitle = "Central Park Squirrel Census", alpha = "Squirrels") +
  theme(
    legend.position = "right",
    legend.title = element_text(family = font_1, size = 7, face = "plain", margin = margin(0, 2, 0, 2, "mm")),
    legend.text = element_text(family = font_1, size = 7, face = "plain", margin = margin(0, 2, 0, 2, "mm")),
    plot.caption = ggtext::element_textbox_simple(
      margin = margin(3, 0, 0, 0, "mm"),
      halign = 0, color = "gray10", size = 5
    ),
    plot.title = element_text(family = font_1, size = 12, hjust = 0, face = "plain", margin = margin(0, 0, 2, 0, "mm")),
    plot.subtitle = element_text(family = font_1, size = 10, hjust = 0, face = "italic", margin = margin(0, 0, 2, 0, "mm")),
    plot.margin = unit(c(3, 4, 3, 4), "mm"),
    plot.background = element_rect(fill = "white", colour = "white"),
    panel.background = element_rect(fill = "white", colour = "white"),
    strip.text = element_text(family = font_1, size = 8, face = "bold")
  )

ggsave("squirrels.png", width = 6, height = 6)
