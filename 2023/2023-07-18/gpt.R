pacman::p_load(
  tidyverse,
  showtext,
  htmltools
)

showtext_auto()
showtext_opts(dpi = 300)

font_add(family = "fb", regular = "C:/Users/Bradf/AppData/Local/Microsoft/Windows/Fonts/Font Awesome 6 Brands-Regular-400.otf")

font_add_google(name = "Roboto Slab", family = "Roboto Slab")
font_1 <- "Roboto Slab"

font_add_google(name = "Roboto", family = "Roboto")
font_2 <- "Roboto"

caption <- paste0(
  "<span style='font-family:fb;'>&#xf09b;</span>",
  "<span style='font-family:sans;color:#F5F9F5;'>.</span>",
  "<span style='font-family:Roboto;'>bradfordjohnson | TidyTuesday - 2023 Week 29</span>"
)

bg_color <- "#F6EAD9"
text_col <- "#2B303A"

detectors <-
  readr::read_csv(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-18/detectors.csv"
  ) |>
  janitor::clean_names()

detectors |>
  group_by(kind, pred_class, native) |>
  summarise(n = n()) |>
  filter(kind == "Human") |>
  mutate(
    pct = n / sum(n),
    native = case_when(
      native == "No" ~ "Non-native English Speaker",
      native == "Yes" ~ "Native English Speaker"
    )
  ) |>
  ggplot(aes(x = pred_class, y = pct, fill = native)) +
  geom_col() +
  geom_text(
    aes(label = scales::percent(pct)),
    vjust = -0.5,
    family = font_2,
    size = 2
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("#5b515c", "#f77b4e")) +
  labs(
    title = "Essays written by non-native English speakers\n disproportionately classified as being written by AI",
    x = "Predicted Classification",
    y = "% of Essays Classified",
    caption = caption
  ) +
  facet_wrap(~native) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = bg_color, color = bg_color),
    panel.background = element_rect(fill = bg_color, color = bg_color),
    legend.position = "none",
    plot.margin = margin(10, 10, 10, 10, "mm"),
    panel.grid = element_blank(),
    plot.caption = ggtext::element_textbox_simple(
      margin = margin(6, 0, 0, 0, "mm"),
      halign = 1,
      color = text_col,
      size = 4.5,
    ),
    plot.title = ggtext::element_textbox_simple(
      family = font_1,
      margin = margin(5, 0, 7, 0, "mm"),
      halign = 0,
      size = 13,
      color = text_col
    ),
    strip.text = element_text(
      family = font_2,
      size = 8,
      color = text_col,
      margin = margin(4, 0, 4, 0, "mm")
    ),
    axis.text = element_text(
      family = font_2,
      size = 8,
      color = text_col
    ),
    axis.title = element_text(
      family = font_2,
      size = 8,
      color = text_col
    ),
    axis.title.y = element_text(margin = margin(0, 10, 0, 0, "mm")),
    axis.text.y = element_blank()
  )

ggsave("gpt.png")
