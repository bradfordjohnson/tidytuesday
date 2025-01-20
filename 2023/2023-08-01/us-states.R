pacman::p_load(
  tidyverse,
  stringr,
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
  "<span style='font-family:sans;color:#4C566A;'>.</span>",
  "<span style='font-family:Roboto;'>bradfordjohnson | TidyTuesday - 2023 Week 31</span>"
)

title <- paste0(
  "<span>US State Demonyms: Embracing the<span style='color:#D08770;'> 'n' </span>Addition</span><span style='font-family:Roboto;'></span>"
)

subtitle <-
  paste0("<span>Alaska > Alaska<span style='color:#D08770;'>n</span></span>")

states <-
  readr::read_csv(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-08-01/states.csv"
  )

text_col <- "#ECEFF4"
text_col_2 <- "#D8DEE9"
bg_color <- "#4C566A"

find_differences <- function(string1, string2) {
  chars1 <- strsplit(string1, "")[[1]]
  chars2 <- strsplit(string2, "")[[1]]

  differences <-
    mapply(function(x, y) {
      ifelse(x == y, " ", y)
    }, chars1, chars2)
  paste(differences, collapse = "")
}

states$differences <-
  mapply(find_differences, states$state, states$demonym)

states$differences <- str_replace_all(states$differences, " ", "")

states |>
  group_by(differences) |>
  summarise(n = n()) |>
  arrange(desc(n)) |>
  mutate(differences = factor(differences, levels = differences)) |>
  mutate(is_n = case_when(differences == "n" ~ 1, differences != "n" ~ 0)) |>
  ggplot(aes(
    x = differences,
    y = n,
    fill = factor(is_n),
    color = factor(is_n)
  )) +
  geom_col() +
  scale_fill_manual(values = c("#81A1C1", "#D08770")) +
  scale_color_manual(values = c("#81A1C1", "#D08770")) +
  geom_text(
    aes(label = n, y = n + .3),
    vjust = 0,
    color = text_col,
    size = 2
  ) +
  theme_void() +
  labs(
    title = title,
    caption = caption,
    subtitle = subtitle
  ) +
  theme(
    plot.background = element_rect(fill = bg_color, color = bg_color),
    panel.background = element_rect(fill = bg_color, color = bg_color),
    legend.position = "none",
    axis.text.x = element_text(
      color = text_col,
      size = 6,
      vjust = 1.4
    ),
    plot.margin = margin(5, 5, 5, 5, "mm"),
    plot.caption = ggtext::element_textbox_simple(
      margin = margin(6, 0, 0, 0, "mm"),
      halign = 1,
      color = text_col,
      size = 5,
    ),
    plot.title = ggtext::element_textbox_simple(
      family = font_1,
      margin = margin(0, 0, 2, 0, "mm"),
      halign = .1,
      size = 15,
      color = text_col_2
    ),
    plot.subtitle = ggtext::element_textbox_simple(
      family = font_1,
      margin = margin(0, 0, 5, 0, "mm"),
      halign = .1,
      size = 10,
      color = text_col_2
    )
  )

ggsave("us-states.png",
  width = 6,
  height = 4,
  dpi = 300
)
