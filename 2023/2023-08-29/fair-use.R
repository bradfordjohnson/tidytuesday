pacman::p_load(tidyverse, htmltools, showtext, jsonlite)

source("functions/caption.r")

showtext_auto()
showtext_opts(dpi = 300)

font_add(family = "fb", regular = "assets/fontAwesome.otf")

font_add_google(name = "Roboto", family = "Roboto")
font_1 <- "Roboto"

colors_json <- fromJSON("assets/colors.json")

background_color <- colors_json$nord$polar_night$nord3

text_color <- colors_json$nord$snow_storm$nord4

label_color <- colors_json$nord$polar_night$nord3

color_pal <-
  c(
    colors_json$nord$frost$nord9,
    colors_json$nord$snow_storm$nord4
  )

caption <- create_caption(2023, 35, background_color, font = font_1)

title <-
  "<span>Fair Use Analysis: Top Categories and Court Findings</span>"

subtitle <-
  "<span>Fair use<span style='color:#81A1C1;'> not found</span> vs. found</span>"

fair_use_cases <-
  readr::read_csv(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-08-29/fair_use_cases.csv"
  )

fair_use_cases$categories <-
  str_to_title(str_remove(fair_use_cases$categories, ";.*")) # remove everything after ;

target_categories <- fair_use_cases %>%
  group_by(categories) %>%
  count() %>%
  arrange(desc(n)) %>%
  head(8) %>%
  pull(categories) %>%
  unname()

fair_use_cases %>%
  group_by(categories, fair_use_found) %>%
  count() %>%
  mutate(n = case_when(
    fair_use_found == TRUE ~ n * 1,
    fair_use_found == FALSE ~ n * -1
  )) %>%
  mutate(lab_y = case_when(n > 0 ~ 2, n < 0 ~ -3)) %>%
  filter(categories %in% target_categories) %>%
  ggplot(aes(
    x = reorder(categories, -n),
    y = n,
    fill = fair_use_found
  )) +
  geom_col() +
  scale_fill_manual(values = color_pal) +
  geom_text(
    aes(
      label = abs(n),
      y = lab_y
    ),
    vjust = 0,
    color = label_color,
    size = 2
  ) +
  coord_flip() +
  labs(
    title = title,
    caption = caption,
    subtitle = subtitle,
    y = "Case count (1841-2022)"
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = background_color, color = background_color),
    panel.background = element_rect(fill = background_color, color = background_color),
    plot.margin = margin(5, 5, 5, 5, "mm"),
    plot.caption = ggtext::element_textbox_simple(
      margin = unit(c(6, 0, 0, 0), "mm"),
      halign = 1,
      color = text_color,
      size = 5
    ),
    plot.title = ggtext::element_textbox_simple(
      family = font_1,
      margin = margin(0, 0, 2, 0, "mm"),
      halign = 0,
      size = 12,
      color = text_color
    ),
    plot.subtitle = ggtext::element_textbox_simple(
      family = font_1,
      margin = margin(2, 0, 2, 0, "mm"),
      halign = .575,
      size = 10,
      color = text_color
    ),
    axis.text.y = element_text(color = text_color, font_1, size = 5, margin = margin(0, 0, 0, 2, "mm")),
    axis.title.x = element_text(color = text_color, font_1, size = 5, hjust = .64),
    legend.position = "none"
  )
