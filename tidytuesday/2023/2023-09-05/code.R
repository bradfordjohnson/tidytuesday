pacman::p_load(tidyverse, htmltools, showtext, jsonlite)

source("2023/functions/caption.r")

showtext_auto()
showtext_opts(dpi = 300)

font_add(family = "fb", regular = "assets/fontAwesome.otf")

font_add_google(name = "Roboto", family = "Roboto")
font_1 <- "Roboto"

colors_json <- fromJSON("assets/colors.json")

background_color <- colors_json$nord$snow_storm$nord5

text_color <- colors_json$nord$polar_night$nord1

grid_color <- colors_json$nord$polar_night$nord3

color_pal <- c(
  colors_json$nord$aurora$nord15,
  colors_json$nord$frost$nord8,
  colors_json$nord$aurora$nord12
)

caption <- create_caption(2023, 36, background_color, font = font_1)

title <-
  "<span>Union Wage Premium: Sectoral Insights and <span style='color:#D08770;'>Retail's </span>Noteworthy Shift</span>"

wages <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-05/wages.csv")

wages_filtered <- wages[!grepl("demographics", wages$facet, ignore.case = TRUE), ]

target_facets <- c("private sector: all", "wholesale/retail", "public sector: all")

wages_filtered %>%
  filter(facet %in% target_facets) %>%
  ggplot(aes(x = year, y = union_wage_premium_raw / 100, color = facet)) +
  geom_line() +
  scale_color_manual(values = color_pal) +
  labs(caption = caption, title = title, y = "Difference in union vs. nonunion wage (%)") +
  scale_y_continuous(labels = scales::percent) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = background_color, color = background_color),
    panel.background = element_rect(fill = background_color, color = background_color),
    plot.margin = margin(5, 5, 5, 5, "mm"),
    plot.caption = ggtext::element_textbox_simple(
      margin = unit(c(6, 0, 0, 0), "mm"),
      halign = 0,
      color = text_color,
      size = 4
    ),
    plot.title = ggtext::element_textbox_simple(
      family = font_1,
      margin = margin(5, 0, 2, 0, "mm"),
      halign = 0,
      size = 10,
      color = text_color
    ),
    axis.text.y = element_text(color = text_color, family = font_1, size = 6, margin = margin(0, 2, 0, 0, "mm")),
    axis.text.x = element_text(color = text_color, family = font_1, size = 6, margin = margin(2, 0, 0, 0, "mm")),
    axis.line = element_line(color = grid_color),
    axis.title.y = element_text(color = text_color, family = font_1, size = 6, angle = 90, margin = margin(0, 5, 0, 0, "mm")),
    legend.title = element_blank(),
    legend.position = "top",
    legend.margin = margin(0, 0, 0, 0, "mm"),
    legend.text = element_text(color = text_color, family = font_1, size = 6, margin = margin(0, 0, 0, 0, "mm")),
  )
