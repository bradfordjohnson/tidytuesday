pacman::p_load(tidyverse,
               collapse,
               showtext,
               htmltools)

showtext_auto()
showtext_opts(dpi = 300)

font_add(
  family = "fb", regular = "C:/Users/Bradf/AppData/Local/Microsoft/Windows/Fonts/Font Awesome 6 Brands-Regular-400.otf"
)

font_add_google(name = "Roboto Slab", family = "Roboto Slab")
font_1 <- "Roboto Slab"

font_add_google(name = "Roboto", family = "Roboto")
font_2 <- "Roboto"

caption <- paste0(
  "<span style='font-family:fb;'>&#xf09b;</span>",
  "<span style='font-family:sans;color:#F0EDE8;'>.</span>",
  "<span style='font-family:Roboto;'>bradfordjohnson | TidyTuesday - 2023 Week 27</span>"
)

plot_bg <- "#F8FBF9"
text_col <- "#20202A"
col_col <- "#7F879C"

historical_markers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-04/historical_markers.csv')

missing_by_year <- historical_markers |>
  drop_na(missing) |>
  drop_na(year_erected) |>
  group_by(missing, year_erected) |>
  count()

change_markers_by_year <- historical_markers |>
  drop_na(year_erected) |>
  group_by(year_erected) |>
  summarise(n_markers = n()) |>
  filter(n_markers != 1) |>
  arrange(year_erected) |>
  fmutate(growth = fgrowth(n_markers))

change_markers_by_year |>
  ggplot(aes(x = year_erected, y = growth)) +
  geom_col(color = col_col, fill = col_col) +
  scale_x_continuous(breaks = seq(1848, 2025, by = 25)) +
  scale_y_continuous(labels = scales::percent_format(scale = 1, big.mark = ",")) +
  theme_minimal() +
  labs(title = "Growth Rate of Historical Markers in the US",
       caption = caption,
       x = "Year Erected",
       y = "Growth Rate"
       ) +
  theme(
    plot.background = element_rect(fill = plot_bg, color = plot_bg),
    panel.background = element_rect(fill = plot_bg, color = plot_bg),
    plot.margin = margin(10, 10, 10, 10),
    plot.caption = ggtext::element_textbox_simple(
      margin = margin(6, 0, 0, 0),
      halign = 1, color = text_col, size = 7
    ),
    plot.title = element_text(family = font_1, color = text_col, margin = margin(5,0,5,0), hjust = .5),
    axis.title = element_text(family = font_2, color = text_col, margin = margin(1,1,1,1)),
    axis.text = element_text(family = font_2, color = text_col, margin = margin(4,4,4,4))
  )

ggsave("historical-markers.png")
