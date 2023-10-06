pacman::p_load(tidyverse,
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

subtitle <- paste0(
  "<span><span style='font-family:Roboto;color:#CA4D55;'>2020 </span><span style='font-family:Roboto;'>had the highest average deviations</span></span>"
)

caption <- paste0(
  "<span style='font-family:fb;'>&#xf09b;</span>",
  "<span style='font-family:sans;color:#F5F9F5;'>.</span>",
  "<span style='font-family:Roboto;'>bradfordjohnson | TidyTuesday - 2023 Week 28</span>"
)

global_temps <- readr::read_csv(
  'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-11/global_temps.csv'
) |>
  janitor::clean_names()

global_temps <- global_temps |>
  filter(year >= 1922 & year <= 2022) |>
  pivot_longer(cols = 2:19,
               names_to = 'month',
               values_to = 'value') |>
  filter(month != c('j_d', 'd_n', 'djf', 'mam', 'jja', 'son')) |>
  mutate(month = str_to_upper(month))

temps_means <- global_temps |>
  group_by(year) |>
  summarise(mean_value = mean(value)) |>
  arrange(desc(mean_value)) |>
  head(1) |>
  mutate(is_max = TRUE)

global_temps <- global_temps |>
  left_join(temps_means, by = 'year') |>
  replace_na(list(is_max = FALSE))

bg_color <- '#F5F9F5'
text_col <- '#2B303A'
focus_col <- '#CA4D55'

global_temps |>
  ggplot(aes(
    x = year,
    y = value,
    group = year,
    color = factor(is_max)
  )) +
  geom_hline(
    yintercept = 0,
    color = '#2B303A',
    alpha = .5,
    linetype = 2
  ) +
  geom_boxplot(outlier.shape = NA) +
  theme_minimal() +
  labs(
    title = 'Global Temperature Deviations from means recorded in 1951-1980',
    subtitle = subtitle,
    color = 'Mean Deviation (Celsius)',
    y = 'Temperature Deviation (Celsius)',
    caption = caption
  ) +
  scale_color_manual(values = c('#9DA1A3', focus_col)) +
  theme(
    plot.background = element_rect(fill = bg_color, color = bg_color),
    panel.background = element_rect(fill = bg_color, color = bg_color),
    panel.grid = element_blank(),
    legend.position = 'none',
    axis.title.x = element_blank(),
    axis.title.y = element_text(
      family = font_2,
      color = text_col,
      size = 10,
      margin = margin(0, 5, 0, 10)
    ),
    axis.text.x = element_text(
      family = font_2,
      color = text_col,
      size = 9,
      margin = margin(5, 10, 0, 0)
    ),
    axis.text.y = element_text(
      family = font_2,
      color = text_col,
      size = 9,
      margin = margin(0, 5, 0, 0)
    ),
    plot.title = ggtext::element_textbox_simple(
      family = font_1, 
      margin = margin(5, 0, 5, 0),
      halign = .1,
      size = 13,
      color = text_col),
    plot.subtitle = ggtext::element_textbox_simple(
      family = font_1, 
      margin = margin(5, 0, 5, 0),
      halign = .1,
      size = 12,
      color = text_col),
    plot.caption = ggtext::element_textbox_simple(
      margin = margin(6, 0, 0, 0),
      halign = 1, color = text_col, size = 4.5
    ),
    plot.margin = margin(10, 10, 10, 10),
  )

ggsave("global-temps.png")
