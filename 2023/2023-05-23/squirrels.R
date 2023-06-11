pacman::p_load(tidyverse,
               ggmap,
               showtext,
               htmltools)

showtext_auto()
showtext_opts(dpi = 300)

font_add(
  family = "fb", regular = "C:/Users/Bradf/AppData/Local/Microsoft/Windows/Fonts/Font Awesome 6 Brands-Regular-400.otf"
)

squirrel_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-23/squirrel_data.csv') |>
  janitor::clean_names()

top_locations <- squirrel_data |>
  group_by(hectare) |>
  summarise(avg_per_sighting_session = mean(hectare_squirrel_number), n = n()) |>
  arrange(desc(avg_per_sighting_session))

squirrel_data <- squirrel_data |>
  mutate(highlight = case_when(hectare == '14E' ~ 'highlight',
                               hectare == '14D' ~ 'highlight',
                               hectare == '32E' ~ 'highlight',
                               hectare == '07B' ~ 'highlight',
                   default_value = NULL))

squirrel_data |>
  ggplot(aes(x,y)) +
  geom_point(aes(alpha = hectare_squirrel_number)) +
  theme_void() +
  theme(legend.position = "none") +
  facet_wrap(~shift)
