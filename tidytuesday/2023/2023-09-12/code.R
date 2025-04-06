library(tidyverse)
library(showtext)
library(htmltools)

all_countries <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-12/all_countries.csv') |>
  janitor::clean_names()

country_regions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-12/country_regions.csv') |>
  janitor::clean_names() %>%
  select('country_name', 'country_iso3', 'region_name')

all_countries %>%
  select(subcategory) %>%
  count(subcategory)

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
  "<span style='font-family:sans;color:#e8eae7;'>.</span>",
  "<span style='font-family:Roboto;'>bradfordjohnson | TidyTuesday - 2023 Week 37</span>"
)


all_countries %>%
  group_by(category, country_iso3) %>%
  summarise(total_category_hours = sum(hours_per_day_combined), population = min(population)/1000000000) %>%
  left_join(country_regions, by = 'country_iso3') %>%
  select(region_name, country_name, category, total_category_hours, population) %>%
  arrange(desc(population)) %>%
  mutate(target_country = case_when(
    country_name == 'China' ~ 'China',
    country_name == 'India' ~ 'India',
    TRUE ~ 'Other'
  )) %>%
  ggplot(aes(x=population, y=total_category_hours, color=target_country)) +
  geom_point(alpha = .3) +
  scale_x_log10() +
  facet_wrap(~category, scales = 'free_y') +
  scale_x_continuous(labels = scales::label_comma(suffix="b")) +
  labs(
    title = 'Population vs. daily hours by activity and country',
    x = 'Population',
    y = 'Hours per day',
    color = 'Target Country',
    caption = caption
  ) +
  theme(
    plot.title = element_text(
      margin = margin(2, 0, 3, 0, "mm"),
      size = 8,
      family = font_1,
      face = "bold",
      color = "black"
    ),
    plot.caption = ggtext::element_textbox_simple(
      margin = margin(10, 0, 0, 0),
      halign = .01,
      color = "gray10",
      size = 5
    ),
    axis.title.y = element_text(color = 'black', angle = 90, margin = margin(0, 4, 0, 0, 'mm'), size = 6),
    axis.title.x = element_text(color = 'black', margin = margin(4,0,0,0,"mm") ,size = 6),
    axis.text = element_text(
      size = 6,
      family = font_2,
      color = "black"
    ),
    strip.background = element_blank(),
    strip.text = element_text(
      color = "black",
      family = font_2,
      size = 6
    ),
    legend.title = element_text(
      size = 6,
      family = font_2,
      color = "black"
    ),
    legend.text = element_text(
      size = 5,
      family = font_2,
      color = "black"
    ),
    axis.ticks = element_line(color = "grey40", size = .1),
    panel.grid.major = element_line(color = "grey40", size = .1),
    panel.background = element_blank(),
    plot.margin = margin(10, 20, 10, 20),
  )

ggsave('2023-09-12/global-human-day.png', width = 8, height = 6, dpi = 300)
