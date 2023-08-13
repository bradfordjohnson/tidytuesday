pacman::p_load(
  tidyverse,
  geofacet,
  ggh4x,
  showtext,
  htmltools
)

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
  "<span style='font-family:sans;color:#FFFFFF;'>.</span>",
  "<span style='font-family:Roboto;'>bradfordjohnson | TidyTuesday - 2023 Week 23</span>"
)

subtitle <- paste0(
  "<span><span style='font-family:Roboto;color:#C32E5A;'>Demand</span><span style='font-family:Roboto;'> vs. </span><span style='font-family:Roboto;color:#3D85F7;'>Generation</span><span style='font-family:Roboto;'> in Terawatt-Hours</span></span>"
)

owid_energy <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-06/owid-energy.csv")

plot_bg <- "#f6f5f0"

owid_energy |>
  mutate(country = ifelse(country == "Czechia", "Czech Republic", country)) |>
  select(country, year, population, gdp, electricity_demand, electricity_generation) |>
  drop_na(electricity_generation, electricity_demand) |>
  ggplot(aes(x = year)) +
  geom_line(aes(y = electricity_generation, color = "Generation")) +
  geom_line(aes(y = electricity_demand, color = "Demand")) +
  stat_difference(aes(ymin = electricity_generation, ymax = electricity_demand), alpha = 0.3) +
  facet_geo(~country, grid = "eu_grid1", scales = "free_y") +
  scale_fill_manual(
    values = c(
      colorspace::lighten("#C32E5A"),
      colorspace::lighten("#3D85F7"),
      "grey60"
    ),
    labels = c("Deficit", "Surplus", "")
  ) +
  scale_color_manual(values = c("#C32E5A", "#3D85F7")) +
  guides(
    color = guide_legend(order = 1),
    fill = guide_legend(order = 2)
  ) +
  labs(
    title = "European Electricity:",
    caption = caption, subtitle = subtitle
  ) +
  scale_x_continuous(breaks = seq(1990, 2020, 15)) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = plot_bg, color = plot_bg),
    panel.background = element_rect(fill = plot_bg, color = plot_bg),
    legend.pos = c(0.875, 1),
    legend.direction = "horizontal",
    legend.key.size = unit(0.5, "lines"),
    legend.text = element_text(size = 8, family = font_1),
    legend.box = "vertical",
    legend.title = element_blank(),
    plot.margin = margin(10, 20, 10, 20),
    plot.title = element_text(
      margin = margin(0, 0, 3, 0, "mm"),
      size = 15,
      family = font_1,
      face = "bold",
      vjust = 0,
      color = "black"
    ),
    plot.caption = ggtext::element_textbox_simple(
      margin = margin(6, 0, 0, 0),
      halign = 0, color = "gray10", size = 5
    ),
    plot.subtitle = ggtext::element_textbox_simple(
      family = font_1, margin = margin(0, 0, 5, 0),
      size = 12
    ),
    axis.title = element_blank(),
    axis.text = element_text(color = "grey40", family = font_2, size = 4),
    strip.text = element_text(face = "bold", color = "grey20", family = font_1, size = 7)
  )

ggsave("owid_energy.png")
