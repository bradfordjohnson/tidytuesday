centenarians <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-30/centenarians.csv")

pacman::p_load(
  tidyverse,
  tidyquant,
  ggdist,
  ggthemes,
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
font_2 <- "Roboto"

caption <- paste0(
  "<span style='font-family:fb;'>&#xf09b;</span>",
  "<span style='font-family:sans;color:#FFFFFF;'>.</span>",
  "<span style='font-family:Roboto;'>bradfordjohnson | TidyTuesday - 2023 Week 22</span>"
)

subtitle <- paste0(
  "<span style='font-family:sans;color:#000000;'>Top 100 <span style='font-family:sans;color:#ff748c;'>Females</span> and Upper Quartile of <span style='font-family:sans;color:#74afff;'>Male</span> Ages</span>"
)

centenarians |>
  mutate(gender = case_when(
    gender == "male" ~ "Male",
    gender == "female" ~ "Female"
  )) |>
  ggplot(aes(x = gender, y = age, fill = gender)) +
  stat_halfeye(
    adjust = 0.5,
    justification = -0.2,
    .width = 0,
    point_colour = NA
  ) +
  geom_boxplot(
    width = 0.12,
    outlier.color = NA,
    alpha = 0.5,
    aes(color = gender)
  ) +
  stat_dots(
    side = "left",
    justification = 1.1,
    aes(colour = gender)
  ) +
  scale_fill_manual(
    values = c(
      "#ff748c",
      "#74afff"
    )
  ) +
  scale_color_manual(
    values = c(
      "#ff748c",
      "#74afff"
    )
  ) +
  theme_tq() +
  labs(
    title = "Verified Oldest 200 People: Age Trends and Subtle Overlap",
    subtitle = subtitle,
    x = "",
    y = "Age",
    caption = caption
  ) +
  coord_flip() +
  guides(
    fill = "none",
    color = "none"
  ) +
  scale_y_continuous(
    limits = c(110, 125)
  ) +
  theme(
    plot.margin = unit(c(4, 4, 4, 4), "mm"),
    plot.title = element_text(family = font_1, size = 15, hjust = 0, face = "plain", margin = margin(0, 0, 2, 0, "mm"), color = "black"),
    plot.subtitle = ggtext::element_textbox_simple(
      family = font_1, margin = margin(2, 0, 2, 0, "mm"),
      size = 12
    ),
    axis.text = element_text(family = font_2, size = 7, color = "black"),
    axis.text.y = element_blank(),
    axis.title = element_text(family = font_2, size = 8, color = "black"),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    plot.background = element_rect(fill = "#FAFAFB", color = "#FAFAFB"),
    panel.background = element_rect(fill = "#FAFAFB", color = "#FAFAFB"),
    panel.grid.major.x = element_line(color = "#d3d3d3", linewidth = .25),
    panel.grid.major.y = element_blank(),
    plot.caption = ggtext::element_textbox_simple(
      margin = margin(3, 0, 0, 0, "mm"),
      halign = .96, color = "gray10", size = 5
    )
  )

ggsave("oldest_people.png")
