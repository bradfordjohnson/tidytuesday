# packages
library(tidyverse)
library(showtext)
library(htmltools)

showtext_auto()

# load data
age_gaps <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-14/age_gaps.csv')

# wrangle data
age_gaps <- age_gaps |>
  mutate(relationship = case_when(
    character_1_gender == "man" ~ "Older Man; Younger Partner",
    character_1_gender == "woman" ~ "Older Woman; Younger Partner"
    ))


# create base visual
vis <- age_gaps |>
  group_by(relationship, release_year) |>
  summarise(avg_age_diff = mean(age_difference), avg_age_older = mean(actor_1_age), avg_age_younger = mean(actor_2_age)) |>
  ggplot(aes(x = release_year, ymin = avg_age_younger, ymax = avg_age_older, alpha = avg_age_diff)) +
  geom_errorbar() +
  facet_wrap(~relationship, nrow = 2) +
  theme_minimal()

# import fonts
font_add(family = "fb",
         regular = "C:/Users/Bradf/AppData/Local/Microsoft/Windows/Fonts/Font Awesome 6 Brands-Regular-400.otf")

font_add_google(name = "Oswald", family = "Oswald")
font_1 <- "Oswald"

font_add_google(name = "Noto Sans", family = "Noto Sans")
font_2 <- "Noto Sans"

font_add_google(name = "Roboto", family = "Roboto")
font_3 <- "Roboto"

# labs
caption = paste0("<span style='font-family:fb;'>&#xf09b;</span>",
                 "<span style='font-family:sans;color:#F5F5F4;'>.</span>",
                 "<span style='font-family:Roboto;'>bradfordjohnson | TidyTuesday - 2023 Week 7</span>")

# add labs to plot
vis <- vis +
  labs(
    title = "The Age Gap in Hollywood",
    subtitle = "Range of average ages for couples in films by release year, includes same-sex couples.",
    x = "Film Release Year",
    y = "Actors' Ages",
    caption = caption
  )

# customize theme
vis +
  theme(
    plot.title = element_text(family = font_1, hjust = .5, vjust = 0, size = 60, margin = margin(0,0,3,0, unit = "mm")),
    plot.subtitle = element_text(family = font_2, hjust = .5, size = 37),
    plot.caption = ggtext::element_textbox_simple(color="black", size = 28, halign = 1),
    plot.margin = unit(c(10,10,10,10), "pt"),
    axis.title.x = element_text(family = font_2, size = 32, margin = margin(3,0,3,0, unit = "mm"), face = "bold"),
    axis.title.y = element_text(family = font_2, size = 32, margin = margin(0,3,0,0, unit = "mm"), face = "bold"),
    axis.text.x = element_text(family = font_2, size = 28),
    axis.text.y = element_text(family = font_2, size = 28),
    legend.position = "none",
    panel.background = element_rect(fill = "#F5F5F4", color = "#F5F5F4"),
    plot.background = element_rect(fill = "#F5F5F4", color = "#F5F5F4"),
    panel.grid.major = element_line(colour = "black", linetype = "dotted", linewidth = 0.1),
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(),
    strip.text.x = element_text(family = font_2, size = 35, face = "bold")
  )

ggsave("age-gap.png", width = 9, height = 9)
