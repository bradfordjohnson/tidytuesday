# load packages
library(tidyverse)
library(cowplot)
library(showtext)
showtext_auto()

font_add_google("Roboto Mono", "Roboto Mono")
font_add_google("Open Sans", "Open Sans")
font_add_google("Ubuntu", "Unbuntu")
font_add(
  family = "fb", regular = "C:/Users/Bradf/AppData/Local/Microsoft/Windows/Fonts/Font Awesome 6 Brands-Regular-400.otf"
)

## create caption
caption = paste0(
  "<span style='font-family:fb;color:#7A7973;'>&#xf09b;</span>", "<span style='font-family:sans;color:#FFFFFF;'>.</span>",
  "<span style='font-family:Unbuntu;color:#7A7973;'>bradfordjohnson | TidyTuesday - Week 4</span>"
)

# Set ggplot theme
theme_set(theme_minimal(base_family = "Open Sans"))
theme_update(
  plot.background = element_rect(
    fill = "#fafaf5",
    color = "#fafaf5"
  ),
  panel.background = element_rect(fill = NA, color = NA),
  panel.border = element_rect(fill = NA, color = NA),
  panel.grid.major.x = element_blank(),
  panel.grid.minor = element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_text(size = 28, face = "bold"),
  axis.ticks = element_blank(),
  axis.title.y = element_text(
    face = "bold", size = 32,
    margin = margin(r = 10)
  ),
  plot.caption = ggtext::element_textbox_simple(
    color = "#444444",
    size = 35
  ),
  plot.margin = margin(10, 10, 10, 10, unit = "pt"),
  plot.title = element_text(
    size = 55, face = "bold",
    hjust = 0.5
  ),
  plot.subtitle = element_text(
    size = 30, hjust = 0.5,
    lineheight = 0.4,
    vjust = -0.5
  )
)

# Turn on showtext
showtext_auto()

# load data
episodes <- readr::read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-24/episodes.csv"
)

# clean data
episodes <- episodes |>
  filter(season != 9)

df_alone_avg <- episodes |>
  arrange(season, episode) |>
  mutate(episode_id = row_number()) |>
  group_by(season) |>
  mutate(
    avg = mean(imdb_rating),
    episode_mod = episode_id + (9 * season),
    mid = mean(episode_mod)
  ) |>
  ungroup() |>
  mutate(season = factor(season))

df_lines <- df_alone_avg |>
  group_by(season) |>
  summarize(
    start_x = min(episode_mod) -
      5, end_x = max(episode_mod) +
      5, y = unique(avg)
  ) |>
  pivot_longer(
    cols = c(start_x, end_x),
    names_to = "type", values_to = "x"
  ) |>
  mutate(
    x_group = if_else(type == "start_x", x + 0.1, x - 0.1),
    x_group = if_else(
      type == "start_x" & x == min(x),
      x_group - 0.1, x_group
    ),
    x_group = if_else(
      type == "end_x" & x == max(x),
      x_group + 0.1, x_group
    )
  )

p <- df_alone_avg |>
  ggplot(aes(episode_mod, imdb_rating)) +
  geom_hline(
    data = tibble(y = 7:10),
    aes(yintercept = y),
    color = "grey82", linewidth = 0.5
  )

p <- p + geom_segment(
  aes(
    xend = episode_mod, yend = avg, color = season,
    color = after_scale(colorspace::lighten(color, 0.2))
  )
)

p <- p + geom_line(
  data = df_lines, aes(x, y),
  color = "grey40"
) +
  geom_line(
    data = df_lines, aes(
      x_group, y, color = season,
      color = after_scale(colorspace::darken(color, 0.2))
    ),
    size = 2.5
  ) +
  geom_point(aes(size = n_ratings, color = season))

p

p <- p + geom_label(
  aes(
    mid, 10.12, label = glue::glue(" Season {season} "),
    color = season,
    color = after_scale(colorspace::darken(color, 0.2))
  ),
  fill = NA, family = "Unbuntu",
  label.padding = unit(0.2, "lines"),
  label.r = unit(0.25, "lines"),
  label.size = 0.5, size = 12
)
p

p <- p + scale_x_continuous(expand = c(0.015, 0.015)) +
  scale_y_continuous(
    expand = c(0.03, 0.03),
    limits = c(6.5, 10.2),
    breaks = seq(6.5, 10, by = 0.5),
    sec.axis = dup_axis(name = NULL)
  ) +
  scale_color_manual(
    values = c(
      "#232d5c", "#52346f", "#833777", "#b13a74", "#d74567", "#f35d51",
               "#ff7f35", "#ffa600"
    ),
    guide = "none"
  ) +
  scale_size_binned(name = "Votes per Episode", range = c(0.3, 3)) +
  labs(
    title = "Alone", subtitle = "IMDB ratings for each episode of the Alone survival TV show series (seasons 1-8),\nshowing the difference between the average rating for each respective season and the episodes therein.\n The larger the dot, the more reviews for that episode.",
    x = NULL, y = "IMDb Rating", caption = caption
  ) +
  guides(
    size = guide_bins(
      show.limits = TRUE, direction = "horizontal", title.position = "top",
      title.hjust = 0.5
    )
  ) +
  theme(legend.position = "none")

p

ggsave("alone.png", width = 9, height = 9)