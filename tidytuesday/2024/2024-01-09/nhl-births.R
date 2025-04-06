library(tidyverse)
library(scales)

nhl_rosters <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-01-09/nhl_rosters.csv")

create_year_ranges <- function(start_year, end_year) {
  year_ranges <- c()
  for (year in start_year:(end_year - 1)) {
    year_range <- paste0(year, year + 1)
    year_ranges <- c(year_ranges, year_range)
  }
  return(year_ranges)
}

start_year <- 2010
end_year <- 2024
year_ranges <- create_year_ranges(start_year, end_year)

total_players_by_dimensions <- nhl_rosters %>%
  filter(season %in% year_ranges) %>%
  mutate(
    season = as.character(season),
    birth_date_quarter = factor(quarter(birth_date))
  ) %>%
  group_by(season, birth_date_quarter) %>%
  summarise(distinct_player_count = n_distinct(player_id), .groups = "drop") %>%
  arrange(desc(distinct_player_count))

total_players_by_target_season <- nhl_rosters %>%
  filter(season %in% year_ranges) %>%
  mutate(season = as.character(season)) %>%
  group_by(season) %>%
  summarise(total_player_count = n_distinct(player_id), .groups = "drop")

average_pct_by_birth_quarter <- total_players_by_dimensions %>%
  left_join(total_players_by_target_season, by = "season") %>%
  group_by(birth_date_quarter, season) %>%
  summarise(percent_of_total = sum(distinct_player_count) / sum(total_player_count) * 100, .groups = 'drop') %>%
  group_by(birth_date_quarter) %>%
  summarise(avg_percent_of_total = mean(percent_of_total, na.rm = TRUE), .groups = 'drop')

plot <- total_players_by_dimensions %>%
  left_join(total_players_by_target_season, by = "season") %>%
  mutate(
    percent_of_total = distinct_player_count / total_player_count * 100,
    formatted_season = paste0(substr(season, 1, 4), "-", substr(season, 7, 8))
  ) %>%
  ggplot(
    aes(
      x = formatted_season,
      y = percent_of_total,
      color = birth_date_quarter,
      group = birth_date_quarter
    )
  ) +
  scale_y_continuous(limits = c(0, 50), labels = percent_format(scale = 1)) +
  scale_color_manual(values = c("#6929c4", "#32647a", "#449abe", "#b3cad6")) +
  geom_line() +
  geom_point() +
  labs(
    title = "Quarterly Edge: Birth Trends Among NHL Players",
    subtitle = str_wrap("Players born in the first quarter account for an average of 29.6% of NHL participants, with the second quarter close behind at 27.2%, while representation decreases for those born in subsequent quarters.", 120),
    caption = "Source: NHL API • Graphic: Ford Johnson",
    x = "NHL Season",
    y = "% of Players",
    color = "Birth Quarter"
  ) +
  theme_minimal(base_family = "Roboto", base_size = 11) +
  theme(
    plot.background = element_rect(fill = "white", color = "white"),
    panel.background = element_rect(fill = "white", color = "white"),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(10, 20, 10, 30),
    axis.title.x = element_text(margin = margin(8, 0, 0, 0)),
    axis.title.y = element_text(margin = margin(0, 8, 0, 0)),
    plot.title = element_text(face = "bold"),
    plot
  )

ggsave("2024/2024-01-09/image.png", plot = plot)
