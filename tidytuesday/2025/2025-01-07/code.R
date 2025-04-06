library(tidyverse)
library(janitor)
library(lubridate)

axon_crime_data <- read_csv("2025/2025-01-07/data.csv") %>%
  clean_names() %>%
  mutate(report_date = as.POSIXct(report_date, format = "%m/%d/%Y %I:%M:%S %p")) %>%
  mutate(
    year = year(report_date),
    month = month(report_date, label = TRUE, abbr = TRUE),
    year_month = floor_date(report_date, "month")
  )

crimes <- axon_crime_data %>%
  filter(year < 2025) %>%
  group_by(year_month, nibrs_offense) %>%
  summarise(total_incidents = n_distinct(incident_number), .groups = "drop")

crimes <- crimes %>%
  arrange(year_month) %>%
  group_by(nibrs_offense) %>%
  mutate(
    mom_growth = (total_incidents - lag(total_incidents)) / lag(total_incidents) * 100
  )

crimes <- crimes %>%
  mutate(
    yoy_growth = (total_incidents - lag(total_incidents, 12)) / lag(total_incidents, 12) * 100
  )

target_offenses <- crimes %>%
  group_by(nibrs_offense) %>%
  summarise(total_incidents = sum(total_incidents)) %>%
  arrange(desc(total_incidents)) %>%
  head(16) %>%
  pull(nibrs_offense)

crimes %>%
  filter(nibrs_offense %in% target_offenses) %>%
  ggplot(aes(x = year_month, y = mom_growth)) +
  geom_line(color = "#5e81ac") +
  facet_wrap(~nibrs_offense,  strip.position="bottom") +
  theme_minimal() +
    # scale_x_datetime(
    #   breaks = as.POSIXct(c("2021-01-01", "2022-01-01", "2023-01-01", "2024-01-01")),
    #   labels = c("2021", "2022", "2023", "2024")
    # ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(
    title = "Month-over-Month % Change in Crimes in Atlanta",
    subtitle = "Top 16 Crimes by Volume | 2021-2024",
    caption = "Source: Atlanta Police Department • Graphic: Ford Johnson",
    x = "",
    y = ""
  ) +
  theme(
    text = element_text(family = "Mulish"),
    plot.margin = margin(10, 20, 10, 10),
    plot.background = element_rect(fill = "white", color = "white"),
    panel.background = element_rect(fill = "white", color = "white"),
    strip.text = element_text(size = 8, color = "#2e3440"),
    axis.text.x = element_text(size = 6, angle = 0),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#eceff4"),
    plot.title = element_text(face = "bold"),
    panel.spacing = unit(.25, "cm")
  )

ggsave("2025/2025-01-07/image.png", scale = 3.2)
