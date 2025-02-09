library(tidyverse)

exped_tidy <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-01-21/exped_tidy.csv")

exped_tidy <- exped_tidy %>%
  mutate(year = as.numeric(YEAR))

oxygen_trends <- exped_tidy %>%
  filter(!is.na(O2USED) & HIGHPOINT > 6000) %>%
  group_by(year) %>%
  summarize(
    proportion_oxygen = mean(O2USED, na.rm = TRUE),
    expeditions = n(),
    .groups = "drop"
  )

oxygen_trends %>%
  ggplot(aes(x = year, y = proportion_oxygen)) +
  geom_line(color = "#a88c2d", size = 1) +
  geom_point(color = "#46424b", size = 2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  labs(
    title = "Breathing Thin Air: The Rising Reliance on Supplemental Oxygen in Himalayan Expeditions",
    subtitle = "From 25% in 2020 to 77% in 2024 | A Shift in Oxygen Use for High-Altitude Climbing ( >6000m)",
    x = "",
    y = "",
    caption = "Source: The Himalayan Database • Graphic: Ford Johnson",
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Mulish"),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "#d4cfc5", color = "#d4cfc5"),
    panel.background = element_rect(fill = "#d4cfc5", color = "#d4cfc5"),
    axis.text = element_text(face = "bold", size = 12),
    plot.margin = margin(10, 20, 10, 10),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12)
  )

ggsave("tidytuesday/2025/2025-01-21/himalayan-expeditions.png", width = 10, height = 5)
