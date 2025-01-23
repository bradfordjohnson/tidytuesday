# install.packages(c("tidyverse", "tidytext", "ggplot2"))
library(tidyverse)
library(tidytext)
library(ggplot2)

conf2023 <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-01-14/conf2023.csv")
conf2024 <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-01-14/conf2024.csv")

tidy_words_2023 <- conf2023 %>%
  unnest_tokens(word, session_title) %>%
  anti_join(stop_words, by = "word") %>%
  count(word, sort = TRUE) %>%
  rename(freq_2023 = n)

tidy_words_2024 <- conf2024 %>%
  unnest_tokens(word, talk_title) %>%
  anti_join(stop_words, by = "word") %>%
  count(word, sort = TRUE) %>%
  rename(freq_2024 = n)

word_comparison <- tidy_words_2023 %>%
  inner_join(tidy_words_2024, by = "word") %>%
  # filter(freq_2023 >= 5 | freq_2024 >= 5) %>%
  mutate(delta = abs(freq_2023 - freq_2024)) %>%
  arrange(desc(delta)) %>%
  head(20)

word_comparison_long <- word_comparison %>%
  gather(key = "year", value = "frequency", freq_2023, freq_2024) %>%
  mutate(year = recode(year, "freq_2023" = "2023", "freq_2024" = "2024"))

ggplot(word_comparison_long, aes(x = word, y = frequency, group = word, color = year)) +
  geom_line(color = "black") +
  geom_point() +
  coord_flip() +
  scale_color_manual(values = c("2023" = "orange", "2024" = "blue")) +
  theme_minimal(base_family = "Verdana", base_size = 11) +
  labs(
    title = "Top Words by Frequency Shifts in \n2023 vs 2024 posit::conf Titles",
    y = "Frequency",
    caption = "Source: `posit::conf` talk's • Graphic: Ford Johnson"
  ) +
  theme(
    legend.title = element_blank(),
    axis.title = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    legend.justification = "left",
    legend.margin = margin(0, 0, 0, 0, "mm"),
    plot.margin = margin(20, 10, 10, 10),
    plot.background = element_rect(fill = "white", color = "white"),
    panel.background = element_rect(fill = "white", color = "white")
  )

ggsave("test.png", scale = 1.5)
