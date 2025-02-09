library(tidyverse)

simpsons_characters <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-02-04/simpsons_characters.csv")
simpsons_episodes <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-02-04/simpsons_episodes.csv")
# simpsons_locations <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-02-04/simpsons_locations.csv')
simpsons_script_lines <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-02-04/simpsons_script_lines.csv")


top_characters_by_word_count <- simpsons_script_lines %>%
  filter(speaking_line == TRUE) %>%
  group_by(character_id) %>%
  summarise(words = sum(word_count)) %>%
  arrange(desc(words)) %>%
  head(12) %>%
  pull(character_id)

character_words_spoken_by_episode <- simpsons_script_lines %>%
  filter(speaking_line == TRUE & character_id %in% top_characters_by_word_count) %>%
  group_by(episode_id, character_id) %>%
  summarise(words = sum(word_count))

character_words_spoken_by_episode %>%
  left_join(simpsons_characters, by = c("character_id" = "id")) %>%
  mutate(normalized_name = str_to_title(normalized_name)) %>%
  ggplot(aes(x = episode_id, y = words)) +
  geom_line(color = "#4285F4") +
  facet_wrap(~normalized_name) +
  labs(
    title = "How Much Do Simpsons Characters Talk?",
    subtitle = "Total Words Spoken by Episode from the 12 Most Outspoken Characters (2010–2016)",
    caption = "Source: Kaggle • Graphic: Ford Johnson",
    x = "Episode Number",
    y = ""
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Mulish"),
    plot.margin = margin(10, 20, 10, 10),
    plot.title = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 10, color = "#4C4C4C"),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "white", color = "white"),
    panel.background = element_rect(fill = "white", color = "white"),
    panel.spacing = unit(1, "cm"),
    axis.title.x = element_text(hjust = -.01, vjust = -.5),
    strip.text = element_text(size = 10, face = "bold")
  )

ggsave("tidytuesday/2025/2025-02-04/simpsons.png", scale = 2.5)
