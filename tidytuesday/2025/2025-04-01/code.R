library(tidyverse)
library(camcorder)
library(treemapify)
library(showtext)

font_add_google("Nunito", "nunito")
showtext_auto()

gg_record(dir = "tidytuesday/2025/2025-04-01/temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)

pokemon_df <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-04-01/pokemon_df.csv")

pokemon_df %>%
  pivot_longer(cols = c(color_1, color_2, color_f), 
               names_to = "color_type", 
               values_to = "color") %>%
  filter(!is.na(color)) %>%
  mutate(hierarchy = case_when(
    color_type == "color_1" ~ "color_1",
    color_type == "color_2" ~ "color_1 > color_2",
    color_type == "color_f" ~ "color_1 > color_f",
    TRUE ~ "unknown"
  )) %>%
  group_by(hierarchy, color, type_1) %>%
  summarize(count = n(), .groups = "drop") %>%
  group_by(type_1) %>%
  mutate(max_count = max(count)) %>%
  mutate(label_color = if_else(count == max_count, "white", color)) %>%
  ggplot(aes(area = count, fill = color, label = type_1, subgroup = type_1)) +
  geom_treemap() +
  geom_treemap_subgroup_border(colour = "white", size = 12) +
  geom_treemap_text(aes(color = if_else(count == max_count, "white", color), alpha = if_else(count == max_count, 100, 0.0)), place = "bottom", grow = TRUE, family = "nunito") +  # Set label color
  scale_fill_identity() +
  scale_colour_identity() +
  labs(
    title = "Pokemon color frequency by primary type",
    caption = "Source: pokemon::pokemon • Graphic: Ford Johnson"
  ) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(
    legend.position = "none",
    text = element_text(family = "nunito"),
    plot.background = element_rect(fill = "white", color = "white"),
    panel.background = element_rect(fill = "white", color = "white"),
    plot.title = element_text(size = 70, hjust = .01, margin = margin(0, 0, 5, 0,"mm")),
    plot.caption = element_text(size = 25, hjust = .01),
    plot.margin = margin(10, 10, 10, 10, "mm"),
  )


ggsave("tidytuesday/2025/2025-04-01/image.png")

gg_playback(frame_duration = 0.15, image_resize = 1080, name = "tidytuesday/2025/2025-04-01/animated.gif", playback = FALSE)
