library(tidyverse)
library(treemapify)
library(scales)
library(showtext)

font_add_google("Roboto", "roboto")
showtext_auto()

agencies <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-02-18/agencies.csv")

filtered_agencies <- agencies %>%
  filter(!is.na(agency_type) & !is.na(is_nibrs))

filtered_agencies %>%
  mutate(nibrs_label = case_when(
    is_nibrs == TRUE ~ "NIBRS Participants",
    is_nibrs == FALSE ~ " "
  )) %>%
  count(agency_type, nibrs_label) %>%
  ggplot(aes(area = n, fill = agency_type, subgroup = nibrs_label, label = agency_type)) +
  geom_treemap() +
  geom_treemap_subgroup_border(color = "white", size = 12) +
  geom_treemap_text(aes(label = paste0(agency_type, "\n", comma(n))),
    colour = "white", place = "center", reflow = TRUE,
    padding.x = unit(2, "mm"), padding.y = unit(2, "mm"),
    family = "roboto", fontface = "bold"
  ) +
  scale_fill_viridis_d(option = "cividis") +
  labs(
    title = "NIBRS Participation Across Different Agency Types",
    subtitle = "Participating Agencies (left) vs Non-Participating Agencies (right)",
    caption = "Source: FBI Crime Data API • Graphic: Ford Johnson"
  ) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(
    text = element_text(family = "roboto"),
    legend.position = "none",
    plot.title = element_text(size = 16, hjust = .01),
    plot.subtitle = element_text(size = 12, hjust = .01),
    plot.margin = margin(10, 10, 10, 10, "mm"),
    plot.background = element_rect(fill = "white", color = "white"),
    panel.background = element_rect(fill = "white", color = "white"),
    plot.caption = element_text(hjust = .01)
  )

ggsave("tidytuesday/2025/2025-02-18/image.png", scale = 2.5)
