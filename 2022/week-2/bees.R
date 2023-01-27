# load packages
library(tidyverse)
library(ggh4x)
library(ggrepel)
library(showtext)
library(htmltools)

# load data
stressor <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/stressor.csv')

# wrangle data
df <- stressor |>
  drop_na() |>
  filter(state == "United States" & stressor != "Other" & stressor != "Unknown") |>
  group_by(year, stressor) |>
  summarise(min_stress = min(stress_pct), max_stress = max(stress_pct))

data_starts <- df %>%
  group_by(stressor) %>%
  top_n(-1, year)

data_ends <- df %>%
  group_by(stressor) %>%
  arrange(desc(min_stress)) %>%
  slice_head(., n = 1)

# load fonts 
font_add(family = "MulishB",
         regular = "C:/Users/Bradf/AppData/Local/Microsoft/Windows/Fonts/Mulish-Bold.ttf")
font_add(family = "Mulish",
         regular = "C:/Users/Bradf/AppData/Local/Microsoft/Windows/Fonts/Mulish-Regular.ttf")
font_add(family = "fb",
         regular = "C:/Users/Bradf/AppData/Local/Microsoft/Windows/Fonts/Font Awesome 6 Brands-Regular-400.otf")
showtext_auto()

# create caption
caption = paste0("<span style='font-family:fb;'><br<&#xf09b;</span>",
                 "<span style='font-family:sans;color:white;'>.</span>",
                 "<span style='font-family:sans;'>bradfordjohnson | TidyTuesday - 2022 Week 2</span>")

# visualize
df |>
  ggplot(aes(x = year, y = min_stress)) +
  geom_line(color = "black", alpha = .5) +
  geom_line(aes(x = year, y = max_stress), color = "black", alpha = .5) +
  stat_difference(aes(ymin = min_stress, ymax = max_stress), fill = "yellow", alpha = 0.55) +
  facet_wrap(~stressor) +
  geom_text_repel(aes(label = "Min"),
                  data = data_starts,
                  color = "black",
                  size = 8,
                  nudge_y = -.5,
                  family = "MulishB") +
  geom_text_repel(aes(label = "Max"),
                  data = data_ends,
                  color = "black",
                  size = 8,
                  nudge_y = 2.5,
                  family = "MulishB", ) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(title = "Bees Can Bee Stressed",
       subtitle = "Percent ranges of beehives stressed by various stressors in the U.S.",
       x = "",
       y = "Percent of Hives Stressed",
       caption = caption) +
  theme(text = element_text(family = "MulishB"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "gray50", linewidth = .05),
        strip.background = element_blank(),
        strip.text = element_text(size = 34),
        plot.caption = ggtext::element_textbox_simple(color="#444444", size = 30),
        plot.title = element_text(hjust = .5, size = 58),
        plot.margin = unit(c(10,20,10,10), "pt"),
        plot.subtitle = element_text(family = "Mulish", hjust = .5, size = 35),
        axis.text = element_text(size = 28),
        axis.title = element_text(size = 35),
        plot.background = element_rect(fill = "gray100", color = "gray100"))


ggsave("bees.png", width = 9, height = 9)