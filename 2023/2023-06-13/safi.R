pacman::p_load(
  tidyverse,
  showtext,
  htmltools
)

showtext_auto()
showtext_opts(dpi = 300)

font_add(
  family = "fb", regular = "C:/Users/Bradf/AppData/Local/Microsoft/Windows/Fonts/Font Awesome 6 Brands-Regular-400.otf"
)

font_add_google(name = "Roboto Slab", family = "Roboto Slab")
font_1 <- "Roboto Slab"

font_add_google(name = "Roboto", family = "Roboto")
font_2 <- "Roboto"

caption <- paste0(
  "<span style='font-family:fb;'>&#xf09b;</span>",
  "<span style='font-family:sans;color:#e8eae7;'>.</span>",
  "<span style='font-family:Roboto;'>bradfordjohnson | TidyTuesday - 2023 Week 24</span>"
)

v_colors <- c("#8fb1bc", "#6e677d", "#2f4554")

plot_bg <- "#E8EAE7"

safi_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-13/safi_data.csv') |>
  janitor::clean_names()

result_df <- safi_data %>%
  filter(items_owned != "NULL") %>%
  separate_rows(items_owned, sep = ";") %>%
  mutate(item_present = 1) %>%
  pivot_wider(names_from = items_owned, values_from = item_present, values_fill = 0, names_prefix = "has_")

perc_household <- result_df %>%
  select(1:2, starts_with("has_")) |>
  group_by(village) |>
  summarise(n=n(), across(c(3:19), ~ sum(.x, na.rm = TRUE)/n))

perc_household <- perc_household |>
  pivot_longer(
    cols = starts_with("has_"),
    names_to = "item",
    values_to = "has_item"
  ) %>%
  mutate(item = sub("^has_", "", item)) %>%
  select(-n)

perc_household |>
  mutate(item = sub("_", " ", item)) %>%
  filter(item %in% c("mobile phone", "fridge", "television", "radio", "solar torch")) %>%
  ggplot(aes(x = reorder(item, has_item), y = has_item, fill = village, color = village)) +
  geom_col(position = "dodge2") +
  geom_text(aes(label = paste0(round(has_item * 100, 1), "%")), position = position_dodge(width = 0.9), vjust = -0.5) +
  scale_fill_manual(values = v_colors) +
  scale_color_manual(values = v_colors) +
  labs(title = "Percentage of Households Possessing Item in SAFI Survey",
       subtitle = "Comparative Analysis Across Villages",
       caption = caption) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = plot_bg, color = plot_bg),
    panel.background = element_rect(fill = plot_bg, color = plot_bg),
    plot.margin = margin(10, 5, 5, 5),
    plot.title = element_text(
      margin = margin(2, 0, 3, 0, "mm"),
      size = 15,
      family = font_1,
      face = "bold",
      vjust = 0,
      color = "black"
    ),
    plot.caption = ggtext::element_textbox_simple(
      margin = margin(10, 0, 0, 0),
      halign = .01, color = "gray10", size = 5
    ),
    legend.pos = c(0.05, .89),
    legend.text = element_text(size = 8, family = font_1),
    legend.box = "vertical",
    legend.title = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    axis.title = element_blank()
  )

ggsave("safi.png")  
