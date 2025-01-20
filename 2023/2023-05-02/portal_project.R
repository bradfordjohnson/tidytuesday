# ----------------- load packages ----------------------------------
pacman::p_load(tidyverse,
               showtext,
               htmltools)

showtext_auto()
showtext_opts(dpi = 300)

# ----------------- load data --------------------------------------
species <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-02/species.csv')
surveys <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-02/surveys.csv')

# ----------------- wrangle data -----------------------------------
rodents <- surveys |>
  left_join(species, by = c("species" = "species")) |>
  mutate(
    common_name = case_when(
      species == "DS" ~ "Banner-tailed kangaroo rat",
      .default = "Other rodents"
      )
    )

# ----------------- labels and text --------------------------------
font_add(family = "fb", regular = "C:/Users/Bradf/AppData/Local/Microsoft/Windows/Fonts/Font Awesome 6 Brands-Regular-400.otf")

font_add_google(name = "Inter", family = "Inter")
font_1 <- "Inter"

caption <- paste0("<span style='font-family:fb;'>&#xf09b;</span>",
                  "<span style='font-family:sans;color:#FEF9F4FF;'>.</span>",
                  "<span style='font-family:Inter;'>bradfordjohnson | TidyTuesday - 2023 Week 18</span>")

subtitle <- paste0("<span style='color:#953536;'>Kangaroo rats</span>",
                   " had an average hindfoot length of ",
                   "<span style='color:#953536;'>49.9 mm</span>",
                   " and an average weight of ",
                   "<span style='color:#953536;'>119.9 grams</span>")

# ----------------- visualize data ---------------------------------
color_pal <- c("#953536", "#d3d3d3")

base_plot <- rodents |>
  ggplot(aes(x = wgt, y = hfl)) +
  geom_jitter(alpha = .7, aes(color = common_name)) +
  scale_color_manual(values = color_pal) +
  theme_minimal() +
  labs(title = "Kangaroo rats in the Portal Project",
       subtitle = subtitle,
       x = "Weight (g)",
       y = "Hindfoot Length (mm)",
       caption = caption)

# ----------------- customize theme --------------------------------

bg <- "#FBFBFB"

base_plot +
  theme(
    plot.background = element_rect(fill = bg, color = bg),
    panel.background = element_rect(fill = bg, color = bg),
    panel.grid.minor = element_blank(),
    plot.title = element_text(family = font_1, size = 16, margin = margin(0,2,0,2, "mm")),
    plot.subtitle = ggtext::element_textbox_simple(family = font_1, margin = margin(2, 0, 2, 0, "mm"),
                                                   size = 10),
    axis.title = element_text(family = font_1, size = 8),
    axis.title.x = element_text(margin = margin(1,0,0,0, "mm")),
    axis.title.y = element_text(margin = margin(1,1,1,1, "mm")),
    axis.text = element_text(family = font_1, size = 8),
    legend.title = element_blank(),
    legend.text = element_text(family = font_1, size = 8),
    legend.position = "top",
    plot.margin = unit(c(2,2,2,2), "mm"),
    plot.caption = ggtext::element_textbox_simple(size = 6, halign = 1)
    )

ggsave("portal_project.png", height = 6, width = 6)
