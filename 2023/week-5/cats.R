# load packages
library(tidyverse)
library(showtext)
library(htmltools)

showtext_auto()

# load data
cats_uk <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-31/cats_uk.csv')
cats_uk_reference <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-31/cats_uk_reference.csv')

# clean and wrangle data
cats_uk <- cats_uk |>
  filter(algorithm_marked_outlier == FALSE & manually_marked_outlier == FALSE) |>
  select(-c(study_name, algorithm_marked_outlier, manually_marked_outlier))

cats <- cats_uk |>
  group_by(tag_id) |>
  summarise(avg_speed = round(mean(ground_speed),2),
            min_speed = min(ground_speed),
            max_speed = max(ground_speed))

cats_reference <- cats_uk_reference |>
  select(tag_id, animal_id, hunt, prey_p_month, animal_sex, hrs_indoors, n_cats, age_years)

cats_joined <- cats |>
  left_join(cats_reference, on = c("tag_id" = "tag_id"))

cats_joined <- cats_joined |>
  mutate(hrs_outdoors = 24 - hrs_indoors)

cats_joined <- cats_joined |>
  drop_na(age_years)

cats_joined$age_years <- as.character(cats_joined$age_years)
cats_joined$age_years <- factor(cats_joined$age_years, levels = c("0", "1", "2", "3", "4", "5", "6",
                                                        "7", "8", "9", "10", "11", "12", "13"))
# load fonts
font_add_google("Commissioner")
font_add_google(name = "Ubuntu", family = "Ubuntu")
font <- "Commissioner"

font_add(family = "fb",
         regular = "C:/Users/Bradf/AppData/Local/Microsoft/Windows/Fonts/Font Awesome 6 Brands-Regular-400.otf")

# load caption
caption = paste0("<span style='font-family:fb;'><br>&#xf09b;</span>",
                 "<span style='font-family:sans;color:white;'>.</span>",
                 "<span style='font-family:sans;'>bradfordjohnson | TidyTuesday - 2023 Week 5</span>")

# visual
cats_joined |>
  filter(hunt == TRUE) |>
  ggplot(aes(x = age_years, y = avg_speed, size = prey_p_month)) +
  geom_point(alpha = .5) +
  guides(size = guide_legend(reverse = TRUE)) +
  labs(
    title = "Cats - The Natural Hunters",
    subtitle = "Fast or slow, young or old; cats remain successful hunters.",
    x = "Age (years)",
    y = "Average Speed (m/s)",
    size = "Monthly Prey",
    caption = caption
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(family = "Ubuntu", hjust = .5, vjust = 1, size = 20, margin = unit(c(0, 0, 0, 6), "mm")),
    plot.subtitle = element_text(family = font, hjust = .5),
    plot.caption = ggtext::element_textbox_simple(color="black", size = 12),
    plot.margin = unit(c(10,10,10,10), "pt"),
    axis.title.x = element_text(family = font, size = 12, margin = unit(c(3, 0, 0, 0), "mm")),
    axis.title.y = element_text(family = font, size = 12, margin = unit(c(0, 3, 0, 0), "mm")),
    axis.text.x = element_text(family = font, size = 12),
    axis.text.y = element_text(family = font, size = 12),
    legend.title.align = 0.5,
    legend.text.align = 0.5,
    legend.title = element_text(family = font, size = 12),
    legend.text = element_text(family = font, size = 12)
  )