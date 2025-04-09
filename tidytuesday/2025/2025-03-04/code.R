library(tidyverse)
library(camcorder)
library(lubridate)
library(ggtext)
library(sysfonts)
library(showtext)

font_add("Quicksand",
    regular = "tidytuesday/2025/2025-03-04/Quicksand-Regular.ttf",
    bold = "tidytuesday/2025/2025-03-04/Quicksand-Medium.ttf"
)
showtext::showtext_auto()

gg_record(dir = "tidytuesday/2025/2025-03-04/temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)

longbeach <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-03-04/longbeach.csv")

longbeach %>%
    select(animal_id) %>%
    group_by(animal_id) %>%
    mutate(n = n()) %>%
    arrange(desc(n)) %>%
    distinct() %>%
    head(10)

longbeach$days_in_shelter <- as.numeric(longbeach$outcome_date - longbeach$intake_date)

longbeach %>%
    arrange(desc(days_in_shelter)) %>%
    select(animal_type, days_in_shelter, intake_type, outcome_subtype, was_outcome_alive)
head(10)



longbeach_filtered <- longbeach %>%
    filter(was_outcome_alive == TRUE & outcome_type %in% c("adoption") & animal_type %in% c("dog", "cat")) %>%
    mutate(animal_type = paste0(str_to_title(animal_type), "s"))



longest_stay_dog <- longbeach_filtered %>%
    filter(animal_type == "Dogs") %>%
    filter(days_in_shelter == max(days_in_shelter, na.rm = TRUE))

longest_stay_cat <- longbeach_filtered %>%
    filter(animal_type == "Cats") %>%
    filter(days_in_shelter == max(days_in_shelter, na.rm = TRUE))

longbeach_filtered %>%
    mutate(
        outcome_year = year(outcome_date),
        color = case_when(
            animal_id %in% c("A689084", "A629132") ~ "#551A8B",
            TRUE ~ "black"
        ),
        alpha_value = case_when(
            animal_id %in% c("A689084", "A629132") ~ 1, # Alpha for target dogs is 1
            TRUE ~ 0.2
        )
    ) %>%
    ggplot(aes(x = days_in_shelter, y = animal_type, color = color, alpha = alpha_value)) +
    geom_jitter(width = 0.1, size = .9) +
    stat_summary(
        fun = median,
        geom = "text",
        aes(label = round(..x.., 1)),
        vjust = -1,
        size = 15,
        fontface = "bold",
        color = "#FF7F50",
        family = "Quicksand",
        position = position_nudge(y = 0.4)
    ) +
    scale_color_identity() +
    scale_alpha_continuous(range = c(0.2, 1)) +
    # coord_flip() +
    geom_text(
        data = longest_stay_dog,
        aes(
            y = animal_type,
            x = days_in_shelter,
            label = paste0(days_in_shelter, " days\n", animal_name, "\n", primary_color)
        ),
        color = "#551A8B",
        family = "Quicksand",
        size = 14,
        lineheight = .3,
        position = position_nudge(x = -65, y = .4),
        hjust = 0,
        inherit.aes = FALSE
    ) +
    geom_text(
        data = longest_stay_cat,
        aes(
            y = animal_type,
            x = days_in_shelter,
            label = paste0(days_in_shelter, " days\n", animal_name, "\n", primary_color)
        ),
        color = "#551A8B",
        family = "Quicksand",
        size = 14,
        lineheight = .3,
        position = position_nudge(x = -65, y = .4),
        hjust = 0,
        inherit.aes = FALSE
    ) +
    labs(
        title = "The Patience of Hope: Adoption Durations and the Remarkable Stays of Tanya and Macy",
        subtitle = "<span style='color:#FF7F50'>median days in shelter</span><span style='font-size:50px'> - </span><span style='color:#551A8B'>longest stay</span>",
        x = "Days",
        caption = "Source: Long Beach Animal Shelter • Graphic: Ford Johnson"
    ) +
    theme_void() +
    theme(
        text = element_text(family = "Quicksand"),
        plot.margin = margin(10, 10, 10, 10, "mm"),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white", color = "white"),
        plot.subtitle = element_markdown(size = 40, face = "bold"),
        axis.text = element_text(size = 30, face = "bold"),
        axis.title.x = element_markdown(hjust = -.01, size = 30, face = "bold"),
        plot.caption = element_text(family = "Quicksand", size = 30),
        plot.title = element_text(size = 50, face = "bold"),
        legend.position = "none"
    )



ggsave("tidytuesday/2025/2025-03-04/image.png", width = 12, height = 8)

gg_playback(frame_duration = 0.15, image_resize = 1080, name = "tidytuesday/2025/2025-03-04/animated.gif", playback = FALSE)
