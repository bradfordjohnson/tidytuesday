library(tidyverse)
library(camcorder)
library(maps)
library(sf)
library(showtext)

font_add_google("Roboto", "roboto")
showtext_auto()

gg_record(dir = "tidytuesday/2025/2025-04-08/temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)

care_state <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-04-08/care_state.csv")

state_lookup <- tibble(
    state_abb = state.abb,
    state_name = tolower(state.name)
)

us_states <- map_data("state")

care_state_filtered <- care_state %>%
    filter(measure_id == "OP_18b" & !is.na(score))

care_state_clean <- care_state_filtered %>%
    distinct(state, .keep_all = TRUE)

care_state_clean <- care_state_clean %>%
    left_join(state_lookup, by = c("state" = "state_abb"))

plot_data <- us_states %>%
    left_join(care_state_clean, by = c("region" = "state_name"))

us_states_unnested <- us_states %>%
    unnest(cols = c(long, lat))

us_states_sf <- us_states_unnested %>%
    st_as_sf(coords = c("long", "lat"), crs = 4326)

ggplot(plot_data, aes(x = long, y = lat, group = group, fill = score)) +
    geom_polygon(color = "white") +
    scale_fill_viridis_c(option = "plasma", na.value = "lightgray", name = "Minutes") + # Set color scale
    coord_fixed(1.3) +
    labs(
        title = "Median Time Patients Spent in the Emergency Department Before Leaving",
        caption = "Source: cms.gov • Graphic: Ford Johnson"
    ) +
    theme_void() +
    theme(
        text = element_text(family = "roboto"),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.background = element_rect(fill = "white", color = "white"),
        plot.title = element_text(size = 40, hjust = .01, family = "roboto"),
        plot.caption = element_text(size = 25, hjust = .01, family = "roboto"),
        plot.margin = margin(0, 10, 0, 10, "mm"),
        legend.title = element_text(size = 30, family = "roboto"),
        legend.text = element_text(size = 30, family = "roboto")
    )

ggsave("tidytuesday/2025/2025-04-08/image.png", width = 7, height = 6)

gg_playback(frame_duration = 0.15, image_resize = 1080, name = "tidytuesday/2025/2025-04-08/animated.gif", playback = FALSE)
