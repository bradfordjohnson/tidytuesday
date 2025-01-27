#---- load packages ----
pacman::p_load(
        tidyverse,
        sf,
        rnaturalearth,
        showtext,
        htmltools
)

showtext_auto()

#---- load data ----
timezones <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-03-28/timezones.csv")
world <- ne_countries(scale = "small", returnclass = "sf")

#---- wrangle data ----
timezones <- timezones |>
        mutate(lon = longitude, lat = latitude)

timezones_sf <- sf::st_as_sf(timezones,
        coords = c("lon", "lat"),
        crs = "WGS84"
)

#---- create colors ----
bg <- "#77A2BBFF"
text_col <- "black"

#---- load fonts ----

font_add(
        family = "fb",
        regular = "C:/Users/Bradf/AppData/Local/Microsoft/Windows/Fonts/Font Awesome 6 Brands-Regular-400.otf"
)

font_add_google(name = "Dosis", family = "Dosis")

font_add_google(name = "Cabin", family = "Cabin")
font_1 <- "Cabin"

#---- load labs ----
caption <- paste0(
        "<span style='font-family:fb;'>&#xf09b;</span>",
        "<span style='font-family:sans;color:#77A2BBFF;'>.</span>",
        "<span style='font-family:Dosis;'>bradfordjohnson</span>",
        "<span style='font-family:sans;color:#77A2BBFF;'>-</span>",
        "<span style='font-family:Dosis;'>| TidyTuesday - 2023 Week 13</span>"
)

subtitle <- paste0(
        "<span>There are over </span>",
        "<span style='color:#B71E42FF;'>**330 time zones**</span>",
        "<span> agreed upon since </span>",
        "<span style='color:#B71E42FF;'>**1970**</span>"
)

#---- visualize data ----
ggplot() +
        geom_sf(
                data = world,
                mapping = aes(geometry = geometry),
                fill = "#B2AD8FFF"
        ) +
        geom_sf(
                data = timezones_sf,
                aes(geometry = geometry),
                size = 1, color = "#B71E42FF", alpha = .5
        ) +
        coord_sf(crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs ") +
        theme_void() +
        labs(
                title = "When working with time zones, it takes time...",
                subtitle = subtitle,
                caption = caption
        ) +
        theme(
                legend.position = "none",
                plot.background = element_rect(colour = bg, fill = bg),
                panel.background = element_rect(color = bg, fill = bg),
                plot.title = element_text(color = text_col, family = font_1, hjust = 0.5, size = 56),
                plot.subtitle = ggtext::element_textbox_simple(family = font_1, halign = 0.5, margin = margin(2, 0, 2, 0, "mm"), size = 50),
                plot.caption = ggtext::element_textbox_simple(margin = margin(1, 0, 1, 0, "mm"), size = 30),
                plot.margin = unit(c(2, 1, 1, 1), "mm")
        )

ggsave("timezones.png", height = 9, width = 9)
