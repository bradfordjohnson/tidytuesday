# second version; including mapping resources shared by @BjnNowak
# https://wilkelab.org/practicalgg/articles/Winkel_tripel.html

#---- load packages ----
pacman::p_load(tidyverse,
               sf,
               rworldmap,
               lwgeom,
               cowplot,
               showtext,
               htmltools)

showtext_auto()

#---- load data ----
timezones <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-03-28/timezones.csv')

world_sf <- st_as_sf(getMap(resolution = "low"))

#---- wrangle data ----
crs_wintri <- "+proj=wintri +datum=WGS84 +no_defs +over"

timezones <- timezones |>
  mutate(lon = longitude, lat = latitude)

timezones_sf <- sf::st_as_sf(timezones,
                             coords = c("lon", "lat")) %>%
  st_transform_proj(crs = crs_wintri)


world_wintri <- st_transform_proj(world_sf, crs = crs_wintri)

grat_wintri <- 
  st_graticule(lat = c(-89.9, seq(-80, 80, 20), 89.9)) %>%
  st_transform_proj(crs = crs_wintri)

lats <- c(90:-90, -90:90, 90)
longs <- c(rep(c(180, -180), each = 181), 180)

# turn into correctly projected sf collection
wintri_outline <- 
  list(cbind(longs, lats)) %>%
  st_polygon() %>%
  st_sfc( # create sf geometry list column
    crs = "WGS84"
  ) %>% 
  st_sf() %>%
  st_transform_proj(crs = crs_wintri) # transform to Winkel tripel

#---- create colors ----
bg <- "#FCFCFCFF"
text_col <- "black"

#---- load fonts ----

font_add(family = "fb",
         regular = "C:/Users/Bradf/AppData/Local/Microsoft/Windows/Fonts/Font Awesome 6 Brands-Regular-400.otf")

font_add_google(name = "Dosis", family = "Dosis")

font_add_google(name = "Cabin", family = "Cabin")
font_1 <- "Cabin"

#---- load labs ----
caption = paste0("<span style='font-family:fb;'>&#xf09b;</span>",
                 "<span style='font-family:sans;color:#FCFCFCFF;'>.</span>",
                 "<span style='font-family:Dosis;'>bradfordjohnson</span>",
                 "<span style='font-family:sans;color:#FCFCFCFF;'>-</span>",
                 "<span style='font-family:Dosis;'>| TidyTuesday - 2023 Week 13</span>")

subtitle = paste0("<span>There are over </span>",
                  "<span style='color:#B71E42FF;'>**330 time zones**</span>",
                  "<span> agreed upon since </span>",
                  "<span style='color:#B71E42FF;'>**1970**</span>")

#---- visualize data ----
ggplot() + 
  geom_sf(data = wintri_outline, fill = "#56B4E950", color = NA) +
  geom_sf(data = grat_wintri, color = "gray30", size = 0.25/.pt, alpha = .5) + 
  geom_sf(data = world_wintri, color = "black", fill = "#B2AD8FFF", size = 0.5/.pt) +
  geom_sf(data = wintri_outline, fill = NA, color = "grey30", size = 0.5/.pt) +
  geom_sf(data = timezones_sf,
          aes(geometry = geometry),
          size = 1, color = "#B71E42FF", alpha = .7
  ) +
  coord_sf(datum = NULL) +
  theme_map() +
  labs(title = "What time is it?",
       subtitle = subtitle,
       caption = caption) +
  theme(legend.position = "none",
        plot.background = element_rect(colour = bg, fill = bg),
        panel.background = element_rect(color = bg, fill = bg),
        plot.title = element_text(color = text_col, family = font_1, hjust = 0.5, size = 56),
        plot.subtitle = ggtext::element_textbox_simple(family = font_1, halign = 0.5, margin = margin(2,0,2,0,"mm"), size = 50),
        plot.caption = ggtext::element_textbox_simple(margin = margin(1,0,0,0,"mm"), size = 30),
        plot.margin = unit(c(2,2,2,2),"mm"))

ggsave("timezones-v2.png", height = 6, width = 9)
