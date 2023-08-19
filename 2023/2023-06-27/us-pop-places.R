pacman::p_load(tidyverse,
               showtext,
               htmltools,
               sf)

showtext_auto()
showtext_opts(dpi = 300)

font_add(
  family = "fb", regular = "C:/Users/Bradf/AppData/Local/Microsoft/Windows/Fonts/Font Awesome 6 Brands-Regular-400.otf"
)

font_add_google(name = "Roboto Slab", family = "Roboto Slab")
font_1 <- "Roboto Slab"

font_add_google(name = "Roboto", family = "Roboto")
font_2 <- "Roboto"


us_place_names <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-27/us_place_names.csv')
us_place_history <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-27/us_place_history.csv')

ghost_towns <- us_place_history |>
  filter(description == "A ghost town.")

ghost_towns_joined <- us_place_names |>
  inner_join(ghost_towns, by = c("feature_id"))

texas <- st_read("Texas_State_Boundary/State.shp")
interstate <- st_read("Tx_Interstates_General_NE/Tx_Interstates_General_NE.shp")

title <- paste0(
  "<span><span style='font-family:Roboto;color:#B15152;'>40 </span><span style='font-family:Roboto;'>of Texas's 500+ Ghost Towns</span></span>"
)

caption <- paste0(
  "<span style='font-family:fb;'>&#xf09b;</span>",
  "<span style='font-family:sans;color:#F0EDE8;'>.</span>",
  "<span style='font-family:Roboto;'>bradfordjohnson | TidyTuesday - 2023 Week 26</span>"
)

plot_bg <- "#F5F3EF"
text_col <- "#545963"
state_col <- "#B7AFB5"
point_col <- "#B15152"

ggplot() +
  geom_sf(data = texas, fill = state_col) +
  geom_sf(data = interstate) +
  theme_void() +
  labs(title = title, caption = caption) +
  geom_jitter(ghost_towns_joined, mapping = aes(x = prim_long_dec, y = prim_lat_dec), shape = 20, color = point_col, size = 2) +
  theme(
    plot.background = element_rect(fill = plot_bg, color = plot_bg),
    panel.background = element_rect(fill = plot_bg, color = plot_bg),
    plot.margin = margin(10, 1, 10, 1),
    plot.title = ggtext::element_textbox_simple(
      family = font_1, 
      margin = margin(5, 0, 5, 0),
      halign = .5,
      size = 18,
      color = text_col),
    plot.caption = ggtext::element_textbox_simple(
      margin = margin(6, 0, 0, 0),
      halign = .5, color = text_col, size = 7
    ),
    )

ggsave("us-pop-places.png", height = 6, width = 6)   
