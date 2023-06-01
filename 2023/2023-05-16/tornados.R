pacman::p_load(tidyverse,
               ggmap)

tornados <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-16/tornados.csv') |>
  drop_na()

tor_mag_5 <- tornados |>
  filter(mag == 5) |>
  mutate(post = case_when(
    yr >= 2000 ~ "Post-2000",
    yr < 2000 ~ "Pre-2000"
  ))

bbox <- c(bottom = 25.75, top = 49 , right = -67, left = -125)

usmap <- get_stamenmap(bbox = bbox, zoom = 6, maptype = 'toner-lite') 

ggmap(usmap) +
  geom_segment(
    data = tornados,
    aes(
      x = slon,
      y = slat,
      xend = elon,
      yend = elat,
      # alpha = mag,
      colour = factor(mag)
      )
    ) + 
  theme_void() +
  scale_colour_manual(
    values = c(
     "#FFF5DC",
     "#F6E3BB",
     "#E8C584",
     "#DDAD58",
     "#90651C",
     "#382301"
      )
    ) +
  labs(colour = "Magnitude (Fujita Scale)")
