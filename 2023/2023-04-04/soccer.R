# load packages
pacman::p_load(tidyverse,
               camcorder,
               showtext,
               htmltools)

showtext_auto()

# load data
soccer <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-04-04/soccer21-22.csv') |>
  janitor::clean_names()

# wrangle data | not used anymore but I liked this code and kept it |
# referees <- soccer |>
#   group_by(referee) |>
#   count() |>
#   arrange(desc(n)) |>
#   head(9)
# 
# ref_fil <- pull(referees, referee) # vector for referee filter

# load colors
bg <- "#EAEEED"
home <- "#FFBD19"
away <- "#195BFF"
line <- "#654A53"

# load fonts
font_add(
  family = "fb", regular = "C:/Users/Bradf/AppData/Local/Microsoft/Windows/Fonts/Font Awesome 6 Brands-Regular-400.otf"
)

font_add_google(name = "Dosis", family = "Dosis")

font_add_google(name = "Rubik", family = "Rubik")
font_1 <- "Rubik"

# load labs
title <- "English Premier League Referees 2021-22"

# x <- "Average yellow cards / match"

subtitle <- "Average number of yellow cards given to the <span style='color:#FFBD19'><b>home</b></span> and <span style='color:#195BFF'><b>away</b></span> teams per match"

caption <- paste0("<span style='font-family:fb;'>&#xf09b;</span>",
                 "<span style='font-family:sans;color:#EAEEED;'>.</span>",
                 "<span style='font-family:Dosis;'>bradfordjohnson | TidyTuesday - 2023 Week 14</span>")

# start recording
gg_record(
  dir = file.path("2023", "2023-04-04", "recording"), # where to save the recording
  device = "png", # device to use to save images
  width = 6, # width of saved image
  height = 6, # height of saved image
  units = "in", # units for width and height
  dpi = 300 # dpi to use when saving image
)

soccer |>
  select(referee, hf, af, hy, ay, hr, ar) |>
  group_by(referee) |>
  summarise(across(c(hf, af, hy, ay, hr, ar), list(mean = mean, sum = sum, median = median))) |>
  filter(ay_median != hy_median) |>
  ggplot(aes(y = referee)) +
  geom_segment(aes(xend = hy_mean, x = ay_mean, yend = referee, y = referee), color = line) +
  geom_point(aes(x = hy_mean, y = referee), color = home) +
  geom_point(aes(x = ay_mean, y = referee), color = away) +
  labs(title = title,
       subtitle = subtitle,
       caption = caption) +
  geom_text(aes(x = (hy_mean + ay_mean)/2, y = referee, label = referee), vjust = 1.5, family = font_1, size = 10) +
  scale_x_continuous(limits = c(1.0,3.0)) +
  theme(
    plot.background = element_rect(fill = bg, color = bg),
    panel.background = element_rect(fill = bg, color = bg),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(family = font_1, size = 25),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    # axis.title.x = element_text(margin = margin(2,0,0,0,"mm"), family = font_1, size = 35),
    plot.title = element_text(family = font_1, size = 45),
    plot.margin = unit(c(2,2,2,2), "mm"),
    plot.caption = ggtext::element_textbox_simple(size = 20, margin = margin(3,0,0,1,"mm"), halign = 1),
    plot.subtitle = ggtext::element_textbox_simple(size = 32, family = font_1)
    )

# save gif
gg_playback(
  name = file.path("2023", "2023-04-04","20230404.gif"),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .18,
  background = "#EAEEED"
)

# save image
ggsave("soccer.png", width = 6, height = 6)
