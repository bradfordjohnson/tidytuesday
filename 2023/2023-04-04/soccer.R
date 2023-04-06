# load packages
pacman::p_load(tidyverse,
               camcorder)

# load data
soccer <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-04-04/soccer21-22.csv') |>
  janitor::clean_names()

# wrangle data
referees <- soccer |>
  group_by(referee) |>
  count() |>
  arrange(desc(n)) |>
  head(9)

ref_fil <- pull(referees, referee) # vector for referee filter

bg = "#EAEEED"

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
  geom_segment(aes(xend = hy_mean, x = ay_mean, yend = referee, y = referee), color = "#654A53") +
  geom_point(aes(x = hy_mean, y = referee), color = "#FFB600") +
  geom_point(aes(x = ay_mean, y = referee), color = "#0189B9") +
  labs(title = "English Premier League Referee 2021-22",
       x = "Average yellow cards given per match") +
  
  theme(
    plot.background = element_rect(fill = bg, color = bg),
    panel.background = element_rect(fill = bg, color = bg),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_text(margin = margin(2,0,0,0,"mm")),
    plot.title = element_text(hjust = .5),
    plot.margin = unit(c(2,20,2,2), "mm")
    )

# save gif **CHANGE INFO**
# gg_playback(
#   name = file.path("2023", "2023-02-28","20230228.gif"),
#   first_image_duration = 4,
#   last_image_duration = 20,
#   frame_duration = .20,
#   background = "#F0F5F5"
# )
