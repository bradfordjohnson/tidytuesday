# load packages
library(tidyverse)
library(camcorder)
library(ggpath)
library(showtext)
library(htmltools)

showtext_auto()

# load data
numbats <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-03-07/numbats.csv") |>
  janitor::clean_names()

local_image_path <- "numbat.png"

numbats <- numbats |>
  select(-c(scientific_name, taxon_concept_id, data_resource_name, prcp, tmax, tmin))

# wrangle by month
num_month <- numbats |>
  drop_na(month) |>
  group_by(month) |>
  summarise(n = n()) |>
  mutate(path = local_image_path)

# start recording
gg_record(
  dir = file.path("2023", "2023-03-07", "recording"), # where to save the recording
  device = "png", # device to use to save images
  width = 6, # width of saved image
  height = 6, # height of saved image
  units = "in", # units for width and height
  dpi = 300 # dpi to use when saving image
)

# load fonts
font_add(
  family = "fb",
  regular = "C:/Users/Bradf/AppData/Local/Microsoft/Windows/Fonts/Font Awesome 6 Brands-Regular-400.otf"
)

font_add_google(name = "Roboto", family = "Roboto")
font <- "Roboto"

font_add_google(name = "Dosis", family = "Dosis")

font_add_google(name = "Neucha", family = "Neucha")

# load caption
caption <- paste0(
  "<span style='font-family:fb;'>&#xf09b;</span>",
  "<span style='font-family:sans;color:white;'>.</span>",
  "<span style='font-family:Dosis;'>bradfordjohnson | TidyTuesday - 2023 Week 10</span>"
)

# visualize
num_month$month <- factor(num_month$month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

ggplot(num_month, aes(x = 0, y = 0, label = paste("Seen:", n, sep = " "))) +
  geom_from_path(aes(path = path), width = 0.6, alpha = num_month$n / 212) +
  facet_wrap(~month) +
  scale_y_continuous(limits = c(-1, 1)) +
  geom_text(nudge_y = -0.65, size = 18, family = "Neucha") +
  theme_bw() +
  labs(
    title = "The Numbat Tracker",
    caption = caption
  ) +
  theme(
    legend.position = "bottom",
    plot.margin = unit(c(4, 10, 4, 10), "mm"),
    strip.background = element_rect(fill = "#c64040", color = "#c64040"),
    strip.text = element_text(size = 40, color = "white", face = "bold", family = font),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_rect(colour = "white"),
    panel.border = element_blank(),
    plot.background = element_rect(fill = "#d9d9d9"),
    plot.caption = ggtext::element_textbox_simple(color = "gray30", size = 25, margin = margin(2, 0, 0, 0, "mm")),
    plot.title = element_text(family = "Neucha", size = 65, hjust = .5, margin = margin(0, 0, 4, 0, "mm"))
  )

# save gif
gg_playback(
  name = file.path("2023", "2023-03-07", "20230307.gif"),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = "#F0F5F5"
)

ggsave("numbats.png", width = 9, height = 9)
