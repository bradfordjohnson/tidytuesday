# load packages
library(tidyverse)
library(camcorder)
library(showtext)
library(htmltools)

showtext_auto()

# load data
bob_ross <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-21/bob_ross.csv')

# wrangle data
colors <- bob_ross |>
  pivot_longer(cols = Black_Gesso:Alizarin_Crimson,
               names_to = "color",
               values_to = "logical"
  )

count_table <- colors |>
  group_by(color) |>
  summarise(count = sum(logical)) |>
  arrange(desc(count))

count_table$color <- gsub("_", " ", count_table$color)

# start recording
gg_record(
  dir = file.path("2023", "2023-02-21", "recording"), # where to save the recording
  device = "png", # device to use to save images
  width = 6, # width of saved image
  height = 6, # height of saved image
  units = "in", # units for width and height
  dpi = 300 # dpi to use when saving image
)

# prepare colors
color_pal <-c("#FFFFFF", "#4E1500", "#221B15", "#FFEC00", "#C79B00", "#0C0040",
                       "#DB0000", "#000000", "#0A3410", "#FFB800", "#5F2E1F",
                       "#021E44", "#102E3C", "#000000", "#8A3324", "#FFFFFF",
                       "#000000", "#CD5C5C"
                       )


count_table <- count_table |>
  mutate(hex = color_pal)

# prepare fonts
font_add(family = "fb",
         regular = "C:/Users/Bradf/AppData/Local/Microsoft/Windows/Fonts/Font Awesome 6 Brands-Regular-400.otf")

font_add_google(name = "Patrick Hand", family = "Patrick Hand")

font_add_google(name = "Neucha", family = "Neucha")

# prepare caption
caption = paste0("<span style='font-family:fb;'>&#xf09b;</span>",
                 "<span style='font-family:sans;color:white;'>.</span>",
                 "<span style='font-family:Neucha;'>bradfordjohnson | TidyTuesday - 2023 Week 8</span>")

# visualize
count_table |>
  ggplot(aes(x = fct_infreq(color, count), y = count)) +
  geom_col(fill = color_pal) +
  scale_fill_manual(values = color_pal) +
  theme_minimal() +
  labs(title = "The Colors of 400+ Bob Ross Paintings",
       y = "Times Used",
       x = "",
       caption = caption) +
  coord_cartesian(ylim = c(0, 400), expand = FALSE) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, family = "Patrick Hand", size = 35, color = "gray10"),
        axis.title.x = element_blank(),
        axis.text.y = element_text(family = "Patrick Hand", size = 30),
        axis.title.y = element_text(margin = margin(0,3,0,0, "mm"), family = "Patrick Hand", size = 40),
        plot.title = element_text(hjust = 0.5, family = "Patrick Hand", size = 65),
        plot.caption = ggtext::element_textbox_simple(color="gray20", size = 30, margin = margin(5,0,0,0,"mm")),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_line(linetype = "dotted", color = "gray30"),
        panel.grid.major.y = element_line(color = "gray30", linewidth = .2),
        panel.background = element_rect(fill = "#EAE2D6", color = "#EAE2D6"),
        plot.background = element_rect(fill = "#EAE2D6", color = "#EAE2D6"),
        plot.margin = unit(c(3,10,3,3), "mm"))

# save gif
gg_playback(
  name = file.path("2023", "2023-02-21","20230221.gif"),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .25,
  background = "#F0F5F5"
)

ggsave("bob-ross.png", width = 9, height = 9)
