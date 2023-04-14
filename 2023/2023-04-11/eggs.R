# load packages
pacman::p_load(tidyverse,
               collapse,
               ggpath,
               camcorder,
               patchwork,
               showtext,
               htmltools,
               ggtext)

showtext_auto()

# load data
eggproduction  <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-04-11/egg-production.csv')

# load local image
local_image_path <- "egg.svg"

# start recording
gg_record(
  dir = file.path("2023", "2023-04-11", "recording"), # where to save the recording
  device = "png", # device to use to save images
  width = 6, # width of saved image
  height = 6, # height of saved image
  units = "in", # units for width and height
  dpi = 300 # dpi to use when saving image
)

# load fonts
font_add(family = "fb",
         regular = "C:/Users/Bradf/AppData/Local/Microsoft/Windows/Fonts/Font Awesome 6 Brands-Regular-400.otf")

font_add_google(name = "Public Sans", family = "Public Sans")
font_1 <- "Public Sans"

font_add_google(name = "Dosis", family = "Dosis")

# load caption
caption = paste0("<span style='font-family:fb;'>&#xf09b;</span>",
                 "<span style='font-family:sans;color:#FFFFFF;'>.</span>",
                 "<span style='font-family:Dosis;'>bradfordjohnson | TidyTuesday - 2023 Week 15</span>")

# titles for plots
title_1 <- paste0("Egg Production Growth Rates: ",
                  "<span style='color:#8cb369'> ",
                  "Top 6</span> vs ",
                  "<span style='color:#bc4749'> Bottom 6</span>")

title_2 <- paste0("<span style='color:#1B1916'>Production of Cage-Free (Non-Organic) Table Eggs</span>")

# wrangle data
table_eggs_growth <- eggproduction |>
  filter(prod_type == "table eggs" & prod_process == "cage-free (non-organic)") |>
  fmutate(growth = fgrowth(n_eggs)) |>
  drop_na()

top_6_growth_months <- table_eggs_growth |>
  arrange(desc(growth)) |>
  head(6) |>
  mutate(color = "#8cb369", path = local_image_path, x = 1:6)

bottom_6_growth_months <- table_eggs_growth |>
  arrange(growth) |>
  head(6) |>
  mutate(color = "#bc4749", path = local_image_path, x = 1:6)

dozen_eggs <- top_6_growth_months |>
  rbind(bottom_6_growth_months)

dozen_eggs <- dozen_eggs |>
  mutate(y = case_when(
    growth > 0 ~ 1,
    growth < 0 ~ 0))

# visualize egg carton
 p1 <- dozen_eggs |>
  ggplot(aes(x = x, y = y)) +
  geom_from_path(aes(path = path), width = .2) +
  geom_text(aes(x = x, y = y -.22, label = substr(observed_month,0,7)), size = 10, color = dozen_eggs$color,
            family = font_1) +
  geom_text(aes(x = x, y = y, label = paste0(round(growth, 1), "%")), size = 18, color = dozen_eggs$color,
            family = font_1) +
  scale_y_continuous(limits = c(-.5,5)) +
  scale_x_continuous(limits = c(.5, 6.5)) +
  theme_void() +
  geom_textbox(aes(x = 3.5, y = 1.7, label = title_1),
               box.colour = NA, fill = NA, family = font_1, size = 13,
               width = unit(20, "lines"), color = "#1B1916") +
   labs(caption = caption) +
  theme(plot.background = element_rect(color = "white", fill = "white"),
        panel.background = element_rect(color = "white", fill = "white"),
        panel.spacing = unit(-0.5, "lines"),
        plot.caption = ggtext::element_textbox_simple(color="#1B1916", size = 24,
                                                      margin = margin(0,2,2,2,"mm"),
                                                      halign = 1))
 
# visualize line trend
p2 <- table_eggs_growth |>
  ggplot(aes(x = observed_month, y = growth)) +
  geom_line(alpha = .3, color = "#CCBFAA") +
  geom_smooth(se = FALSE, color = "#ffefd5") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(x = "",
       y = "Growth Rate",
       title = title_2) +
  theme_minimal() +
  theme(plot.background = element_rect(color = "white", fill = "white"),
        panel.background = element_rect(color = "white", fill = "white"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(family = font_1, color = "#1B1916", size = 28),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "#222222", linewidth = .02),
        plot.title = element_textbox_simple(family = font_1, size = 48, halign = 0, color = "#1B1916", face = "bold"),
        text = element_text(family = font_1, size = 25, color = "#1B1916"),
        title = element_text(family = font_1, color = "#1B1916"),
        axis.text = element_text(family = font_1, size = 28, color = "#1B1916")
        )


# patchwork
p1 + inset_element(p2, 0, 0.5, 1, 1, align_to = "full")

# save gif
gg_playback(
  name = file.path("2023", "2023-04-11","eggs1.gif"),
  first_image_duration = 6,
  last_image_duration = 25,
  frame_duration = .1,
  background = "white"
)
