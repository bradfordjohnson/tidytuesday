# load packages
library(tidyverse)
library(ggridges)
library(showtext)
library(htmltools)
  
# load synthetic data
rfid_df <- read_csv("rfid-tag-data.csv") |>
  janitor::clean_names()

# remove unwanted columns
## removing percents fields to calculate new and uniform fields
rfid_df <- rfid_df |>
  select(-c(venue, percent_edit, percent_no_tag, suppressed_tags, percent_wagsak))

# remove na records
rfid_df <- rfid_df |>
  drop_na()

# change date from char to date data type
rfid_df$date <- lubridate::mdy(rfid_df$date)

# calculate new percent fields, in decimal form
rfid_df <- rfid_df |>
  mutate(percent_edit = round(edited / dispensed, 6),
         percent_tag_error = round(no_tag / dispensed, 6),
         percent_wagsak = round(wagsak_total / dispensed, 6))

# load fonts 
font_add(family = "Roboto",
         regular = "C:/Users/Bradf/AppData/Local/Microsoft/Windows/Fonts/Roboto-Regular.ttf")
font_add(family = "RobotoB",
         regular = "C:/Users/Bradf/AppData/Local/Microsoft/Windows/Fonts/Roboto-Bold.ttf")
font_add(family = "fb",
         regular = "C:/Users/Bradf/AppData/Local/Microsoft/Windows/Fonts/Font Awesome 6 Brands-Regular-400.otf")
showtext_auto()

# create caption
caption = paste0("<span style='font-family:fb;'>&#xf09b;</span>",
                 "<span style='font-family:sans;color:white;'>.</span>",
                 "<span style='font-family:sans;'>bradfordjohnson | TidyTuesday - 2023 Week 1 | Using Synthetic Data</span>")

# factor days of the week in order (M-U)
rfid_df$week_day <- factor(rfid_df$week_day,
                           levels = c("Sunday","Saturday", "Friday",
                                      "Thursday", "Wednesday", "Tuesday", "Monday"))

# density ridges by day for rfid observations
daily_rfid_dist <- rfid_df |>
  ggplot(aes(x = dispensed, y = week_day, fill = factor(after_stat(quantile)))) +
  stat_density_ridges(rel_min_height = 0.01,
                      geom = "density_ridges_gradient",
                      calc_ecdf = TRUE,
                      quantiles = 4, 
                      quantile_lines = TRUE,
                      jittered_points = TRUE,
                      position = position_points_jitter(width = 0.05, height = 0),
                        point_shape = '|', point_size = 8, point_alpha = 1, alpha = 0.7) +
  scale_fill_viridis_d(name = "Quartiles", alpha = .7) +
  theme_ridges() +
  labs(title = "Daily Distributions - RFID Tags",
       x = "Number of RFID Tags",
       caption = caption) +
  theme(text = element_text(family = "Roboto"),
        plot.title = element_text(family = "RobotoB", hjust = .5, vjust = 2, size = 58),
        legend.title.align = 0.5,
        legend.title = element_text(family = "RobotoB", size = 32),
        legend.text = element_text(family = "Roboto", size = 28),
        axis.title.x = element_text(family = "RobotoB", size = 32),
        axis.title.y = element_blank(),
        plot.caption = ggtext::element_textbox_simple(color="#444444", size = 30),
        axis.text.x = element_text(family = "Roboto", size = 32),
        axis.text.y = element_text(family = "RobotoB", size = 34),
        plot.margin = unit(c(8,4,8,4), "pt"),
        panel.grid.major = element_line(colour = "#f3f3f3"),
        panel.background = element_rect(fill = "gray90"),
        plot.background = element_rect(fill = "gray90")
        )

daily_rfid_dist

ggsave("rfid-v1.png", width = 9, height = 9)
