# load packages
library(tidyverse)
library(showtext)
library(htmltools)
library(scales)
# library(skimr)

# load data
bus <- read_csv("bus-breakdowns-delays.csv") |>
  janitor::clean_names()

# skim(bus)
# 
# bus |>
#   group_by(boro, breakdown_or_running_late) |>
#   summarise(n=n()) |>
#   arrange(desc(n))

# clean data
## remove columns
bus <- bus |>
  select(-c(busbreakdown_id, created_on, has_contractor_notified_parents, has_contractor_notified_schools,
            have_you_alerted_opt, informed_on, incident_number, last_updated_on, schools_serviced, route_number))

## convert occurred_on to date
bus$occurred_on <- lubridate::mdy_hms(bus$occurred_on, tz = "EST")

bus <- bus |>
  filter(reason == "Heavy Traffic") |>
  mutate(year = lubridate::year(occurred_on))

## sep how_long_delayed
bus <- bus |>
  mutate(max = strsplit(how_long_delayed, "-")) |>
  unnest(max)

bus$max <- gsub("[a-zA-Z]+", "", bus$max)

bus$max <- as.numeric(bus$max)

bus <- bus |>
  mutate(is_am = lubridate::am(bus$occurred_on))

bus <- bus |>
  mutate(route_time = case_when(is_am == TRUE ~ "Morning Bus Delay",
                                is_am == FALSE ~ "Afternoon Bus Delay"))

bus$route_time <- factor(bus$route_time, levels = c("Morning Bus Delay", "Afternoon Bus Delay"))

# visual prep
## fonts
font_add(family = "MulishB",
         regular = "C:/Users/Bradf/AppData/Local/Microsoft/Windows/Fonts/Mulish-Bold.ttf")
font_add(family = "Mulish",
         regular = "C:/Users/Bradf/AppData/Local/Microsoft/Windows/Fonts/Mulish-Regular.ttf")
font_add(family = "fb",
         regular = "C:/Users/Bradf/AppData/Local/Microsoft/Windows/Fonts/Font Awesome 6 Brands-Regular-400.otf")
showtext_auto()

## create caption
caption = paste0("<span style='font-family:fb;color:#FFFFFF;'>&#xf09b;</span>",
                 "<span style='font-family:sans;color:#2e2e2e;'>.</span>",
                 "<span style='font-family:sans;color:#FFFFFF;'>bradfordjohnson</span>")


# visualize data
late_bus <- bus |>
  drop_na() |>
  filter(max < 200 & number_of_students_on_the_bus > 0 & route_time == "Morning Bus Delay") |>
  group_by(year, route_time) |>
  summarise(count = n()) |>
  mutate(to_highlight = ifelse( year >= 2020, "yes", "no")) |>
  ggplot(aes(x = year, y = count, fill = to_highlight)) +
  geom_col(position = "dodge", width = .5) +
  # geom_segment(aes(x = 2020.65, y = -2000, xend = 2022.5, yend = -1900),
  #              colour = "#ff764a", lineend = "round", linejoin = "mitre",
  #              arrow = arrow(length = unit(0.1, "inches"))) +
  scale_fill_manual(values = c("yes" = "#ff764a", "no" = "#2f4b7c")) +
  geom_label(aes(x = 2021, y = -2000),
             label = "Post Covid-19",colour = "white", family = "MulishB", size = 20) +
  labs(title = "Morning School Bus Delays",
       subtitle = "in New York City",
       y = "Total Delays",
       x = "",
       caption = caption) +
  scale_y_continuous(expand = c(.05,0), labels = label_number(suffix = "K", scale = 1e-3)) +
  scale_x_continuous(breaks = c(2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022), expand = c(0.1,0)) +
  theme(legend.position = "none",
        panel.grid.minor.y = element_line(colour = "lightgray", linetype = 3),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_line(colour = "lightgray"),
        plot.title = element_text(family = "MulishB", size = 65, colour = "white"),
        axis.title.y = element_text(angle = 90, family = "MulishB", size = 38, colour = "white"),
        text = element_text(family = "Mulish"),
        axis.text.x = element_text(vjust = 23, colour = "white", family = "MulishB", size = 40),
        axis.text.y = element_text(family = "MulishB", size = 34, colour = "white"),
        plot.margin = unit(c(4,4,4,4), "pt"),
        plot.caption = ggtext::element_textbox_simple(color="#444444", size = 35),
        plot.subtitle = element_text(family = "Mulish", size = 36, colour = "white"),
        plot.background = element_rect(fill = "#2e2e2e"))
late_bus
ggsave("buses.png", width = 9, height = 9)
