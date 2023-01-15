# load packages
library(tidyverse)
library(maps)
library(showtext)
library(htmltools)

# load feeder watch data
feederwatch <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-10/PFW_2021_public.csv') |>
  janitor::clean_names()

# load spec code data
spec_codes <- read_csv("species-codes.csv") |>
  janitor::clean_names()

# clean and remove invalid and na from feeder watch
feederwatch <- feederwatch |>
  filter(valid == 1) |>
  drop_na()

# prepare spec_codes
spec_codes <- spec_codes |>
  select(species_code, primary_com_name, extinct, extinct_year) |>
  filter(is.na(extinct_year))

# remove extinct species < 2021
extinct <- feederwatch |>
  anti_join(spec_codes, by = c("species_code"))

feederwatch <- feederwatch |>
  anti_join(extinct)

# clean up feederwatch
## remove inaccuracies in location data -> see documentation for entry technique
## create new effort hours col
## create new snow depth col
## create data col
feederwatch <- feederwatch |>
  filter(!grepl('POSTCODE|postal', entry_technique)) |> # remove records that involve postal code references ~ location inaccuracy for these in documentation
  mutate(effort_hours = case_when(effort_hrs_atleast == 0.001 ~ "less than 1 hr",
                                  effort_hrs_atleast == 1.001 ~ "1-4 hrs",
                                  effort_hrs_atleast == 4.001 ~ "4-8 hrs",
                                  effort_hrs_atleast == 8.001 ~ "8+ hrs"),
         snow_depth = case_when(snow_dep_atleast == 0 ~ "none",
                                snow_dep_atleast == 0.001 ~ "less than 5 cm",
                                snow_dep_atleast == 5.000 ~ "5 - 15 cm",
                                snow_dep_atleast == 5.001 ~ "more than 15 cm"),
         date = lubridate::make_date(year, month, day))

## remove no longer needed columns
feederwatch <- feederwatch |>
  select(-c(entry_technique, sub_id, obs_id, proj_period_id, month, year, day,
            valid, reviewed, day1_am, day1_pm, day2_am, day2_pm, snow_dep_atleast, effort_hrs_atleast))

# clean up spec_codes for join
spec_codes <- spec_codes |>
  select(-c(extinct, extinct_year))

## join spec cols to feeder watch on species
joined_birds <- feederwatch |>
  left_join(spec_codes, by = "species_code")

# re-order joined df
joined_birds <- joined_birds |>
  select(loc_id, subnational1_code, primary_com_name, date, how_many, data_entry_method, effort_hours, snow_depth, latitude, longitude)

# filter for US southeast
## create two groups for "winter" & "summer"
south_east <-c("US-AL", "US-TN", "US-SC", "US-NC", "US-GA", "US-FL", "US-MS")

south_east_birds <- joined_birds |>
  filter(subnational1_code %in% south_east)

# write as csv
write_csv(south_east_birds, "feederwatch-cleaned.csv")

# most common spotted SE birds
south_east_birds |>
  group_by(primary_com_name) |>
  summarise(count = sum(how_many)) |>
  arrange(desc(count))

# select birds to visualize
selected_birds <-c("Eastern Bluebird", "Northern Cardinal")

selected_birds_df <- south_east_birds |>
  filter(primary_com_name %in% selected_birds)

# get map data 
map_df <- map_data("state")

states <-c("alabama", "florida", "tennessee", "south carolina", "north carolina", "mississippi", "georgia")

map_df <- map_df |>
  filter(region %in% states)

## fonts
font_add(family = "MulishB",
         regular = "C:/Users/Bradf/AppData/Local/Microsoft/Windows/Fonts/Mulish-Bold.ttf")
font_add(family = "Mulish",
         regular = "C:/Users/Bradf/AppData/Local/Microsoft/Windows/Fonts/Mulish-Regular.ttf")
font_add(family = "fb",
         regular = "C:/Users/Bradf/AppData/Local/Microsoft/Windows/Fonts/Font Awesome 6 Brands-Regular-400.otf")
showtext_auto()

caption = paste0("<span style='font-family:fb;color:#404040;'>&#xf09b;</span>",
                 "<span style='font-family:sans;color:#e6e6e6;'>.</span>",
                 "<span style='font-family:sans;color:#404040;'>bradfordjohnson</span>")

# create visual
test_map <- map("state", regions = states, project = "bonne", param = 45)

# colors
colors_df <-c("#0074ee", "#C41E3A")

ggplot(data = map_df) +
  geom_polygon(aes(long, lat, group = group), color = "#e6e6e6", fill = "#404040") +
  geom_jitter(data = selected_birds_df, aes(longitude, latitude, colour = primary_com_name, size = how_many, alpha = .8)) +
  labs(title = "Bluebirds and Cardinals spotted in the Southeast | 2021",
       caption = caption) +
  scale_colour_manual(values = colors_df) +
  guides(size = "none", alpha = "none", colour = guide_legend(override.aes = list(size = 8))) +
  theme_void() +
  theme(legend.position = c(.2,.35),
        legend.title = element_blank(),
        plot.background = element_rect(fill = "#e6e6e6", colour = "#e6e6e6"),
        plot.title = element_text(family = "MulishB", size = 75, colour = "black", hjust = .5),
        plot.caption = ggtext::element_textbox_simple(color="#595959", size = 40),
        plot.margin = unit(c(4,4,4,4), "pt"),
        text = element_text(family = "Mulish"),
        legend.text = element_text(colour = "black", family = "MulishB", size = 50)
        )
  
ggsave("birds.png", width = 9, height = 9)

