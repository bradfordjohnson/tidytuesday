# load packages
library(tidyverse)


# load data
loadouts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-24/loadouts.csv')
survivalists <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-24/survivalists.csv')

# clean data
loadouts <- loadouts |>
  select(name, item_number, item)

survivalists <- survivalists |>
  select(name, days_lasted, result, season)

joined_df <- survivalists |>
  left_join(loadouts, by = c("name" = "name"))

first_place <- joined_df |>
  filter(result == 1)

last_place <- joined_df |>
  filter(result == 10 | result == 7 & season == 4) # season 4 had teams

first_place_items <- first_place |>
  group_by(item) |>
  summarise(n = n()) |>
  filter(n > 1) |>
  arrange(desc(n))

last_place_items <- last_place |>
  group_by(item) |>
  summarise(n = n()) |>
  filter(n > 1) |>
  arrange(desc(n))

first_place_items <- first_place_items |>
  mutate(place = "First")

last_place_items <- last_place_items |>
  mutate(place = "Last", n = n * -1)

items_bind <- first_place_items |>
  rbind(last_place_items)

# visual
items_bind |>
  group_by(place) |>
  ggplot(aes(x = fct_infreq(item), y = n, fill = place)) +
  geom_col()