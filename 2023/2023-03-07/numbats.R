# ** VISUAL IDEAS ***
# - picture of numbat that is faded when observations are low and brighter when observations are higher

# - 2 tables, one with months in each col, month with most obs gets a numbat, for other table
#     it is times for that selected month (sim to drill down) with same image premise

# - table with header cols as months, and rows as times, same premise as idea 1

# - complete visual with text descriptions of findings and if color is used, incorp into text as "highlighting"

# - what do numbats eat? top of table (month) could be the numbat image fading and bright, bottom of table
#   bottom of table could be what numbats eat, and faded based on n() observations for that hour within respective
#   col


# load packages
library(tidyverse)

# load data
numbats <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-03-07/numbats.csv') |>
  janitor::clean_names()

numbats <- numbats |>
  select(-c(scientific_name, taxon_concept_id, data_resource_name, prcp, tmax, tmin))

# wrangle by hour
numbats |>
  drop_na(hour) |>
  filter(dryandra == TRUE | dryandra == FALSE) |>
  group_by(dryandra, hour) |>
  summarise(n = n())

# wrangle by month
numbats |>
  drop_na(month) |>
  filter(dryandra == TRUE | dryandra == FALSE) |>
  group_by(dryandra, month) |>
  summarise(n = n())

# wrangle by year
numbats |>
  drop_na(year) |>
  filter(dryandra == TRUE | dryandra == FALSE) |>
  group_by(dryandra, year) |>
  summarise(n = n())

# create time / hour df
time_df <- 0:23

time_df <- data.frame(time_df)

numbats$hour <- as.integer(numbats$hour)

hour <- numbats |>
  drop_na(hour) |>
  filter(dryandra == TRUE | dryandra == FALSE) |>
  group_by(dryandra, hour) |>
  summarise(n = n())

# found in dryandra
dryandra <- hour |>
  filter(dryandra == TRUE)

time_df |>
  left_join(dryandra, by = c("time_df" = "hour")) |>
  ggplot(aes(x = time_df, y = n)) +
  geom_col()

# not in dryandra
not_dryandra <- hour |>
  filter(dryandra == FALSE)

time_df |>
  left_join(not_dryandra, by = c("time_df" = "hour")) |>
  ggplot(aes(x = time_df, y = n)) +
  geom_col()
