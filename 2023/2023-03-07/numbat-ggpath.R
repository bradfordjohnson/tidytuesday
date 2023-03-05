# load packages
library(tidyverse)
library(ggpath)

local_image_path <- "numbat.png"
# load data
numbats <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-03-07/numbats.csv') |>
  janitor::clean_names()

numbats <- numbats |>
  select(-c(scientific_name, taxon_concept_id, data_resource_name, prcp, tmax, tmin))

# wrangle by hour
hour_test <- numbats |>
  drop_na(hour) |>
  filter(dryandra == TRUE) |>
  group_by(dryandra, hour) |>
  summarise(n = n()) |>
  mutate(path = local_image_path)

ggplot(hour_test, aes(x = 0, y = 0)) +
  geom_from_path(aes(path = path), width = 0.4, alpha = hour_test$n / 19) +
  facet_wrap(~hour) +
  theme_void() +
  theme(legend.position = "bottom",
        plot.margin = unit(c(10,10,10,10), "mm"))



# wrangle by month
test <- numbats |>
  drop_na(month) |>
  filter(dryandra == TRUE) |>
  group_by(month) |>
  summarise(n = n()) |>
  mutate(path = local_image_path)

test$month <- factor(test$month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct","Nov", "Dec"))
  ggplot(test, aes(x = 0, y = 0)) +
  geom_from_path(aes(path = path), width = 0.4, alpha = test$n / 13) +
    facet_wrap(~month) +
    theme_void() +
    theme(legend.position = "bottom",
          plot.margin = unit(c(10,10,10,10), "mm"))
  
  # wrangle by hour and FALSE
  hour_test <- numbats |>
    drop_na(hour) |>
    filter(dryandra == FALSE) |>
    group_by(dryandra, hour) |>
    summarise(n = n()) |>
    mutate(path = local_image_path)
  
  ggplot(hour_test, aes(x = 0, y = 0)) +
    geom_from_path(aes(path = path), width = 0.4, alpha = hour_test$n / 441) +
    facet_wrap(~hour) +
    theme_void() +
    theme(legend.position = "bottom",
          plot.margin = unit(c(10,10,10,10), "mm"))
  
  
  
  # wrangle by month
  test <- numbats |>
    drop_na(month) |>
    filter(dryandra == FALSE) |>
    group_by(month) |>
    summarise(n = n()) |>
    mutate(path = local_image_path)
  
  test$month <- factor(test$month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct","Nov", "Dec"))
  ggplot(test, aes(x = 0, y = 0)) +
    geom_from_path(aes(path = path), width = 0.4, alpha = test$n / 208) +
    facet_wrap(~month) +
    theme_void() +
    theme(legend.position = "bottom",
          plot.margin = unit(c(10,10,10,10), "mm"))
