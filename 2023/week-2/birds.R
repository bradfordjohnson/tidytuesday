# load packages
library(tidyverse)

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
  select(-c(subnational1_code, entry_technique, sub_id, obs_id, proj_period_id, month, year, day,
            valid, reviewed, day1_am, day1_pm, day2_am, day2_pm, snow_dep_atleast, effort_hrs_atleast))

# clean up spec_codes for join
spec_codes <- spec_codes |>
  select(-c(extinct, extinct_year))

## join spec cols to feeder watch on species
joined_birds <- feederwatch |>
  left_join(spec_codes, by = "species_code")

# re-order joined df
joined_birds <- joined_birds |>
  select(loc_id, primary_com_name, date, how_many, data_entry_method, effort_hours, snow_depth, latitude, longitude)

# write as csv
write_csv(joined_birds, "feederwatch-cleaned.csv")