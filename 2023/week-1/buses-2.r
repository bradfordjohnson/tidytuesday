# load packages
library(tidyverse)
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
  mutate(route_time = case_when(is_am == TRUE ~ "Morning",
                          is_am == FALSE ~ "Afternoon"))

bus |>
  drop_na() |>
  filter(max < 1000 & number_of_students_on_the_bus > 0) |>
  group_by(year, school_age_or_pre_k, route_time) |>
  summarise(total_time_missed_in_days = round(sum(max)/60/24,0)) |>
  ggplot(aes(x = year, y = total_time_missed_in_days, fill = is_am)) +
  geom_col(position = "dodge") +
  facet_wrap(~school_age_or_pre_k)
