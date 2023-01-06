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
  mutate(route_time = case_when(is_am == TRUE ~ "Morning Bus Delay",
                                is_am == FALSE ~ "Afternoon Bus Delay"))

### look for outliers in max (delays) remove them for more accurate times
bus |>
  select(-c(boro, bus_company_name, bus_no)) |>
  filter(max < 200 & number_of_students_on_the_bus > 0) |>
  arrange(desc(max))
### maybe do counts of late buses?
### factor so that morning buses are on the left
### keep total time missed in days?
bus$route_time <- factor(bus$route_time, levels = c("Morning Bus Delay", "Afternoon Bus Delay"))

bus |>
  drop_na() |>
  filter(max < 200 & number_of_students_on_the_bus > 0) |>
  group_by(year, route_time) |>
  summarise(count = n()) |>
  ggplot(aes(x = year, y = count, fill = route_time, label = count)) +
  geom_col(position = "dodge")
