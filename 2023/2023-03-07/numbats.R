# load packages
library(tidyverse)

# load data
numbats <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-03-07/numbats.csv') |>
  janitor::clean_names()

numbats <- numbats |>
  select(-c(event_date, scientific_name, taxon_concept_id, data_resource_name,dryandra, prcp, tmax, tmin))