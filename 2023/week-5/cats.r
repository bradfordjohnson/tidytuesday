# load packages
library(tidyverse)
library(explore)

# load data
cats_uk <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-31/cats_uk.csv')
cats_uk_reference <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-31/cats_uk_reference.csv')

# clean data
cats_uk <- cats_uk |>
  filter(algorithm_marked_outlier == FALSE & manually_marked_outlier == FALSE) |>
  select(-c(study_name, algorithm_marked_outlier, manually_marked_outlier))

cats_uk_reference |>
  explore()
