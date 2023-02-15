# packages
library(tidyverse)

# load data
age_gaps <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-14/age_gaps.csv')

# wrangle data
age_gaps <- age_gaps |>
  mutate(relationship = case_when(
    character_1_gender == "man" ~ "Older Man",
    character_1_gender == "woman" ~ "Older Woman"
    ))