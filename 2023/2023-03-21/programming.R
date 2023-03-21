# load packages
pacman::p_load(tidyverse)

# load data
languages <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-03-21/languages.csv')

# explore

languages |>
  filter(is_open_source == TRUE) |>
  group_by(type) |>
  count() |>
  arrange(desc(n)) |>
  print(n=23)

languages |>
  filter(is_open_source == TRUE) |>
  group_by(github_language_type) |>
  count()
  
languages |>
  filter(is_open_source == TRUE) |>
  select(title, github_language_repos) |>
  arrange(desc(github_language_repos))
