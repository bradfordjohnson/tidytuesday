pacman::p_load(tidyverse)

plots <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-02/plots.csv')
species <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-02/species.csv')
surveys <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-02/surveys.csv')

surveys |>
  group_by(plot, species) |>
  count() |>
  ggplot(aes(x = species, y = n)) +
  geom_col() +
  facet_wrap(~plot)

surveys |>
  left_join(species, by = c("species" = "species")) |>
  group_by(commonname) |>
  count() |>
  ggplot(aes(x = fct_infreq(commonname, n), y = n)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90))

surveys |>
  ggplot(aes(x = wgt, y = hfl)) +
  geom_jitter(aes(color = treatment, alpha = .1))

surveys |>
  left_join(species, by = c("species" = "species")) |>
  # slice_sample(n = 10000) |>
  ggplot(aes(x = wgt, y = hfl)) +
  geom_jitter(alpha = .5, aes(color = as.factor(granivore))) +
  geom_smooth(aes(color = as.factor(granivore))) +
  facet_wrap(~treatment)

