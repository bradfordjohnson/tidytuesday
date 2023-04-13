# load packages
pacman::p_load(tidyverse,
               collapse)

# load data
eggproduction  <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-04-11/egg-production.csv')
cagefreepercentages <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-04-11/cage-free-percentages.csv')

# wrangle data
# eggproduction |>
#   filter(prod_type == "table eggs" & prod_process == "cage-free (non-organic)") |>
#   mutate(egg_per_diff = n_eggs / lag(n_eggs) * 100) |>
#   print(n = 100)
  


eggproduction |>
  filter(prod_type == "table eggs" & prod_process == "cage-free (non-organic)") |>
  fmutate(growth = fgrowth(n_eggs)) |>
  drop_na() |>
  ggplot(aes(x = observed_month, y = growth)) +
    geom_line()

eggproduction |>
  filter(prod_type == "table eggs" & prod_process == "cage-free (non-organic)") |>
  fmutate(growth = fgrowth(n_hens)) |>
  drop_na() |>
  ggplot(aes(x = observed_month, y = growth)) +
  geom_line()
