# load packages
pacman::p_load(tidyverse)

# load data
tuesdata <- tidytuesdayR::tt_load(2023, week = 17)

london_marathon <- tuesdata$london_marathon |>
  janitor::clean_names()

# wrangle data