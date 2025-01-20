library(tidyverse)
library(gt)
library(webshot2)

water_2022 <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-01-28/water_insecurity_2022.csv")
water_2023 <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-01-28/water_insecurity_2023.csv")

water_2022 <- water_2022 %>%
  select(geoid, name, year, total_pop, plumbing, percent_lacking_plumbing)

water_2023 <- water_2023 %>%
  select(geoid, name, year, total_pop, plumbing, percent_lacking_plumbing)

combined <- full_join(
  water_2022,
  water_2023,
  by = c("geoid", "name"),
  suffix = c("_2022", "_2023")
)

result <- combined %>%
  mutate(
    delta_plumbing = plumbing_2023 - plumbing_2022,
    delta_percent_lacking_plumbing =
      percent_lacking_plumbing_2023 - percent_lacking_plumbing_2022,
    plumbing_2022 = coalesce(plumbing_2022, 0),
    plumbing_2023 = coalesce(plumbing_2023, 0),
    percent_lacking_plumbing_2022 = coalesce(percent_lacking_plumbing_2022, 0),
    percent_lacking_plumbing_2023 = coalesce(percent_lacking_plumbing_2023, 0)
  )

gt_table <- result %>%
  select(
    name,
    percent_lacking_plumbing_2022,
    percent_lacking_plumbing_2023,
    delta_percent_lacking_plumbing
  ) %>%
  arrange(desc(abs(delta_percent_lacking_plumbing))) %>%
  head(10) %>%
  gt() %>%
  fmt_percent(
    columns = c(
      percent_lacking_plumbing_2022,
      percent_lacking_plumbing_2023,
      delta_percent_lacking_plumbing
    ),
    scale_values = FALSE
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold", color = "#AE362E")
    ),
    locations = cells_body(
      columns = delta_percent_lacking_plumbing,
      rows = delta_percent_lacking_plumbing > 0
    )
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold", color = "#0d8832")
    ),
    locations = cells_body(
      columns = delta_percent_lacking_plumbing,
      rows = delta_percent_lacking_plumbing < 0
    )
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) %>%
  tab_header(
    title = "Top 10 U.S. Counties by YoY change in Plumbing Gaps"
  ) %>%
  tab_spanner(
    label = "% of Population Lacking Plumbing Facilities",
    columns = starts_with("percent")
  ) %>%
  cols_label(
    name = "County",
    percent_lacking_plumbing_2022 = "2022",
    percent_lacking_plumbing_2023 = "2023",
    delta_percent_lacking_plumbing = "YoY Δ"
  ) %>%
  tab_source_note(
    source_note = "Source: tidycensus • Graphic: Ford Johnson"
  ) %>%
  opt_table_font(
    font = list(
      google_font(name = "Open Sans"),
      "Cochin", "serif"
    )
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_title("title")
  ) %>%
  tab_options(
    table.border.bottom.width = px(0)
  )

gtsave(gt_table, filename = "2025/2025-01-28/water-insecurity.png")
