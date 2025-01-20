# rough draft
library(tidyverse)
library(gt)

df_2022 <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-01-28/water_insecurity_2022.csv")
df_2023 <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-01-28/water_insecurity_2023.csv")

df_2022 <- df_2022 %>%
    select(geoid, name, year, total_pop, plumbing, percent_lacking_plumbing)

df_2023 <- df_2023 %>%
    select(geoid, name, year, total_pop, plumbing, percent_lacking_plumbing)


combined <- full_join(df_2022, df_2023, by = c("geoid", "name"), suffix = c("_2022", "_2023"))


result <- combined %>%
    mutate(
        delta_plumbing = plumbing_2023 - plumbing_2022,
        delta_percent_lacking_plumbing = percent_lacking_plumbing_2023 - percent_lacking_plumbing_2022,
        plumbing_2022 = coalesce(plumbing_2022, 0),
        plumbing_2023 = coalesce(plumbing_2023, 0),
        percent_lacking_plumbing_2022 = coalesce(percent_lacking_plumbing_2022, 0),
        percent_lacking_plumbing_2023 = coalesce(percent_lacking_plumbing_2023, 0)
    )

result %>%
    select(name, plumbing_2022, plumbing_2023, delta_plumbing) %>%
    mutate(percent_change = (plumbing_2023 / plumbing_2022) - 1) %>%
    arrange(desc(abs(delta_plumbing)))

result %>%
    select(name, percent_lacking_plumbing_2022, percent_lacking_plumbing_2023, delta_percent_lacking_plumbing) %>%
    arrange(desc(abs(delta_percent_lacking_plumbing))) %>%
    head(10) %>%
    gt() %>%
    fmt_percent(columns = c(percent_lacking_plumbing_2022, percent_lacking_plumbing_2023, delta_percent_lacking_plumbing), scale_values = FALSE) %>%
    tab_style(
        style = list(
            cell_text(weight = "bold", color = "#AE362E")
        ),
        locations = cells_body(
            columns = delta_percent_lacking_plumbing,
            rows = delta_percent_lacking_plumbing < 0
        )
    ) %>%
    tab_style(
        style = list(
            cell_text(weight = "bold", color = "#0d8832")
        ),
        locations = cells_body(
            columns = delta_percent_lacking_plumbing,
            rows = delta_percent_lacking_plumbing > 0
        )
    )
