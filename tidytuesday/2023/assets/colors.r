library(jsonlite)

polar_night <- list(
  "nord0" = "#2E3440",
  "nord1" = "#3B4252",
  "nord2" = "#434C5E",
  "nord3" = "#4C566A"
)

snow_storm <- list(
  "nord4" = "#D8DEE9",
  "nord5" = "#E5E9F0",
  "nord6" = "#ECEFF4"
)

frost <- list(
  "nord7" = "#8FBCBB",
  "nord8" = "#88C0D0",
  "nord9" = "#81A1C1",
  "nord10" = "#5E81AC"
)

aurora <- list(
  "nord11" = "#BF616A",
  "nord12" = "#D08770",
  "nord13" = "#EBCB8B",
  "nord14" = "#A3BE8C",
  "nord15" = "#B48EAD"
)

color_themes <- list(
  "nord" = list(
    "polar_night" = polar_night,
    "snow_storm" = snow_storm,
    "frost" = frost,
    "aurora" = aurora
  )
)

json_data <- toJSON(color_themes, pretty = TRUE)

writeLines(json_data, "assets/colors.json")
