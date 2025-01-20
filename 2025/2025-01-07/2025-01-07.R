library(tidyverse)
library(ggmagnify)

energy <- read_csv("2025-01-07/energy-usage.csv") %>%
  mutate(date = as.Date(date))

energy <- energy %>%
  mutate(date_numeric = as.numeric(date))

from <- list(xmin = as.numeric(as.Date("2024-12-01")),
             xmax = as.numeric(as.Date("2024-12-31")),
             ymin = 0,
             ymax = 70)

to <- list(xmin = as.numeric(as.Date("2025-02-20")),
           xmax = as.numeric(as.Date("2025-05-20")),
           ymin = 20,
           ymax = 50)

plot <- energy %>%
  ggplot(aes(x = date_numeric, y = kwh)) +
  geom_line(color = "#f9ad55", linewidth = .25) +
  scale_x_continuous(
    name = "Date",
    breaks =
      as.numeric(seq(min(energy$date), max(energy$date), by = "1 month")),
    labels = function(x) format(as.Date(x, origin = "1970-01-01"), "%b"),
    limits =
      c(as.numeric(as.Date("2024-01-01")), as.numeric(as.Date("2025-05-20")))
  ) +
  theme_light()

plot + geom_magnify(
  from = from, to = to, linewidth = 0.25, proj.linetype = 3, axes = "xy"
)
