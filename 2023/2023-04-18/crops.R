# load packages
pacman::p_load(
  tidyverse,
  networkD3,
  igraph,
  htmltools
)

# load data
founder_crops <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-04-18/founder_crops.csv")

# wrangle data
# categories <- founder_crops |>
#   select(taxon_source, category) |>
#   group_by(category) |>
#   count() |>
#   arrange(desc(n)) |>
#   drop_na()

grasses <- founder_crops |>
  filter(category == "Grasses") |>
  group_by(category, edibility, genus) |>
  count() |>
  arrange(desc(n)) |>
  mutate(edibility = case_when(
    edibility = is.na(edibility) ~ "Not Edible",
    .default = "Edible"
  ))

wild_plants <- founder_crops |>
  filter(category == "Wild plants") |>
  group_by(category, edibility, genus) |>
  count() |>
  arrange(desc(n)) |>
  mutate(edibility = case_when(
    edibility = is.na(edibility) ~ "Not Edible",
    .default = "Edible"
  ))

pulses <- founder_crops |>
  filter(category == "Pulses") |>
  group_by(category, edibility, genus) |>
  count() |>
  arrange(desc(n)) |>
  mutate(edibility = case_when(
    edibility = is.na(edibility) ~ "Not Edible",
    .default = "Edible"
  ))

fruits <- founder_crops |>
  filter(category == "Fruits/nuts") |>
  group_by(category, edibility, genus) |>
  count() |>
  arrange(desc(n)) |>
  mutate(edibility = case_when(
    edibility = is.na(edibility) ~ "Not Edible",
    .default = "Edible"
  )) |>
  select(-n)


rsplit <- function(x) {
  x <- x[!is.na(x[, 1]), , drop = FALSE]
  if (nrow(x) == 0) {
    return(NULL)
  }
  if (ncol(x) == 1) {
    return(lapply(x[, 1], function(v) list(name = v)))
  }
  s <- split(x[, -1, drop = FALSE], x[, 1])
  unname(mapply(function(v, n) {
    if (!is.null(v)) list(name = n, children = v) else list(name = n)
  }, lapply(s, rsplit), names(s), SIMPLIFY = FALSE))
}
split_g <- rsplit(grasses)
split_wp <- rsplit(wild_plants)
split_f <- rsplit(fruits)
split_p <- rsplit(pulses)

test <- list(name = "Founder Crops", children = c(split_g, split_p, split_wp))
diagonal_plot <- diagonalNetwork(List = test, fontSize = 10, opacity = 0.9)


colorVector <- c(rep("#96B48C", 235))

jsarray <- paste0('["', paste(colorVector, collapse = '", "'), '"]')
nodeStrokeJS <- JS(paste0("function(d, i) { return ", jsarray, "[i]; }"))
radial_plot <- radialNetwork(
  List = test, fontSize = 12, opacity = 0.95, linkColour = nodeStrokeJS, nodeColour = nodeStrokeJS,
  nodeStroke = nodeStrokeJS, textColour = "#0f120e"
)

radial_plot <- htmlwidgets::prependContent(radial_plot, htmltools::tags$body("Neolithic Founder Crops - Edible genera"))
radial_plot

#---- try ggplot ----
# load packages
pacman::p_load(
  ggpath,
  showtext,
  ggtext
)

showtext_auto()

# load data
local_image_path <- "Rplot.png"

x <- c(0)
y <- c(0)
path <- c(local_image_path)

ggpath <- data.frame(x, y, path)

# load fonts
font_add(
  family = "fb",
  regular = "C:/Users/Bradf/AppData/Local/Microsoft/Windows/Fonts/Font Awesome 6 Brands-Regular-400.otf"
)

font_add_google(name = "Roboto", family = "Roboto")
font_1 <- "Roboto"

font_add_google(name = "Dosis", family = "Dosis")

# load caption
caption <- paste0(
  "<span style='font-family:fb;'>&#xf09b;</span>",
  "<span style='font-family:sans;color:#FFFFFF;'>.</span>",
  "<span style='font-family:Dosis;'>bradfordjohnson | TidyTuesday - 2023 Week 16</span>"
)

ggplot(ggpath, aes(x = x, y = y)) +
  geom_from_path(aes(path = path)) +
  theme_void() +
  labs(
    title = "Neolithic Founder Crops",
    subtitle = "Edible or not?",
    caption = caption
  ) +
  theme(
    plot.title = element_textbox_simple(
      family = font_1, size = 48, halign = .5, color = "#1B1916", face = "bold",
      margin = margin(2, 2, 2, 2, "mm")
    ),
    plot.subtitle = element_textbox_simple(
      family = font_1, size = 35, halign = .5, color = "#1B1916", face = "bold",
      margin = margin(2, 2, 2, 2, "mm")
    ),
    plot.caption = ggtext::element_textbox_simple(color = "#1B1916", size = 20, halign = .99, margin = margin(2, 2, 2, 2, "mm")),
    plot.background = element_rect(color = "white", fill = "white"),
    panel.background = element_rect(color = "white", fill = "white")
  )

ggsave("crops.png", width = 6, height = 6)
