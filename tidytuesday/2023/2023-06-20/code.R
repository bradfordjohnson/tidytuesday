pacman::p_load(
  tidyverse,
  tidytext,
  showtext,
  htmltools,
  ggraph,
  igraph
)

showtext_auto()
showtext_opts(dpi = 300)

font_add(
  family = "fb", regular = "C:/Users/Bradf/AppData/Local/Microsoft/Windows/Fonts/Font Awesome 6 Brands-Regular-400.otf"
)

font_add_google(name = "Roboto Slab", family = "Roboto Slab")
font_1 <- "Roboto Slab"

font_add_google(name = "Roboto", family = "Roboto")
font_2 <- "Roboto"

caption <- paste0(
  "<span style='font-family:fb;'>&#xf09b;</span>",
  "<span style='font-family:sans;color:#e8eae7;'>.</span>",
  "<span style='font-family:Roboto;'>bradfordjohnson | TidyTuesday - 2023 Week 25</span>"
)

ufo_sightings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-20/ufo_sightings.csv")

sc_sightings <- ufo_sightings %>%
  filter(state == "SC") %>%
  select(reported_date_time, summary)

sc_bigrams <- sc_sightings %>%
  unnest_tokens(bigram, summary, token = "ngrams", n = 2) %>%
  filter(!is.na(bigram))

bigrams_separated <- sc_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)

bigram_graph <- bigram_counts %>%
  filter(n > 10) %>%
  graph_from_data_frame()

set.seed(2020)

plot_bg <- "#E3E2DA"

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n),
    show.legend = FALSE,
    arrow = a, end_cap = circle(.07, "inches"),
    color = "#E2401D"
  ) +
  geom_node_point(color = "#3C393D", size = 5) +
  geom_node_text(aes(label = name), vjust = 1.7, hjust = 1, size = 3, family = font_2) +
  labs(
    title = "UFO Sightings in South Carolina", subtitle = "Bigrams from witness accounts",
    caption = caption
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = plot_bg, color = plot_bg),
    panel.background = element_rect(fill = plot_bg, color = plot_bg),
    plot.margin = margin(10, 5, 5, 5),
    plot.title = element_text(
      margin = margin(2, 0, 3, 0, "mm"),
      size = 15,
      family = font_1,
      face = "bold",
      vjust = 0,
      hjust = .5,
      color = "black"
    ),
    plot.subtitle = element_text(
      margin = margin(1, 0, 0, 1),
      size = 13,
      family = font_1,
      face = "bold",
      vjust = 0,
      hjust = .5,
      color = "black"
    ),
    plot.caption = ggtext::element_textbox_simple(
      margin = margin(10, 0, 0, 0),
      halign = .01, color = "gray10", size = 5
    ),
  )

ggsave("ufo.png", width = 8, height = 6)
