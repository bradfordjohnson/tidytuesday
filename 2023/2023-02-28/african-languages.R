# laod packages
library(ggsankey)
library(tidyverse)
library(camcorder)
library(showtext)
library(htmltools)

showtext_auto()

# load data
afrisenti <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-28/afrisenti.csv")
languages <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-28/languages.csv")
language_countries <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-28/language_countries.csv")

# wrangle data
df <- language_countries |>
  left_join(languages, by = c("language_iso_code" = "language_iso_code"))

df <- afrisenti |>
  left_join(df, by = c("language_iso_code" = "language_iso_code"))

df <- df |>
  select(country, language, label) |>
  mutate(Country = country, Language = language, Sentiment = label) |>
  select(-c(country, language, label))

selected_countries <- c("Nigeria", "Ghana", "Cameroon", "Ethiopia")

df <- df |>
  filter(Country %in% selected_countries)

df$Sentiment <- str_to_title(df$Sentiment)

df <- df |>
  make_long(Country, Language, Sentiment)

df |>
  head()

# start recording
gg_record(
  dir = file.path("2023", "2023-02-28", "recording"), # where to save the recording
  device = "png", # device to use to save images
  width = 6, # width of saved image
  height = 6, # height of saved image
  units = "in", # units for width and height
  dpi = 300 # dpi to use when saving image
)

# load fonts
font_add(family = "fb",
         regular = "C:/Users/Bradf/AppData/Local/Microsoft/Windows/Fonts/Font Awesome 6 Brands-Regular-400.otf")

font_add_google(name = "Work Sans", family = "Work Sans")
font <- "Work Sans"

font_add_google(name = "Dosis", family = "Dosis")


# load caption
caption = paste0("<span style='font-family:fb;'>&#xf09b;</span>",
                 "<span style='font-family:sans;color:white;'>.</span>",
                 "<span style='font-family:Dosis;'>bradfordjohnson | TidyTuesday - 2023 Week 9</span>")

# start visualizing
pl <- ggplot(df, aes(x = x,                        
                     next_x = next_x,                                     
                     node = node,
                     next_node = next_node,        
                     fill = factor(node),
                     label = node))

pl <- pl +geom_sankey(flow.alpha = 0.5,          #This Creates the transparency of your node 
                      node.color = "black",     # This is your node color        
                      show.legend = TRUE)        # This determines if you want your legend to show

# aes
pl +
  theme_bw() +
  labs(title = "African Language Sentiment",
       fill = "Nodes",
       caption = caption) +
  geom_sankey_text(size = 7, 
                    color = "black",
                    type = "sankey",
                    family = font,
                   angle = 0,
                   hjust = -.60, vjust = 1.5) + # This specifies the Label format for each node
  scale_x_discrete(position = "top") +
  scale_fill_viridis_d(option = "viridis") +
  theme(legend.position = 'none',
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill = "#FAF5D6", color = "#FAF5D6"),
        plot.background = element_rect(fill = "#FAF5D6", color = "#FAF5D6"),
        plot.title = element_text(family = font, hjust = .5, vjust = 0, size = 58, margin = margin(0,0,5,0,"mm")),
        plot.caption = ggtext::element_textbox_simple(color="#3D4750", size = 25),
        axis.text.x = element_text(family = font, size = 30, face = "bold")
        )

# save gif
gg_playback(
  name = file.path("2023", "2023-02-28","20230228.gif"),
  first_image_duration = 4,
  last_image_duration = 20,
  frame_duration = .20,
  background = "#F0F5F5"
)

ggsave("african-languages.png", width = 9, height = 9)
