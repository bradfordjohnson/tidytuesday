# load packages
library(tidyverse)
library(showtext)
library(htmltools)

## fonts
font_add(family = "fb",
         regular = "C:/Users/Bradf/AppData/Local/Microsoft/Windows/Fonts/Font Awesome 6 Brands-Regular-400.otf")
font_add_google(name = "Ubuntu", family = "Ubuntu")
font <- "Ubuntu"


showtext_auto()
showtext_opts(dpi = 320)

## create caption
caption = paste0("<span style='font-family:fb;color:#626064;'><br>&#xf09b;</span>",
                 "<span style='font-family:sans;color:#f0edd4;'>.</span>",
                 "<span style='font-family:sans;color:#626064;'>bradfordjohnson | TidyTuesday - Week 3 | Original Design: Ryan Hart</span>")


# load data
artists <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-17/artists.csv')

# wrangle data
df <- artists %>% 
  select(edition_number, book, artist_name, space_ratio_per_page_total) %>% 
  group_by(edition_number, book) %>% 
  mutate(avg = mean(space_ratio_per_page_total, na.rm = TRUE)) %>% 
  filter(artist_name == "Gustave Courbet") %>% 
  mutate(delta = space_ratio_per_page_total - avg) %>% 
  pivot_longer(cols = c(space_ratio_per_page_total, avg)) %>% 
  mutate(name = recode(name, space_ratio_per_page_total = "Courbet", avg = "Average")) %>% 
  filter(book == "Janson")

# required to conditionally adjust the hjust for geom_text labels ---------
align <- ifelse(df$name == "Average", 0, 1)

# create plot
df %>% 
  ggplot(aes(y = as.character(edition_number), x = value)) +
  geom_line(aes(group = edition_number), size = 3, color = "#B4632F") +
  geom_text(aes(label = name), nudge_y = 0.3, size = 3, color = "#626064", hjust = align, family = font) +
  scale_x_continuous() +
  theme_minimal() +
  theme(plot.title = element_text(family = font, size = 28, hjust = 0.5, color = "#B4632F"),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = font, size = 11, hjust = 0.5, lineheight = 1.1, color = "#626064" ),
        plot.caption.position = "panel",
        plot.caption = ggtext::element_textbox_simple(color="#444444", size = 10, halign = -.15, vjust = 1),
        legend.position = "none",
        axis.title = element_text(size = 11, family = font, color = "#626064", hjust = 0.5),
        axis.text = element_text(size = 9, family = font, color = "#626064", hjust = 0.5),
        panel.grid = element_line(linewidth = 0.35, linetype = "dotted", color = "#626064"),
        plot.margin = unit(c(1.0, 1.0, 1.0, 1.0), "cm"),
        plot.background = element_rect(color = "#f0edd4", fill = "#f0edd4")) +
  labs(title = "Gustave Courbet",
       subtitle = "The space allocated to the artist Gustave Courbet in every edition of\nJanson's History of Art books relative to the average for all artists.\n",
       y = "Book Editions\n",
       x = "\nSpace per Page Ratio",
       caption = caption)

  ggsave("art-history-v2.png", width = 9, height = 9)

  
  # v1 "#DCA65C"
  # v2 "#B4632F"