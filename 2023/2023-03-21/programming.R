# load packages
pacman::p_load(tidyverse,
               camcorder,
               showtext,
               htmltools)

showtext_auto()
# load data
languages <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-03-21/languages.csv')

# explore

# languages |>
#   filter(is_open_source == TRUE) |>
#   group_by(type) |>
#   count() |>
#   arrange(desc(n)) |>
#   print(n=23)
# 
# languages |>
#   filter(is_open_source == TRUE) |>
#   group_by(github_language_type) |>
#   count()
#   
# languages |>
#   filter(is_open_source == TRUE) |>
#   select(title, github_language_repos) |>
#   arrange(desc(github_language_repos))

# top_tokens <- languages |>
#   filter(features_has_line_comments == TRUE) |>
#   group_by(line_comment_token) |>
#   summarise(jobs = sum(number_of_jobs), users = sum(number_of_users), n = n()) |>
#   drop_na() |>
#   arrange(desc(n)) |>
#   head(6)
# 
# 
#   top_tokens |>
#     ggplot(aes(x = fct_infreq(line_comment_token, users), y = users, label = line_comment_token)) +
#     geom_col() +
#     geom_text(aes(y = users / 2), size = 6)
#   
#   top_tokens |>
#     ggplot(aes(x = users, y = jobs, label = line_comment_token)) +
#     geom_text(aes(color = n), size = 10)

# start recording
gg_record(
  dir = file.path("2023", "2023-03-21", "recording"), # where to save the recording
  device = "png", # device to use to save images
  width = 6, # width of saved image
  height = 6, # height of saved image
  units = "in", # units for width and height
  dpi = 300 # dpi to use when saving image
)

## wrangle
type_fil <- c("textMarkup", "schema", "queryLanguage", "pl", "grammarLanguage", "dataNotation", "assembly")

tokens <- languages |>
  filter(features_has_line_comments == TRUE & type %in% type_fil)

tokens$type <- gsub('([[:upper:]])', ' \\1', tokens$type)
tokens$type <- str_to_title(tokens$type)
tokens$type <- replace(tokens$type,tokens$type=="Pl","PL")

tokens <- tokens |>
  mutate(is_r = ifelse(title == "R", "y", "n"))

# colors
colors <- c("#DACFDD", "#ED9A01")
bg <- "#36374c"
lines <- "#2B2C3D"
text_1 <- "#CFD1CC"
text_2 <- "#cfd1cc"

# fonts
font_add(family = "fb",
         regular = "C:/Users/Bradf/AppData/Local/Microsoft/Windows/Fonts/Font Awesome 6 Brands-Regular-400.otf")

font_add_google(name = "Ubuntu", family = "Ubuntu")
font_1 <- "Ubuntu"

font_add_google(name = "Dosis", family = "Dosis")

# labs
caption = paste0("<span style='font-family:fb;'>&#xf09b;</span>",
                 "<span style='font-family:sans;color:#36374c;'>.</span>",
                 "<span style='font-family:Dosis;'>bradfordjohnson | TidyTuesday - 2023 Week 12</span>")

title = paste0("<span>Code comment tokens</span>")
subtitle = paste0("There are **517** total languages that allow line comments, with **22** unique comment tokens. ",
                  "Shown is a subset of different programming languages and respective release years. The tokens are grouped by language types. ",
                  "<span style='color:#ED9A01;'>Highlighted</span> is the comment token for <span style='color:#ED9A01;'>R</span>, **the very language used to make this visual!**")

## visualize data
  tokens |>
    group_by(line_comment_token) |>
    ggplot(aes(x = appeared, y = fct_infreq(type), label = line_comment_token)) +
    geom_text(aes(color = is_r), check_overlap = TRUE, size = 10, family = font_1) +
    theme_minimal() +
    scale_color_manual(values = colors) +
    labs(caption = caption, title = title, subtitle = subtitle) +
    
    theme(axis.title = element_blank(),
          axis.text = element_text(color = text_1, family = font_1, size = 25, face = "bold"),
          axis.text.y = element_text(size = 20, hjust = 1),
          panel.grid.major = element_line(colour = lines, linewidth = .1),
          panel.grid.minor = element_blank(),
          panel.grid.major.y = element_line(linewidth = .1),
          plot.background = element_rect(color = bg, fill = bg),
          panel.background = element_rect(color = bg, fill = bg),
          legend.position = "none",
          plot.caption = ggtext::element_textbox_simple(color = text_2, margin = margin(3,0,0,0,"mm"), size = 15, halign = -.2),
          plot.title = ggtext::element_textbox_simple(color = text_1, size = 50, family = font_1, margin = margin(2,0,1,0,"mm"), halign = .9),
          plot.subtitle = ggtext::element_textbox_simple(color = text_1, size = 28, margin = margin(3,0,1,0,"mm"), lineheight = .5, halign = 0),
          plot.margin = unit(c(1,1,1,1), "mm")
          )
  
# save gif
gg_playback(
    name = file.path("2023", "2023-03-21","20230321.gif"),
    first_image_duration = 4,
    last_image_duration = 20,
    frame_duration = .15,
    background = "#F0F5F5"
  )
  
ggsave("programming.png", width = 9, height = 9)
