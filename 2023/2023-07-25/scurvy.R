# inspired by nrennie (github) I want to make a similar visual for the scurvy data from scratch

pacman::p_load(tidyverse,
               ggpath,
               showtext,
               htmltools)

showtext_auto()
showtext_opts(dpi = 300)

font_add(family = "fb", regular =
           "C:/Users/Bradf/AppData/Local/Microsoft/Windows/Fonts/Font Awesome 6 Brands-Regular-400.otf")

font_add_google(name = "Roboto Slab", family = "Roboto Slab")
font_1 <- "Roboto Slab"

font_add_google(name = "Roboto", family = "Roboto")
font_2 <- "Roboto"

caption <- paste0(
  "<span style='font-family:fb;'>&#xf09b;</span>",
  "<span style='font-family:sans;color:#F5F9F5;'>.</span>",
  "<span style='font-family:Roboto;'>bradfordjohnson | TidyTuesday - 2023 Week 30</span>"
)

title <- paste0(
  "<span style='font-family:Roboto;'><span style='color:#ff754b;'>1 </span><span>of 12 sailors fit for duty - after 6 day scurvy treatment</span></span>"
)

scurvy <-
  readr::read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-25/scurvy.csv'
  ) %>%
  janitor::clean_names()

colnames(scurvy) <- sub("_d6$", "", colnames(scurvy))

remove_non_numeric <- function(df, columns_to_clean) {
  #' Removes non-numeric characters from a column
  #'
  #' @param df A dataframe
  #' @param columns_to_clean A vector of column names to clean
  #' @return A dataframe with cleaned columns
  #' @export
  
  for (col_name in columns_to_clean) {
    df[[col_name]] <- as.numeric(gsub("[^0-9]", "", df[[col_name]]))
  }
  return(df)
}

scurvy <-
  remove_non_numeric(
    scurvy,
    c(
      "gum_rot",
      "skin_sores",
      "weakness_of_the_knees",
      "lassitude",
      "fit_for_duty"
    )
  )

scurvy$treatment <- gsub("_", " ", scurvy$treatment)

anchor <- "anchor.png"

visual_df <- data.frame(id = 1:12)

visual_df <- visual_df %>%
  mutate(x = row_number() %% 3, y = row_number() %% 4, path = anchor) %>%
  mutate(x = case_when(x == 0 ~ 3,
                       .default = x),
         y = case_when(y == 0 ~ 4,
                       .default = y)) %>%
  mutate(color_flag = case_when(x == 1 & y == 4 ~ 1,
                   .default = 0))

text_col <- "#000000"

bg_color <- "#FBF4E5"

visual_df %>%
  ggplot(aes(x = x, y = y)) +
  geom_from_path(aes(path = path, color = factor(color_flag)), width = .1, height = .15) +
  scale_x_continuous(limit = c(.5, 3.5)) +
  scale_y_continuous(limit = c(.5, 4.5)) +
  scale_color_manual(values = c("#16569c", "#ff754b")) +
  labs(title = title,
       caption = caption) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = bg_color, color = bg_color),
    panel.background = element_rect(fill = bg_color, color = bg_color),
    plot.margin = margin(10, 10, 10, 10, 'mm'),
        plot.caption = ggtext::element_textbox_simple(
          margin = margin(6, 0, 0, 0, 'mm'),
          halign = 1,
          color = text_col,
          size = 8,
        ),
        plot.title = ggtext::element_textbox_simple(
          family = font_1, 
          margin = margin(0, 0, 0, 0),
          halign = .1,
          size = 20,
          color = text_col))

ggsave("scurvy.png")
