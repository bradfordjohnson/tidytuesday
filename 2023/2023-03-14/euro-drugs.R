# inspiration from and credits to @danoehm and @BjnNowak on Twitter | they make amazing content!

# load packages
pacman::p_load(tidyverse,
               ggpath,
               ggtext,
               showtext,
               htmltools)

showtext_auto()

# load data
drugs <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-03-14/drugs.csv')

local_image_path <- "pill.png"
# wrangle data

## -- wrangle code by: Dan Oehm -- ##
## github: doehm
n_active_sub <- drugs |>
  count(common_name) |>
  arrange(desc(n)) |>
  slice_head(n = 100)

n_area <- drugs |>
  count(therapeutic_area) |>
  arrange(desc(n)) |>
  drop_na() |>
  slice_head(n = 100)

full_grid <- expand_grid(
  common_name = n_active_sub$common_name,
  therapeutic_area = n_area$therapeutic_area
)

df_count <-drugs |>
  semi_join(n_active_sub, by = "common_name") |>
  semi_join(n_area, by = "therapeutic_area") |>
  count(common_name, therapeutic_area)

df_base <- full_grid |>
  left_join(df_count, by = c("common_name", "therapeutic_area")) |>
  mutate(n = replace_na(n, 0)) |>
  arrange(desc(n), common_name, therapeutic_area) |>
  mutate(
    x = as.numeric(factor(common_name)),
    y = as.numeric(factor(therapeutic_area))
  ) |>
  filter(n > 0)

df_allergies <- drugs |>
  filter(str_detect(tolower(therapeutic_area), "allergi")) |>
  count(active_substance) |>
  mutate(
    active_substance = str_to_title(active_substance),
    active_substance = factor(active_substance),
    active_substance = fct_reorder(active_substance, n, max)
  ) |>
  arrange(active_substance)

df_allergies <- map_dfr(df_allergies$active_substance, ~{
  tibble(
    active_substance = .x,
    x = 1:df_allergies$n[df_allergies$active_substance == .x]
  )
})

df_allergies <- df_allergies |>
  mutate(path = local_image_path)
## -- -- ##

# visualize
## load fonts
font_add(family = "fb",
         regular = "Font Awesome 6 Brands-Regular-400.otf")

font_add_google(name = "Rubik", family = "Rubik")

font1 <- "Rubik"

font_add_google(name = "Mukta", family = "Mukta")

font2 <- "Mukta"

font_add_google(name = "Dosis", family = "Dosis")

# load caption
caption = paste0("<span style='font-family:fb;'>&#xf09b;</span>",
                 "<span style='font-family:sans;color:#99AEBB;'>.</span>",
                 "<span style='font-family:Dosis;'>bradfordjohnson | TidyTuesday - 2023 Week 11</span>")

## colors
bg_color <- "#99AEBB"
text_color_1 <- "#354A54"
text_color_2 <- "#005074"
text_color_3 <- "#A53D71"

ggplot(df_allergies,aes(x = fct_rev(active_substance), y = x)) +
  geom_from_path(aes(path = path), width = .03) +
  theme_void() +
  labs(title = "European Allergy Meds",
       subtitle = "<span style='color:#A53D71;'>Desloratadine</span> is the most common ingredient of the <span style='color:#A53D71;'>14 allergy medications</span>",
       caption = caption) +
  scale_x_discrete(labels = scales::wrap_format(10)) +
  theme(
    plot.background = element_rect(fill = bg_color,
                                   colour = bg_color),
    panel.background = element_rect(fill = bg_color,
                                    colour = bg_color),
    plot.title = element_text(color = text_color_2, family = font1,
                              size = 80, margin = margin(3,0,0,0,"mm")),
    plot.subtitle = ggtext::element_textbox_simple(color = text_color_1, family = font2,
                                 size = 45, margin = margin(4,0,4,0,"mm")),
    axis.text.x = element_text(color = c(text_color_3, text_color_1, text_color_1, text_color_1, text_color_1), family = font2,
                               size = 26, margin = margin(0,0,0,0,"mm")),
    plot.caption = ggtext::element_textbox_simple(color= text_color_1, size = 30, lineheight = 1 ,margin = margin(2,0,0,0,"mm")),
    plot.margin = unit(c(3,3,3,3),"mm")
  )

ggsave("euro-drugs.png", width = 9, height = 9)
