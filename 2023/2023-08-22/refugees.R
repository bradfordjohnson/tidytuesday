pacman::p_load(tidyverse, htmltools, showtext, jsonlite)

source("functions/caption.r")

showtext_auto()
showtext_opts(dpi = 300)

font_add(family = "fb", regular = "assets/fontAwesome.otf")

font_add_google(name = "Roboto", family = "Roboto")
font_1 <- "Roboto"

colors_json <- fromJSON("assets/colors.json")

background_color <- "#ffffff"

text_color <- colors_json$nord$polar_night$nord1

color_pal <- c(colors_json$nord$frost$nord10, colors_json$nord$aurora$nord11, colors_json$nord$snow_storm$nord4, colors_json$nord$aurora$nord12)

caption <- create_caption(2023, 34, background_color, font = font_1)

title <- "<span>Refugee Trends:<span style='color:#D08770;'> USA </span>vs. Neighbors (Euclidean Distance)</span>"

population <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-08-22/population.csv")

refugees <- population %>%
    group_by(year, coa_name) %>%
    summarise(refugees = sum(refugees))

refugees_pivoted <- refugees %>%
    pivot_wider(names_from = year, values_from = refugees) %>%
    drop_na()

usa_array <- as.numeric(unlist(refugees_pivoted[refugees_pivoted$coa_name == "United States of America", -1]))

euclidean_distance <- function(vector_1, vector_2) {
    if (length(vector_1) != length(vector_2)) {
        stop("Vectors must have the same length.")
    }
    sum_of_squared_differences <- sum((vector_1 - vector_2)^2)
    result <- sqrt(sum_of_squared_differences)
    return(result)
}

euclidean_distance_df <- function(df, vector) {
    distances <- apply(df[, -1], 1, function(row) euclidean_distance(row, vector))
    df$distance <- distances
    return(df)
}

selected_cols <- refugees_pivoted[, -c(1)]

refugees_pivoted_with_distances <- euclidean_distance_df(refugees_pivoted, usa_array)

refugees_long_with_distances <- refugees_pivoted_with_distances %>%
    arrange(distance) %>%
    head(7) %>%
    pivot_longer(cols = -c(coa_name, distance), names_to = "year", values_to = "refugees") %>%
    ungroup() %>%
    mutate(color_flag = case_when(
        coa_name == "United States of America" ~ "USA",
        coa_name == "China" ~ "China",
        coa_name == "France" ~ "France",
        TRUE ~ "Other"
    ))

refugees_long_with_distances %>%
    ggplot(aes(x = year, y = refugees, group = coa_name, color = color_flag)) +
    geom_line() +
    labs(caption = caption, y = "Refugee Count", x = "", title = title) +
    theme_void() +
    scale_color_manual(values = color_pal) +
    scale_y_continuous(labels = scales::comma) +
    theme(
        plot.background = element_rect(fill = background_color, color = background_color),
        panel.background = element_rect(fill = background_color, color = background_color),
        plot.caption = ggtext::element_textbox_simple(
            margin = unit(c(6, 0, 0, 0), "mm"),
            halign = 1,
            color = text_color,
            size = 5
        ),
        plot.title = ggtext::element_textbox_simple(
            family = font_1,
            margin = margin(0, 0, 2, 0, "mm"),
            halign = .1,
            size = 14,
            color = text_color
            
        ),
        plot.margin = margin(5, 5, 5, 5, 'mm'),
        legend.title = element_blank(),
        axis.text = element_text(color = text_color, family = font_1, size = 7),
        axis.title.y = element_text(color = text_color, family = font_1, size = 7, angle = 90, margin = margin(0, 5, 0, 1, "mm")),
        legend.text = element_text(color = text_color, family = font_1, size = 7),
    )

ggsave("2023/2023-08-22/refugees.png")
