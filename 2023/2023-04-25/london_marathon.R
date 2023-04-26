# load packages
pacman::p_load(tidyverse,
               camcorder,
               showtext,
               htmltools)

showtext_auto()
showtext_opts(dpi = 300)

# load data
tuesdata <- tidytuesdayR::tt_load(2023, week = 17)

london_marathon <- tuesdata$london_marathon |>
  janitor::clean_names()

# wrangle data
london_marathon <- london_marathon |>
  filter(year <= 2019) |>
  select(-c(raised, official_charity))

# explore data
sum(london_marathon$starters)
sum(london_marathon$finishers)
sum(london_marathon$finishers)/sum(london_marathon$starters)

# visualize
  ## camcorder
    gg_record(
      dir = file.path("2023", "2023-04-26", "recording"), # where to save the recording
      device = "png", # device to use to save images
      width = 6, # width of saved image
      height = 6, # height of saved image
      units = "in", # units for width and height
      dpi = 300 # dpi to use when saving image
    )
    
  ## fonts
    font_add(
      family = "fb", regular = "C:/Users/Bradf/AppData/Local/Microsoft/Windows/Fonts/Font Awesome 6 Brands-Regular-400.otf"
    )
    
    font_add_google(name = "Jost", family = "Jost")
    font_1 <- "Jost"
  
  ## labels and text
    caption <- paste0("<span style='font-family:fb;'>&#xf09b;</span>",
                      "<span style='font-family:sans;color:#FEF9F4FF;'>.</span>",
                      "<span style='font-family:Jost;'>bradfordjohnson | TidyTuesday - 2023 Week 17</span>")
    
    subtitle <- paste0("From 1981 to 2019, over <span style='color:#f7915b;'>1.1 million participants started</span>",
                       " the marathon, <span style='color:#1e4985;'>97% of participants finished</span>.")
  ## ggplot
    london_marathon |>
      ggplot() +
      geom_area(aes(x = year, y = starters / 1000), fill = "#f7915b") +
      geom_area(aes(x = year, y = finishers / 1000), fill = "#1e4985") +
      labs(x = "", y = "",
           title = "The London Marathon",
           subtitle = subtitle,
           caption = caption
      ) +
      scale_y_continuous(labels = function(x) paste0(x, "k")) +
      theme_minimal() +
      theme(
        panel.background = element_rect(fill = "#FEF9F4FF", color = "#FEF9F4FF"),
        plot.background = element_rect(fill = "#FEF9F4FF", color = "#FEF9F4FF"),
        axis.title = element_blank(),
        plot.title = element_text(family = font_1, size = 20),
        axis.text = element_text(family = font_1, size = 10, color = "black"),
        plot.caption = ggtext::element_textbox_simple(margin = margin(3, 0, 0, 0, "mm"),
                                                      halign = 1, color = "gray10"),
        plot.subtitle = ggtext::element_textbox_simple(family = font_1, margin = margin(0, 0, 2, 0, "mm"),
                                                       size = 12),
        panel.grid = element_line(color = "#e4e0db", linewidth = .2),
        panel.grid.minor = element_blank(),
        plot.margin = unit(c(2,2,2,2), "mm")
        )

    ## save gif
      gg_playback(
        name = file.path("2023", "2023-04-26","london_marathon.gif"),
        first_image_duration = 4,
        last_image_duration = 20,
        frame_duration = .12,
        background = "#FEF9F4FF"
      )
    
    ## save plot
      ggsave("london_marathon.png", width = 6, height = 6)
      