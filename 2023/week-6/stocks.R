# load packages
library(tidyverse)
library(showtext)
library(htmltools)
library(ggtext)

showtext_auto()
# load data
big_tech_stock_prices <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-07/big_tech_stock_prices.csv')
big_tech_companies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-07/big_tech_companies.csv')

# wrangle data
big_tech_stock_prices <- big_tech_stock_prices |>
  left_join(big_tech_companies, by = c("stock_symbol" = "stock_symbol"))

big_tech_stock_prices$date <- lubridate::ymd(big_tech_stock_prices$date)

stock_filter <- c("TSLA")

# load fonts
font_add(family = "fb",
         regular = "C:/Users/Bradf/AppData/Local/Microsoft/Windows/Fonts/Font Awesome 6 Brands-Regular-400.otf")
font_add(family = "Mulish",
         regular = "C:/Users/Bradf/AppData/Local/Microsoft/Windows/Fonts/Mulish-Regular.ttf")
font_add(family = "MulishB",
         regular = "C:/Users/Bradf/AppData/Local/Microsoft/Windows/Fonts/Mulish-Bold.ttf")
font_add_google("Open Sans", "Open Sans")
font_add_google(name = "Ubuntu", family = "Ubuntu")
font <- "Ubuntu"

# caption
caption = paste0(
  "<span style='font-family:fb;color:#7A7973;'>&#xf09b;</span>", "<span style='font-family:sans;color:#FFFFFF;'>.</span>",
  "<span style='font-family:Ubuntu;color:#7A7973;'>bradfordjohnson  | TidyTuesday - 2023 Week 6</span>"
)

# visualize data
col_pal <-c("#3d647a")
col_pal2 <-c("#6c5a98")

stocks <- big_tech_stock_prices |>
  filter(stock_symbol %in% stock_filter)

stocks_gg <- stocks |>
    ggplot(aes(x = date, y = high, color = company)) +
      geom_line() +
      geom_area(mapping = aes(x = date, y = volume / -10000000), fill = col_pal2, color = col_pal2, alpha = 1) +
      scale_y_continuous(labels = function(x) paste0("$", x),
        breaks = seq(0, 400, len = 5)) +
      scale_color_manual(values = col_pal) +
      theme_minimal()

stocks_lab <- stocks_gg +
  labs(
    title = "Daily highs falling for Tesla",
    subtitle = "Along with the falling highs in blue we can see the volume of trades in purple.",
    x = "",
    y = "Daily high / share",
    caption = caption
  )

stocks_theme <- stocks_lab +
  theme(
      plot.title = element_text(family = font, face = "bold", hjust = 0.5, vjust = 0, size = 58),
      plot.subtitle = element_text(family = "Mulish", hjust = 0.5, size = 34),
      plot.caption = ggtext::element_textbox_simple(size = 26),
      plot.margin = unit(c(10,20,10,7), "pt"),
      axis.title.y = element_text(family = font, face = "bold", margin = margin(0,3,0,0, unit = "mm"), size = 34),
      axis.text.y = element_text(family = font, color = "black", size = 26),
      axis.text.x = element_text(family = font, color = "black", size = 26),
      panel.grid.major = element_line(colour = "gray90"),
      panel.grid.minor = element_blank(),
      legend.position = "none",
      text = element_text(family = font),
      panel.background = element_blank(),
      plot.background = element_rect(fill = "#F3F5F6", color = "#F3F5F6"))

stocks_arrow <- stocks_theme +
  annotate(geom = "curve", 
           x = lubridate::ymd("2010-04-01"), 
           y = -80, 
           xend = lubridate::ymd("2010-04-01"), 
           yend = -30, 
           linewidth = 0.8,
           colour = "black",
           curvature = -0.5,
           arrow = arrow(length = unit(1.5, "mm"), type = "closed")
        )
stocks_arrow +
  geom_label(mapping = aes(
              x = lubridate::ymd("2011-02-10"), y = -80,
              label = "Volume Traded"),
              size = 11,
              family = "Ubuntu",
              colour = "#6c5a98",
              label.size = NA,
              fill = "#F3F5F6"
  )

ggsave("stocks-v2.png", width = 9, height = 9)
