# load packages
library(tidyverse)

# load data
big_tech_stock_prices <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-07/big_tech_stock_prices.csv')
big_tech_companies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-07/big_tech_companies.csv')

# wrangle data
big_tech_stock_prices <- big_tech_stock_prices |>
  left_join(big_tech_companies, by = c("stock_symbol" = "stock_symbol"))

big_tech_stock_prices$date <- lubridate::ymd(big_tech_stock_prices$date)

stock_filter <- c("NVDA", "TSLA", "META", "MSFT")

# visualize data
## break the below code down into pieces, assign as obj and create isolated task steps such as adding the theme args in one task

big_tech_stock_prices |>
  filter(stock_symbol %in% stock_filter) |>
    ggplot(aes(x = date, y = high, color = company)) +
      geom_line() +
      geom_line(mapping = aes(x = date, y = volume / -10000000), alpha = .5) +
      scale_y_continuous(breaks = seq(0, 400, len = 5)) +
      facet_wrap(~company) +
      theme_minimal() +
      theme(legend.position = "none",
            axis.title.x = element_blank())
