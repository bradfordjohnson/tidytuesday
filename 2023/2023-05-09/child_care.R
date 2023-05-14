pacman::p_load(tidyverse, usmap, patchwork, htmltools, showtext)

showtext_auto()
showtext_opts(dpi = 300)

childcare_costs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-09/childcare_costs.csv')
counties <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-09/counties.csv')

sc_counties <- counties |>
  filter(state_abbreviation == "SC")

sc_childcare_costs <- sc_counties |>
  inner_join(childcare_costs)

sc_childcare_costs <- sc_childcare_costs |>
  mutate(med_hh_inc_prop = mcsa / (mhi_2018 / 52)) |>
  filter(study_year %in% c(2014, 2018))

sc_childcare_costs <- sc_childcare_costs |>
  mutate(fips = county_fips_code)

font_add(family = "fb", regular = "C:/Users/Bradf/AppData/Local/Microsoft/Windows/Fonts/Font Awesome 6 Brands-Regular-400.otf")

font_add_google(name = "Inter", family = "Inter")
font_1 <- "Inter"

caption <- paste0("<span style='font-family:fb;'>&#xf09b;</span>",
                  "<span style='font-family:sans;color:#FEF9F4FF;'>.</span>",
                  "<span style='font-family:Inter;'>bradfordjohnson | TidyTuesday - 2023 Week 19</span>")


p_map <- plot_usmap("counties", include = c("SC"), data = sc_childcare_costs, values = "med_hh_inc_prop") +
viridis::scale_fill_viridis(option = "C", labels = scales::percent_format(accuracy = 1)) +
facet_wrap(~study_year) +
  theme(strip.background = element_blank(), strip.text = element_text(family = font_1, size = 5),
        legend.position = c(.45,.1),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.text = element_text(family = font_1, size = 3),
        legend.direction = "horizontal")

p_map
p_curve <- sc_childcare_costs |>
  ggplot(aes(x = med_hh_inc_prop, y = as.factor(study_year), fill = study_year)) +
  geom_point(aes(alpha = .7), shape = 8) +
  ggridges::geom_density_ridges(alpha = .5, scale = 1, rel_min_height = 0.005) +
  viridis::scale_fill_viridis(option = "F") +
  ggridges::theme_ridges() +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Weekly childcare costs in South Carolina",
       subtitle = "Proportion of median weekly income",
    x = "",
       y = "",
    caption = caption) +
  theme(legend.position = "none",
        plot.margin = unit(c(2,2,2,2), "mm"),
        plot.title = element_text(family = font_1, size = 10, margin = margin(2,2,2,2, "mm")),
        plot.caption = ggtext::element_textbox_simple(size = 4.5, halign = 1, margin = margin(2,2,2,2,"mm")),
axis.title.x = element_text(family = font_1, size = 8, margin = margin(1,0,0,0, "mm")),
plot.subtitle = ggtext::element_textbox_simple(family = font_1, margin = margin(2, 0, 2, 0, "mm"),
                                               size = 7),
axis.text = element_text(family = font_1, size = 6))

p_curve + inset_element(p_map, align_to = "panel", .4, .7, 1.2, 1.1)

ggsave("child_care2.png", height = 6, width = 6)
