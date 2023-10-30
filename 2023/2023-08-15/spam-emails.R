pacman::p_load(tidyverse,
               caret,
               randomForest,
               pROC,
               htmltools,
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
  "<span style='font-family:sans;color:#ECEFF4;'>.</span>",
  "<span style='font-family:Roboto;'>bradfordjohnson | TidyTuesday - 2023 Week 33</span>"
)

title <- paste0(
  "<span>Spam Email Classification: Confusion Matrix</span>"
)

bg_color <- "#ECEFF4"
text_color <- "#2E3440"
text_color_2 <- "#ECEFF4"

spam <-
  readr::read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-08-15/spam.csv'
  )

data_selected <- spam %>%
  select(crl.tot, dollar, bang, money, n000, make, yesno) %>%
  mutate(yesno = case_when(yesno == "n" ~ "Not Spam",
                           yesno == "y" ~ "Spam"))

set.seed(123)
trainIndex <-
  createDataPartition(data_selected$yesno, p = 0.7, list = FALSE)
training_data <- data_selected[trainIndex,]
testing_data <- data_selected[-trainIndex,]

training_data$yesno <- as.factor(training_data$yesno)
testing_data$yesno <- as.factor(testing_data$yesno)

model <- randomForest(yesno ~ ., data = training_data)

predictions <- predict(model, newdata = testing_data)

conf_matrix <- confusionMatrix(predictions, testing_data$yesno)

confusion_df <- as.data.frame(conf_matrix$table)

ggplot(data = confusion_df, aes(x = Reference, y = Prediction)) +
  geom_tile(aes(fill = Freq)) +
  geom_text(aes(label = Freq), vjust = 1, color = text_color_2) +
  scale_fill_gradient(low = "#5E81AC", high = "#D08770") +
  labs(title = title, x = "Actual", y = "Predicted", caption = caption, fill = "Frequency") +
  theme_void() +
  scale_x_discrete(position = "top") +
  theme(
    plot.background = element_rect(fill = bg_color, color = bg_color),
    panel.background = element_rect(fill = bg_color, color = bg_color),
    plot.margin = unit(c(10, 8, 10, 6), "mm"),
    plot.caption = ggtext::element_textbox_simple(
      margin = unit(c(6, 0, 0, 0), "mm"),
      halign = 1,
      color = text_color,
      size = 5
    ),
    plot.title = ggtext::element_textbox_simple(
      family = font_1,
      margin = unit(c(0, 0, 5, 0), "mm"),
      halign = .5,
      size = 15,
      color = text_color
    ),
    axis.text = element_text(
      family = font_2,
      color = text_color,
      size = 10
    ),
    legend.text = element_text(
      family = font_2,
      color = text_color,
      size = 6
    ),
    legend.title = element_text(
      family = font_2,
      color = text_color,
      size = 8,
      margin = unit(c(0, 2, 0, 0), "mm"),
    ),
    legend.position = "bottom",
  )

ggsave("spam-emails.png", width = 6, height = 6, dpi = 300)