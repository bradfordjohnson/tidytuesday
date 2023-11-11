create_caption <- function(year_number, week_number, background_color, font = font_1) {
    icon <- "<span style='font-family:fb;'>&#xf09b;</span>"
    spacer <- glue::glue("<span style='font-family:sans;color:{background_color};'>.</span>")
    week <- glue::glue("<span style='font-family:{font};'>bradfordjohnson | TidyTuesday - {year_number} Week {week_number}</span>")

    return(paste0(
        icon,
        spacer,
        week
    ))
}