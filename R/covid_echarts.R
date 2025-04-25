library(echarts4r)
library(dplyr)
library(readr)

date <- as.Date("2021-12-12")

# Colors ------------------------------------------------------------------
pal <- c("other" = "#068C8b", "bw" = "#C8102E", "de" = "#007dcc")

caption_msa <- "Data: Ministry of Social Affairs, Health and Integration **\u2022** Illustration: Stefan Moog"
caption_rki <- "Data: Robert-Koch-Institut **\u2022** Illustration: Stefan Moog"

# Read data ---------------------------------------------------------------
covid_bw <- read_csv(glue::glue("data-raw/csv/covidbw_{date}.csv")) %>%
  mutate(fill = case_when(
    grepl("Würt", district) ~ "bw",
    grepl("^Deu", district) ~ "de",
    TRUE ~ "other"
  )) %>%
  rename(region = district)

e_my_tooltip_formatter <- function(style = c("decimal", "percent", "currency"), digits = 0) {
  locale <- "en"
  style <- match.arg(style)
  opts <- list(style = style, minimumFractionDigits = digits,
               maximumFractionDigits = digits)
  tip <- htmlwidgets::JS(sprintf("function(params, ticket, callback) {
                                    var fmt = new Intl.NumberFormat('%s', %s);
                                    var idx = 0;
                                    return params.name[0] + '<br>' + params.marker + ' ' + params.seriesName[0] + ': ' + fmt.format(parseFloat(params.value[idx]));
                                  }",
                                 locale, jsonlite::toJSON(opts, auto_unbox = TRUE)))
  tip <- structure(tip, class = c("JS_EVAL", "item_formatter"))
  return(tip)
}

covid_bw |>
  mutate(region = reorder(region, fullq)) |>
  arrange(region) %>%
  e_charts(region) |>
  e_bar(partq, name = "Partly vaccinated", barWidth = '60%', barGap = '-100%', showBackground = TRUE, color = "#068C8b",
        emphasis = list(focus = "self")) |>
  e_bar(fullq, name = "Fully vaccinated", barWidth = '60%', showBackground = TRUE, color = "#068C8b9A",
        emphasis = list(focus = "self")) |>
  e_flip_coords() |>
  e_x_axis(formatter = e_axis_formatter(style = "percent"), position = "top", max = 1) %>%
  e_y_axis(axisTick = list(show = FALSE)) %>%
  e_tooltip(trigger = "axis",
            #formatter = e_my_tooltip_formatter(style = "percent", digit = 1)
            ) %>%
  e_text_style(fontFamily = "Roboto Condensed")
