library(tidyverse)
library(pdftools)

date <- as.Date("2022-05-02")

url <- "https://sozialministerium.baden-wuerttemberg.de/fileadmin/redaktion/m-sm/intern/downloads/Downloads_Gesundheitsschutz/Corona_Gesamtzahl-Impfungen-Landkreise-BW.pdf"
fn <- glue::glue("data-raw/pdf/covidbw_{date}.pdf")
download.file(url, fn)

covidbw <- pdf_data(fn)[[1]]

y_start <- covidbw$y[grepl("^Alb", covidbw$text)]
y_end <- covidbw$y[grepl("^Zoll", covidbw$text)]

n_kreise <- 44
covidbw <- covidbw %>%
  filter(y >= y_start, y <= y_end) %>%
  mutate(y1 = y, y = y %/% height, is_name = !grepl("^\\d", text)) %>%
  group_by(y) %>%
  mutate(text1 = ifelse(is_name, paste(text[is_name], collapse = " "), text),
         is_dup = duplicated(text1)) %>%
  ungroup() %>%
  filter(!(is_dup & is_name)) %>%
  mutate(col = rep(seq(nrow(.) / n_kreise), each = n_kreise),
         row = rep(seq(n_kreise), nrow(.) / n_kreise)) %>%
  select(row, col, text1) %>%
  pivot_wider(names_from = col, values_from = text1) %>%
  select(-row) %>%
  rename(district = 1, pop = 2, part = 3, full = 4, boost = 5, partq = 6, fullq = 7, boostq = 8) %>%
  mutate(across(c(2:8), readr::parse_number, locale = readr::locale(decimal_mark = ",", grouping_mark = "."))) %>%
  janitor::adorn_totals(name = "Baden-Württemberg") %>%
  mutate(across(c(part, full, boost), ~ .x / pop, .names = "{.col}q")) %>%
  mutate(district = str_remove_all(district, "\\*"),
         district = str_trim(district))


write_csv(covidbw, glue::glue("data-raw/csv/covidbw_{date}.csv"))
