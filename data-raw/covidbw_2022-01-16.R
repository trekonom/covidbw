library(tidyverse)
library(pdftools)

date <- as.Date("2022-01-16")

url <- "https://sozialministerium.baden-wuerttemberg.de/fileadmin/redaktion/m-sm/intern/downloads/Downloads_Gesundheitsschutz/Corona_Gesamtzahl-Impfungen-Landkreise-BW.pdf"
fn <- glue::glue("data-raw/pdf/covidbw_{date}.pdf")
download.file(url, fn)

covidbw <- pdf_data(fn)[[1]]

covidbw <- covidbw %>%
  filter(y >= 94, y < 450) %>%
  mutate(y1 = y, y = y %/% height, is_name = !grepl("^\\d", text)) %>%
  group_by(y) %>%
  mutate(text1 = ifelse(is_name, paste(text[x < 152], collapse = " "), text)) %>%
  distinct(text1, .keep_all = TRUE) %>%
  ungroup() %>%
  mutate(col = rep(seq(nrow(.) / 43), each = 43),
         row = rep(seq(43), nrow(.) / 43)) %>%
  select(row, col, text1) %>%
  pivot_wider(names_from = col, values_from = text1) %>%
  select(-row) %>%
  rename(district = 1, pop = 2, part = 3, full = 4, boost = 5, partq = 6, fullq = 7, boostq = 8) %>%
  mutate(across(c(2:8), readr::parse_number, locale = readr::locale(decimal_mark = ",", grouping_mark = "."))) %>%
  janitor::adorn_totals(name = "Baden-Württemberg") %>%
  mutate(across(c(part, full, boost), ~ .x / pop, .names = "{.col}q"))


write_csv(covidbw, glue::glue("data-raw/csv/covidbw_{date}.csv"))
