library(tidyverse)
library(pdftools)

date <- as.Date("2021-12-12")

url <- "https://sozialministerium.baden-wuerttemberg.de/fileadmin/redaktion/m-sm/intern/downloads/Downloads_Gesundheitsschutz/Corona_Gesamtzahl-Impfungen-Landkreise-BW.pdf"
fn <- glue::glue("data-raw/pdf/covidbw_{date}.pdf")
download.file(url, fn)

covidbw <- pdf_data(fn)[[1]]

covidbw <- covidbw %>%
  filter(y >= 94, y < 450) %>%
  mutate(y = y %/% height) %>%
  group_by(y) %>%
  mutate(text1 = ifelse(!grepl("^\\d", text), paste(text[x < 152], collapse = " "), text)) %>%
  distinct(text1, .keep_all = TRUE) %>%
  mutate(id = row_number()) %>%
  ungroup() %>%
  select(id, y, text1) %>%
  pivot_wider(names_from = id, values_from = text1) %>%
  select(-y) %>%
  rename(district = 1, pop = 2, part = 3, full = 4, boost = 5, partq = 6, fullq = 7, boostq = 8) %>%
  mutate(across(c(2:8), readr::parse_number, locale = readr::locale(decimal_mark = ",", grouping_mark = "."))) %>%
  janitor::adorn_totals(name = "Baden-Württemberg") %>%
  mutate(across(c(part, full, boost), ~ .x / pop, .names = "{.col}q"))


write_csv(covidbw, glue::glue("data-raw/csv/covidbw_{date}.csv"))
