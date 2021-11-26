library(tidyverse)

date <- as.Date("2021-11-21")

url <- "https://sozialministerium.baden-wuerttemberg.de/fileadmin/redaktion/m-sm/intern/downloads/Downloads_Gesundheitsschutz/Corona_Gesamtzahl-Impfungen-Landkreise-BW.pdf"
#download.file(url, glue::glue("data-raw/pdf/covidbw_{date}.pdf"))

part <- c(129.943, 42.258, 127.547, 250.336, 146.456, 179.662, 100.676, 118.165, 121.645, 344.835, 167.363, 80.564, 166.578, 112.92, 79.59, 222.234, 82.87, 70.455, 302.171, 204.591, 187.663, 150.663, 346.469, 88.951, 208.201, 92.505, 284.674, 208.444, 74.382, 152.265, 183.402, 270.098, 190.191, 375.321, 89.112, 119.174, 132.628, 80.996, 402.605, 156.754, 85.395, 92.21, 109.215, 116.473)
full <- c("126.748 65", "6 63", "9 41.036 76", "2 74", "0 124.353 63", "1 61", "5 243.882 63", "7 62", "1 142.345 67", "2 65", "3 174.037 67", "8 65", "7 97.744 62", "9 61", "0 114.763 70", "8 68", "8 118.853 60", "9 59", "5 337.002 64", "6 63", "2 162.620 72", "5 70", "4 79.054 68", "1 66", "8 163.549 64", "4 63", "2 108.587 71", "1 68", "4 77.503 59", "9 58", "4 215.934 64", "2 62", "3 81.897 65", "5 64", "8 68.285 62", "5 60", "6 296.198 67", "6 66", "3 198.373 66", "3 64", "3 182.722 65", "4 63", "7 147.021 65", "8 64", "2 340.009 63", "6 62", "4 88.386 67", "0 66", "6 201.771 67", "2 65", "1 91.628 64", "3 63", "7 273.802 65", "8 63", "3 199.901 66", "3 63", "6 71.047 59", "0 56", "4 149.424 65", "6 64", "4 177.224 64", "2 62", "0 264.683 63", "2 61", "9 184.929 66", "2 64", "3 366.148 68", "5 66", "8 87.517 63", "6 62", "4 118.291 60", "2 59", "8 129.347 62", "3 60", "8 78.289 61", "9 59", "8 390.295 63", "9 61", "9 153.638 68", "6 67", "2 83.401 60", "3 58", "9 88.813 72", "9 70", "3 107.683 63", "8 62", "9 113.185 61", "3 59", "6")
full <- str_extract_all(paste(full, collapse = ","), pattern = "\\d+\\.\\d+ \\d+,\\d \\d+,\\d")[[1]]

krspop <- read_rds("data-raw/rds/krspop.rds")

covidbw <- bind_cols(krspop, part = part, full = full) %>%
  extract(full, into = c("full", "partq", "fullq"), regex = "^(\\d+\\.\\d+) (\\d+,\\d) (\\d+,\\d)$") %>%
  mutate(across(everything(), as.character),
         across(-c(district, ends_with("q")), parse_number),
         across(-c(district, ends_with("q")), ~ .x * 1000),
         across(c(ends_with("q")), parse_number, locale = locale(grouping_mark = ".", decimal_mark = ","))) %>%
  janitor::adorn_totals(fill = "-", na.rm = TRUE, name = "Baden-Württemberg") %>%
  mutate(across(c(part, full), ~ .x / pop, .names = "{.col}q")) %>%
  mutate(district = str_replace(district, "o\\u0308", "ö"),
         district = str_replace(district, "a\\u0308", "ä"),
         district = str_replace(district, "u\\u0308", "ü"))

#write_csv(covidbw, glue::glue("data-raw/csv/covidbw_{date}.csv"))
