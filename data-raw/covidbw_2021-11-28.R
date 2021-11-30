library(tidyverse)

date <- as.Date("2021-11-28")

url <- "https://sozialministerium.baden-wuerttemberg.de/fileadmin/redaktion/m-sm/intern/downloads/Downloads_Gesundheitsschutz/Corona_Gesamtzahl-Impfungen-Landkreise-BW.pdf"
#download.file(url, glue::glue("data-raw/pdf/covidbw_{date}.pdf"))

part <- c(131.405, 42.724, 128.844, 252.257, 148.937, 182.115, 101.655, 119.233, 123.21, 348.917, 168.715, 82.199, 169.03, 113.859, 81.421, 224.707, 84.751, 71.628, 304.743, 206.131, 190.293, 152.197, 350.381, 90.354, 210.585, 93.337, 288.879, 211.319, 75.317, 154.22, 186.477, 273.376, 192.563, 377.901, 90.463, 120.999, 134.987, 82.289, 407.721, 158.276, 87.318, 93.577, 110.141, 117.338)

full <- c("127.538 66", "3 64", "3 41.371 77", "1 74", "6 125.097 63", "7 61", "9 245.210 64", "2 62", "4 144.058 68", "4 66", "1 175.207 68", "8 66", "1 98.580 63", "5 61", "6 115.309 71", "5 69", "1 119.935 61", "7 60", "0 339.556 65", "4 63", "6 163.535 73", "1 70", "8 79.941 69", "4 67", "5 165.000 65", "3 63", "8 109.223 71", "7 68", "8 78.262 61", "3 58", "9 217.415 64", "9 62", "8 83.085 67", "0 65", "7 68.920 63", "5 61", "1 298.332 68", "2 66", "8 199.493 66", "8 64", "7 184.354 66", "3 64", "3 148.001 66", "5 64", "7 342.846 64", "3 62", "9 89.051 68", "1 67", "1 203.547 68", "0 65", "7 92.142 64", "9 64", "1 275.987 66", "8 63", "8 201.698 67", "2 64", "2 71.776 59", "8 57", "0 150.957 66", "4 65", "0 178.665 65", "2 62", "5 266.741 64", "0 62", "4 186.298 67", "0 64", "8 367.881 68", "9 67", "1 88.714 64", "5 63", "3 119.284 61", "2 60", "3 130.773 63", "4 61", "4 78.858 62", "8 60", "2 393.546 64", "7 62", "4 154.685 69", "3 67", "7 84.222 61", "6 59", "4 89.560 74", "0 70", "9 108.380 64", "3 63", "3 113.831 61", "8 60", "0")
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
