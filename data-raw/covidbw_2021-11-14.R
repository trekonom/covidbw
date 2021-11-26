library(tidyverse)

date <- as.Date("2021-11-14")

url <- "https://sozialministerium.baden-wuerttemberg.de/fileadmin/redaktion/m-sm/intern/downloads/Downloads_Gesundheitsschutz/Corona_Gesamtzahl-Impfungen-Landkreise-BW.pdf"
#download.file(url, glue::glue("data-raw/pdf/covidbw_{date}.pdf"))

part <- c(128.746, 41.936, 126.497, 249.294, 145.273, 178.704, 100.046, 117.347, 120.888, 343.195, 166.443, 79.926, 165.314, 112.31, 78.616, 220.629, 81.508, 70.096, 300.828, 203.351, 186.329, 149.853, 345.046, 88.249, 206.518, 92.107, 282.8, 206.68, 73.813, 151.219, 182.226, 268.316, 189.168, 373.572, 88.138, 118.382, 131.604, 80.465, 400.22, 156.005, 84.767, 91.4, 108.425, 116.089)
full <- c("125.473 65,0 63,3 40.741 75,6 73,5 123.597 62,5 61,1 242.714 63,5 61,8 141.039 66,7 64,7 173.121 67,5 65,4 97.209 62,5 60,7 114.141 70,3 68,4 118.146 60,5 59,1 335.223 64,3 62,8 161.693 72,1 70,0 78.575 67,5 66,4 162.379 63,9 62,7 107.873 70,8 68,0 76.968 59,2 58,0 214.576 63,7 62,0 80.786 64,5 63,9 67.935 62,2 60,2 294.733 67,3 66,0 197.118 65,9 63,9 181.248 65,0 63,2 146.252 65,5 63,9 338.524 63,3 62,1 87.900 66,5 66,2 199.851 66,7 64,5 91.199 64,1 63,4 272.154 65,4 62,9 198.641 65,8 63,2 70.546 58,6 56,0 148.271 65,2 63,9 176.123 63,7 61,6 262.973 62,8 61,5 183.936 65,8 64,0 364.435 68,1 66,5 86.557 62,9 61,8 117.543 59,8 59,4 128.431 61,8 60,3 77.778 61,4 59,4 387.999 63,5 61,6 152.766 68,3 66,9 82.913 59,8 58,5 88.200 72,3 69,8 106.978 63,3 62,5 112.630 61,1 59,3")
full <- str_extract_all(full, pattern = "\\d+\\.\\d+ \\d+,\\d \\d+,\\d")[[1]]

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

#write_csv(covid_bw, glue::glue("data-raw/csv/covidbw_{date}.csv"))
