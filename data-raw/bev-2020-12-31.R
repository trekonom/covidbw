library(readr)
library(destapir)

bev <- table_ffcsv("12411-01-01-4", start = 2020, end = 2020, db = "rdb", job = TRUE, browse = F)

bev <- bev %>%
  janitor::clean_names() %>%
  select(-starts_with("statistik"), -zeit_code, -zeit_label, -contains("_merkmal")) %>%
  select(year = zeit, code = 2, region = 3, sex = 5, value = last_col()) %>%
  mutate(value = as.numeric(value)) %>%
  filter(grepl("^Insg", sex), !is.na(value)) %>%
  select(-sex)

readr::write_excel_csv(bev, "data-raw/csv/bev-2020-12-31.csv")



