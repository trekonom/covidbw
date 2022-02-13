# Interactive Map ---------------------------------------------------------

library(dwextra)
library(DatawRappr)

date <- as.Date("2022-02-06")

covid_bw_dw <- read_csv(glue::glue("data-raw/csv/covidbw_{date}.csv"))  %>%
  mutate(across(ends_with("q"), ~ 100 * .x, .names = "{.col}1"),
         district = case_when(
           district == "Heilbronn, Land" ~ "Heilbronn Landkreis",
           district == "Heilbronn, Stadt" ~ "Heilbronn Stadt",
           district == "Karlsruhe, Land" ~ "Karlsruhe Landkreis",
           district == "Karlsruhe, Stadt" ~ "Karlsruhe Stadt",
           TRUE ~ district
         )) %>%
  filter(!grepl("^Baden-W", district))

dw_data_to_chart(covid_bw_dw, "7BugB")
dw_edit_chart("7BugB", intro = format(date, "%B %d, %Y"))
dw_publish_chart("7BugB")
