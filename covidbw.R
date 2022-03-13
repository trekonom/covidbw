library(tidyverse)
library(prognos.geo)
require(ggtext)

source("R/covid_charts.R", encoding = "UTF-8")

date <- as.Date("2022-03-07")

# Theme -------------------------------------------------------------------
base_family <- "Roboto Condensed"

theme_set(theme_minimal(base_size = 11, base_family = base_family))

theme_update(
  plot.title = ggtext::element_markdown(margin = margin(b = 2, unit = "pt")),
  plot.title.position = "plot",
  plot.subtitle = ggtext::element_textbox_simple(size = 8, margin = margin(t = 0, b = 2, unit = "pt")),
  plot.caption = ggtext::element_markdown(size = 5, color = "grey45", lineheight = 1.4),
  strip.text.x = element_text(face = "bold"),
  axis.text.y.right = element_text(size = 8, hjust = 1, margin = margin(l = 2.2, unit = "pt")),
  axis.text.y.left = element_text(size = 8, hjust = 1, margin = margin(l = 2.2, r = 2.2, unit = "pt")),
  axis.ticks.length = unit(2.2, "pt"),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank()
)

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

url <- glue::glue("https://raw.githubusercontent.com/robert-koch-institut/COVID-19-Impfungen_in_Deutschland/master/Archiv/{date + lubridate::days(1)}_Deutschland_Impfquoten_COVID-19.csv")
covid_bl <- read_csv(url) %>%
  select(partq = "Impfquote_gesamt_min1", fullq = "Impfquote_gesamt_voll", region = Bundesland, code = BundeslandId_Impfort) %>%
  mutate(across(ends_with("q"), ~ .x / 100),
         fill = case_when(
           grepl("Würt", region) ~ "bw",
           grepl("^Deu", region) ~ "de",
           TRUE ~ "other"
         ))  %>%
  filter(!grepl("ressort", region))

# Bar Chart ---------------------------------------------------------------

width = .75

covid_vacc_bar(covid_bw) +
  labs(title = '<b>Share of people vaccinated against COVID-19 in *The Länd*</b>',
       subtitle = glue::glue('{format(date, "%b %d, %Y")}<br><br>',
                             'Note: Since January, 15 a vaccination with J&J is no longer counted as a full vaccination.',
                             'Due to reporting issues to take account of that an increasing number of counties reports a higher number of fully than',
                             'partly vaccinated people.', .sep = " "),
       caption = caption_msa)

ggsave(glue::glue("figure/covid_bar_bw_{date}.png"), width = 16, height = 24, units = "cm", bg = "white")

# Bar Chart States --------------------------------------------------------

covid_vacc_bar(covid_bl) +
  labs(title = '**Share of people vaccinated against COVID-19 in Germany**',
       subtitle = format(date, "%b %d, %Y"),
       caption = caption_rki)

ggsave(glue::glue("figure/covid_bar_states_{date}.png"), width = 16, height = 16, units = "cm", bg = "white")

# Map ---------------------------------------------------------------------

sf_covid_bw <- read_shp_krs(codes = "^08") %>%
  left_join(select(dictkrs, ars, label, type), by = "ars") %>%
  add_count(label) %>%
  mutate(region = case_when(
    n == 2 & grepl("^Stadt", type) ~ paste0(label, ", Stadt"),
    n == 2 & grepl("^Land", type) ~ paste0(label, ", Land"),
    TRUE ~ label)) %>%
  left_join(covid_bw, by = "region")

center_bw <- pgo_centroid(sf_covid_bw, .cols = c("region", "type", "fullq", "partq")) %>%
  filter(grepl("^Stadt", type))

center_bw$nudge_right <- center_bw$lat > 9 | grepl("^Pforz", center_bw$region)

covid_vacc_map(sf_covid_bw, center_bw, nudge_x = .75) +
  labs(title = '<b>Share of people fully vaccinated against COVID-19 in *The Länd*</b>',
       subtitle = format(date, "%b %d, %Y"),
       caption = caption_msa,
       fill = NULL)

ggsave(glue::glue("figure/covid_map_bw_{date}.png"), width = 16, height = 15.6, units = "cm", bg = "white")

# Map States --------------------------------------------------------------

sf_bl <- read_shp_land() %>%
  left_join(select(dictland, ars, label), by = "ars") %>%
  select(code = ars, region = label, geometry)

sf_covid_bl <- sf_bl %>%
  left_join(covid_bl, by = c("code", "region"))

center_bl <- pgo_centroid(sf_covid_bl, .cols = c("region", "fullq", "partq"))

center_bl$nudge_right <- center_bl$lat > 10

covid_vacc_map(sf_covid_bl, center_bl, nudge_x = 1) +
  labs(title = '**Share of people fully vaccinated against COVID-19 in Germany**',
       subtitle = format(date, "%b %d, %Y"),
       caption = caption_rki,
       fill = NULL)

ggsave(glue::glue("figure/covid_map_bl_{date}.png"), width = 16, height = 22.1, units = "cm", bg = "white")
