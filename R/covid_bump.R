library(tidyverse)
require(ggtext)

source("R/covid_charts.R", encoding = "UTF-8")

# Theme -------------------------------------------------------------------
base_family <- "Roboto Condensed"

theme_set(theme_minimal(base_size = 11, base_family = base_family))

theme_update(
  plot.title = ggtext::element_markdown(margin = margin(b = 2, unit = "pt")),
  plot.title.position = "plot",
  plot.subtitle = ggtext::element_markdown(size = 8, margin = margin(t = 0, b = 2, unit = "pt")),
  plot.caption = ggtext::element_markdown(size = 5, color = "grey45", lineheight = 1.4),
  strip.text.x = element_text(face = "bold"),
  axis.text.y.right = element_text(size = 8, hjust = 1, margin = margin(l = 2.2, unit = "pt")),
  axis.text.y.left = element_text(size = 8, hjust = 0, margin = margin(l = 2.2, r = 2.2, unit = "pt")),
  axis.ticks.length = unit(2.2, "pt"),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank()
)

# Colors ------------------------------------------------------------------
pal <- c("other" = "#068C8b", "bw" = "#C8102E", "de" = "#007dcc")

# Read data ---------------------------------------------------------------
fn <- list.files("data-raw/csv", "\\.csv")
names(fn) <- fn

covid_bw <- purrr::imap(fn, function(x, y) {
  read_csv(glue::glue("data-raw/csv/{x}")) %>%
    mutate(color = case_when(
      grepl("Würt", district) ~ "bw",
      grepl("^Deu", district) ~ "de",
      TRUE ~ "other"
    )) %>%
    rename(region = district)
}) %>%
  bind_rows(.id = "date") %>%
  mutate(date = str_extract(date, "\\d{4}\\-\\d{2}\\-\\d{2}"),
         date = as.Date(date))

ggplot(covid_bw, aes(date, fullq, color = color, group = region)) +
  geom_line() +
  ggtext::geom_richtext(aes(label = percent(fullq, accuracy = .1)), color = "black",
                        hjust = .5, label.colour = NA, fill = "white", size = 7 / .pt, vjust = .55, family = base_family,
                        label.padding = unit(2, "pt"), label.margin = unit(2, "pt"), show.legend = FALSE) +
  scale_color_manual(values = pal) +
  scale_x_date(breaks = unique(covid_bw$date), labels = ~ format(.x, "%b %d")) +
  scale_y_continuous(labels = percent) +
  labs(x = NULL, y = NULL) +
  guides(color = "none")

covid_bw1 <- covid_bw %>%
  filter(date %in% range(date)) %>%
  group_by(region, fill = color) %>%
  summarise(fullq = diff(fullq),
            partq = diff(partq)) %>%
  ungroup()

ggplot(covid_bw1, aes(x = fullq, y = reorder(region, partq), fill = fill)) +
  #geom_col(aes(x = 1), fill = "grey95", width = width) +
  geom_col(aes(x = partq, alpha = "part"), width = width, alpha = .6) +
  geom_col(aes(alpha = "full"), width = width - .25, size = .1) +
  ggtext::geom_richtext(aes(label = percent(fullq, accuracy = .1)),
                        color = "white", family = base_family, fill = NA,
                        hjust = 1, label.colour = NA, size = 7 / .pt, vjust = .55, label.padding = unit(0, "pt"), label.margin = unit(2, "pt"), show.legend = FALSE) +
  ggtext::geom_richtext(aes(x = partq, label = percent(partq, accuracy = .1)), color = "black",
                        hjust = 0, label.colour = NA, fill = NA, size = 7 / .pt, vjust = .55, family = base_family,
                        label.padding = unit(1, "pt"), label.margin = unit(2, "pt"), show.legend = FALSE) +
  scale_x_continuous(expand = expansion(mult = 0), limits = c(0, .03), position = "top", labels = ~ percent(.x, accuracy = .1)) +
  scale_fill_manual(values = pal) +
  scale_alpha_manual(breaks = c("full", "part"), values = c(part = .6, full = 1),
                     labels = c(part = "Partly vaccinated", full = "Fully vaccinated"),
                     guide = guide_legend(override.aes = list(fill = "#068C8b"))) +
  labs(x = NULL, y = NULL, fill = NULL, alpha = NULL) +
  guides(fill = "none") +
  theme(legend.position = "top",
        legend.justification = "left",
        legend.box.margin = margin(5.5, 0, 0, 0, "pt"),
        legend.margin = margin(0, 0, 0, 0, "pt"),
        legend.spacing.x = unit(0, "pt"),
        legend.text = element_text(margin = margin(0, 8, 0, 2, "pt")),
        plot.margin = margin(5.5, 16.5, 5.5, 5.5, "pt"),
        panel.grid.major = element_blank()) +
  labs(title = '<b>Increase in share of people vaccinated against COVID-19 in *The Länd*</b>',
       subtitle = glue::glue('{format(as.Date("2021-11-14"), "%b %d, %Y")} to {format(date, "%b %d, %Y")}'),
       caption = caption_msa)

covid_bw2 <- covid_bw %>%
  group_by(date) %>%
  mutate(rank = row_number(-fullq)) %>%
  ungroup()

library(ggbump)

labels1 <- covid_bw2 %>%
  filter(date == min(date)) %>%
  select(rank, region) %>%
  deframe()

labels2 <- covid_bw2 %>%
  filter(date == max(date)) %>%
  select(rank, region) %>%
  deframe()

ggplot(covid_bw2, aes(date, rank, color = region, group = region)) +
  geom_bump() +
  geom_point(size = 2) +
  ggtext::geom_richtext(aes(label = percent(fullq, accuracy = .1), fill = region),
                        color = "white", family = base_family,
                        hjust = .5, label.colour = NA, size = 7 / .pt, vjust = .55, label.padding = unit(2, "pt"), label.margin = unit(2, "pt"), show.legend = FALSE) +
  scale_x_date(breaks = unique(covid_bw2$date), date_labels = "%b %d") +
  scale_y_reverse(breaks = 1:45, labels = function(x) labels1[as.character(x)], sec.axis = sec_axis(trans = ~ .x, breaks = 1:45, labels = function(x) labels2[as.character(x)])) +
  #scale_color_manual(values = pal) +
  #scale_fill_manual(values = pal) +
  guides(color = "none", size = "none") +
  #remove_axis() +
  labs(x = NULL, y = NULL)

