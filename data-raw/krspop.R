# Kreise
library(dplyr)
library(purrr)
library(stringr)
library(readr)

krs <- c("Alb-Donau-Kreis Baden-Baden",
         "Biberach",
         "Böblingen",
         "Bodenseekreis Breisgau-Hochschwarzwald Calw",
         "Emmendingen Enzkreis",
         "Esslingen",
         "Freiburg im Breisgau Freudenstadt Göppingen Heidelberg Heidenheim Heilbronn, Land Heilbronn, Stadt Hohenlohekreis Karlsruhe, Land Karlsruhe, Stadt Konstanz",
         "Lörrach",
         "Ludwigsburg Main-Tauber-Kreis Mannheim Neckar-Odenwald-Kreis Ortenaukreis Ostalbkreis",
         "Pforzheim",
         "Rastatt",
         "Ravensburg Rems-Murr-Kreis Reutlingen Rhein-Neckar-Kreis Rottweil",
         "Schwäbisch Hall Schwarzwald-Baar-Kreis Sigmaringen",
         "Stuttgart",
         "Tübingen",
         "Tuttlingen",
         "Ulm",
         "Waldshut Zollernalbkreis")

krs <- map(krs, ~ str_split(.x, pattern = "(?<=[a-z])\\s(?=[A-Z])", simplify = FALSE)[[1]]) %>%
  reduce(c) %>%
  as.data.frame() %>%
  setNames("krs") %>%
  mutate(lead_krs = lead(krs),
         krs = if_else(grepl("^(Breisgau|Hall)$", lead_krs), paste(krs, lead_krs), krs)) %>%
  filter(!grepl("^(Breisgau|Hall)$", krs)) %>%
  pull(krs)

# Population 2020
pop <- c(198.204, 55.449, 202.25, 392.898, 217.901, 264.867, 160.149, 166.862, 199.752, 533.617, 230.94, 118.364, 258.781, 158.741, 132.812, 346.363, 126.458, 112.765, 446.852, 308.436, 286.876, 228.842, 544.971, 132.684, 309.721, 143.797, 432.58, 314.294, 126.016, 232.091, 285.888, 427.286, 287.497, 548.233, 140.166, 197.86, 212.872, 130.946, 630.305, 228.471, 141.682, 126.405, 171.237, 189.862)

krspop <- bind_cols(district = krs, pop = pop)

write_rds(krspop, "data-raw/rds/krspop.rds")
