# Percent helper
percent <- function(x, accuracy = 1, scale = 100, big.mark = ".", decimal.mark = ",",  ...) {
  scales::percent(x, accuracy = accuracy, scale = scale, big.mark = ".", decimal.mark = ",", ...)
}

#' Map of Vaccination Rates
#'
#' @param .data a data.frame
#' @param
#'
#' @export
covid_vacc_map <- function(.data, .center, nudge_x = 0) {
  ggplot(.data) +
    geom_sf(aes(fill = fullq), size = .1, color = "white") +
    ggrepel::geom_text_repel(data = filter(.center, nudge_right), aes(x = lat, y = lon, label = paste(region, percent(fullq, .1), sep = "\n")),
                             min.segment.length = .1, point.padding = unit(1, "pt"), max.overlaps = Inf,
                             nudge_x = nudge_x, segment.size = .1, family = base_family, lineheight = .9, hjust = 1) +
    ggrepel::geom_text_repel(data = filter(.center, !nudge_right), aes(x = lat, y = lon, label = paste(region, percent(fullq, .1), sep = "\n")),
                             min.segment.length = .1, point.padding = unit(1, "pt"), max.overlaps = Inf,
                             nudge_x = -nudge_x, segment.size = .1, family = base_family, lineheight = .9, hjust = 0) +
    annotation_custom(grob = grid::textGrob(label = prognos.geo::pgo_cr_bkg(), x = unit(0, "npc"), y = unit(0, "npc"),
                                            gp = grid::gpar(fontfamily = base_family, fontsize = unit(8, "pt")), hjust = 0, vjust = 0)) +
    scale_x_continuous(expand = expansion(add = c(.6, .6))) +
    scale_fill_binned(low = "grey95", high = "#068C8b", labels = percent, show.limits = TRUE) +
    theme(axis.line = element_blank(), axis.text = element_blank(), axis.text.y.left = element_blank(), axis.ticks = element_blank(), axis.title = element_blank()) +
    theme(legend.position = c(0, 1), legend.justification = c(0, 1))
}


#' Barchart of Vaccination Rates
#'
#' @param x a data.frame
#'
#' @export
covid_vacc_bar <- function(.data) {
  .data <- .data %>%
    mutate(
      xwide = if_else(partq > fullq, partq, fullq),
      xslim = if_else(partq > fullq, fullq, partq),
      alphawide = if_else(partq > fullq, "part",  "full"),
      alphaslim = if_else(partq > fullq, "full",  "part"),
      fillwide = paste(fill, alphawide, sep = "_"),
      fillslim = paste(fill, alphaslim, sep = "_"),
      colorslim = if_else(partq > fullq, "white", "white"),
    )
  pal_full <- pal
  pal_part <- colorspace::lighten(pal, .4)
  names(pal_full) <- paste(names(pal_full), "full", sep = "_")
  names(pal_part) <- paste(names(pal), "part", sep = "_")
  pal1 <- c(pal_full, pal_part)

  ggplot(.data, aes(y = reorder(region, xwide))) +
    geom_col(aes(x = 1), fill = "grey95", width = width) +
    geom_col(aes(x = xwide, fill = fillwide), width = width) +
    geom_col(aes(x = xslim, fill = fillslim), width = width - .25, size = .1) +
    ggtext::geom_richtext(aes(x = xslim, label = percent(xslim, accuracy = .1),
                              color = colorslim),
                          family = base_family, fill = NA,
                          hjust = 1, label.colour = NA, size = 7 / .pt, vjust = .55, label.padding = unit(0, "pt"), label.margin = unit(2, "pt"), show.legend = FALSE) +
    ggtext::geom_richtext(aes(x = xwide, label = percent(xwide, accuracy = .1)), color = "black",
                          hjust = 0, label.colour = NA, fill = NA, size = 7 / .pt, vjust = .55, family = base_family,
                          label.padding = unit(1, "pt"), label.margin = unit(2, "pt"), show.legend = FALSE) +
    scale_x_continuous(expand = expansion(mult = 0), limits = c(0, NA), position = "top", labels = percent) +
    scale_fill_manual(values = pal1, breaks = c("other_full", "other_part"), labels = c("Fully vaccinated", "Partly vaccinated")) +
    scale_color_identity() +
    labs(x = NULL, y = NULL, fill = NULL, alpha = NULL) +
    theme(legend.position = "top",
          legend.justification = "left",
          legend.box.margin = margin(5.5, 0, 0, 0, "pt"),
          legend.margin = margin(0, 0, 0, 0, "pt"),
          legend.spacing.x = unit(0, "pt"),
          legend.text = element_text(margin = margin(0, 8, 0, 2, "pt")),
          plot.margin = margin(5.5, 16.5, 5.5, 5.5, "pt"),
          panel.grid.major = element_blank())
}
