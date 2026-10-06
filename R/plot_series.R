#' Plot a Riksbank series
#'
#' @inheritParams riksbanken_serie
#' @return A ggplot object.
#' @importFrom rlang .data
#' @export
plot_series <- function(ts,
                        from = "1900-01-01",
                        to = Sys.Date()) {

  # Hämta data. Hämtningsfunktionen kontrollerar argumenten.
  d <- riksbanken_serie(ts, from = from, to = to)
  ts <- trimws(ts)

  if (nrow(d) == 0L) {
    stop("No observations were found for this series and period.")
  }

  # Hämta seriens beskrivning
  ts_name <- available_series()
  name <- ts_name$description[match(ts, ts_name$series_id)]

  # Använd serie-id om beskrivningen saknas
  if (length(name) != 1L || is.na(name) || !nzchar(name)) {
    name <- ts
  }

  plot <- ggplot2::ggplot(
    d,
    ggplot2::aes(x = .data$date, y = .data$value)
  ) +
    ggplot2::theme_bw() +
    ggplot2::labs(
      title = name,
      x = "Year",
      y = "Value"
    )

  # Visa en punkt om det bara finns en observation
  if (nrow(d) == 1L) {
    plot <- plot + ggplot2::geom_point(size = 2)
  } else {
    plot <- plot + ggplot2::geom_line(linewidth = 1.1)
  }

  return(plot)
}
