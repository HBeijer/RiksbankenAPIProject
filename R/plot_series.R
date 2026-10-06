plot_series <- function(ts){

  # Calling on the other function to take out the DF
  ts <- trimws(ts)
  d <- riksbanken_serie(ts)

  # Picking out the possible names of the variables
  ts_name <- available_series()
  # Checking what the name of the specific variable is
  name <- ts_name$description[ts_name$series_id == ts]

  # Plotting the ts
  plot <- ggplot2::ggplot(d) +
    ggplot2::geom_line(mapping =
                         ggplot2::aes(x = date,y=value),
                       linewidth = 1.1
    )  +
    ggplot2::theme_bw()+
    ggplot2::labs(
      title = name,
      x = "Year",
      y= "Value"
    )

  return(plot)
}
