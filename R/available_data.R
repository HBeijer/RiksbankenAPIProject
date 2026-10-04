available_series <- function(){

  resp <- httr2::request("https://api.riksbank.se") |>
    httr2::req_url_path(
      "swea",
      "v1",
      "Series"
    ) |>
    httr2::req_user_agent("732A94 lab") |>
    httr2::req_perform()

  data <- httr2::resp_body_json(resp)

  df <- do.call(
    rbind,
    lapply(data, as.data.frame)
  )

  result <- data.frame(
    series_id = df$seriesId,
    description = df$shortDescription,
    row.names = NULL
  )

  return(result)
}
