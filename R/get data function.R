riksbanken_serie <- function(ts){

  resp <- httr2::request("https://api.riksbank.se") |>
    httr2::req_url_path(
      "swea",
      "v1",
      "Observations",
      ts,
      # The two lines below is meant to be able to extract ALL the data, even if the
      # data base is updated, I want to be able to scrap it.
      "1900-01-01",
      as.character(Sys.Date())
    ) |>
    httr2::req_user_agent("732A94 lab") |>
    httr2::req_perform()

  data <- httr2::resp_body_json(resp)


  df <- do.call(rbind, lapply(data, as.data.frame))

  df$date <- as.Date(df$date)
  df$value <- as.numeric(df$value)

  return(df)
}
