riksbanken_serie <- function(ts){

  resp <- request("https://api.riksbank.se") |>
    req_url_path(
      "swea",
      "v1",
      "Observations",
      ts,
      "1900-01-01",
      as.character(Sys.Date())
    ) |>
    req_user_agent("732A94 lab") |>
    req_perform()

  data <- resp_body_json(resp)

  df <- do.call(rbind, lapply(data, as.data.frame))

  df$date <- as.Date(df$date)
  df$value <- as.numeric(df$value)

  return(df)
}
