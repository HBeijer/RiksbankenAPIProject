riksbanken_serie <- function(ts){

  resp <- request("https://api.riksbank.se") |>
    req_url_path(
      "swea",
      "v1",
      "Observations",
      ts,
      "2026-01-01",
      "2026-10-02"
    ) |>
    req_user_agent("732A94 lab") |>
    req_perform()

  print(resp_status(resp))
  print(resp_content_type(resp))

  data <- resp_body_json(resp)

  df <- do.call(
    rbind,
    lapply(data, as.data.frame)
  )

  return(df)
}
