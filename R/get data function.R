# PACKAGE USING THE API OF RIKSBANKEN

# FUNCTION FOR SELECTING A SPECIFIC TIMESERIES

riksbanken_serie <- function(ts){
  resp <- request("https://api.riksbank.se") |>
    req_url_path(
      "swea",
      "v1",
      "Observations",
      # Specifying timeseries below
      ts,
      "2026-01-01",
      "2026-10-02"
    ) |>
    req_user_agent("732A94 lab") |>
    req_perform()

  status <- print(resp_status(resp))

  data <- resp_body_json(resp)

  # Skapar en data frame
  df <- do.call(rbind,lapply(data,as.data.frame))
}
