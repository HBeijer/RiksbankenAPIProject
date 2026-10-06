#' List available Riksbank series
#'
#' Retrieve identifiers and short descriptions of interest rate and exchange
#' rate series from the Riksbank SWEA API.
#'
#' @return A data frame with character columns \code{series_id} and
#'   \code{description}. An empty response returns a table with zero rows.
#' @examples
#' head(available_series())
#'
#' @export
available_series <- function(){

  resp <- httr2::request("https://api.riksbank.se") |>
    httr2::req_url_path("swea", "v1", "Series") |>
    httr2::req_user_agent("732A94 lab") |>
    # Reduce the frequency of API requests.
    httr2::req_throttle(rate = 1 / 15) |> # requests per second
    # Retry temporary errors, with a 60-second fallback wait.
    httr2::req_retry(max_tries = 3, backoff = function(tries) 60) |>
    httr2::req_perform()

  data <- httr2::resp_body_json(resp)

  # Return a correctly structured table when there are no records.
  if (length(data) == 0L) return(data.frame(series_id = character(), description = character()))

  df <- do.call(rbind, lapply(data, as.data.frame))
  stopifnot("API response is missing required fields" = all(c("seriesId", "shortDescription") %in% names(df)))

  # Select and rename the columns users need.
  result <- data.frame(series_id = df$seriesId,
                       description = df$shortDescription,
                       row.names = NULL)

  return(result)
}

