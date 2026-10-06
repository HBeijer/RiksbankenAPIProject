#' Retrieve observations for a Riksbank series
#'
#' Download observations for one series and a specified date interval
#' from the Riksbank SWEA API.
#'
#' @param ts A single series identifier, such as "SECBREPOEFF".
#'   Leading and trailing whitespace is removed.
#' @param from The start date, supplied as a Date object or a character
#'   string in YYYY-MM-DD format. Defaults to "1900-01-01".
#' @param to The end date, supplied as a Date object or a character
#'   string in YYYY-MM-DD format. Defaults to the current date.
#'   Must be later than or equal to from.
#'
#' @return A data frame containing a date column of class Date
#'   and a numeric value column. If no observations are returned,
#'   the result has zero rows.
#'
#' @examples
#' observations <- riksbanken_serie(
#'   "SECBREPOEFF",
#'   from = "2026-01-01",
#'   to = "2026-10-05"
#' )
#' head(observations)
#'
#' @export
riksbanken_serie <- function(ts,
                             from = "1900-01-01",
                             to = Sys.Date()) {

  # Check the series identifier = Input check
  stopifnot("ts must be one nonmissing, nonempty character string" =
              is.character(ts) &&
              length(ts) == 1L &&
              !is.na(ts) &&
              nzchar(trimws(ts))) # trimws() remove leading/trailing whitespace, nzchar() check non-empty strings

  # Check the date argument types and lengths.
  stopifnot("from must be one nonmissing character string or Date" =
              (is.character(from) || inherits(from, "Date")) &&
              length(from) == 1L &&
              !is.na(from),
            "to must be one nonmissing character string or Date" =
              (is.character(to) || inherits(to, "Date")) &&
              length(to) == 1L &&
              !is.na(to))

  # Save the text representations, then parse them as dates.
  from_text <- as.character(from)
  to_text <- as.character(to)
  from <- as.Date(from_text, format = "%Y-%m-%d") #"2026-10-05siu" would be "2026-10-05"
  to <- as.Date(to_text, format = "%Y-%m-%d")

  # Require valid dates in the expected format and correct order.
  # This helps with situation where user uses wrong format from = "05-10-2026", then from_text = "05-10-2026" but from after as.Date = "0005-10-20"
  stopifnot("from must be a valid date in YYYY-MM-DD format" = !is.na(from) && as.character(from) == from_text,
            "to must be a valid date in YYYY-MM-DD format" = !is.na(to) && as.character(to) == to_text,
            "from must be earlier than or equal to to" = from <= to)

  ts <- trimws(ts)

  resp <- httr2::request("https://api.riksbank.se") |>
    httr2::req_url_path("swea",
                        "v1",
                        "Observations",
                        ts,
                        as.character(from),
                        as.character(to)) |>
    httr2::req_user_agent("732A94 lab") |>
    # Retry temporary errors, with a 60-second fallback wait.
    httr2::req_retry(max_tries = 3, backoff = function(tries) 60) |>
    httr2::req_perform()

  # Represent HTTP 204 as an empty list, otherwise parse the JSON.
  if (httr2::resp_status(resp) == 204L) {
    data <- list()
  } else {
    data <- httr2::resp_body_json(resp)
  }

  # Handle either kind of successful empty response in one place.
  # We want to keep the output consistent because if length(data) == 0L, the function would just return NULL.
  # Return a zero-row data frame with consistent column types when there are no observations.
  if (length(data) == 0L) return(data.frame(date = as.Date(character()), value = numeric()))

  df <- do.call(rbind, lapply(data, as.data.frame))

  df$date <- as.Date(df$date)
  df$value <- as.numeric(df$value)

  return(df)
}



