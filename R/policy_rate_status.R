policy_rate_status <- function(ts = "SECBREPOEFF") {

  # Grab the time series
  d <- riksbanken_serie(ts)

  # Check that observations are available
  if (nrow(d) == 0L) {
    stop("No observations were found for this series and period.")
  }

  # Check the values and dates
  if (!is.numeric(d$value) || any(!is.finite(d$value))) {
    stop("The series must contain numeric values without NA or Inf.")
  }

  d$date <- as.Date(d$date)

  if (anyNA(d$date)) {
    stop("The series contains missing or invalid dates.")
  }

  # Sort the observations from oldest to newest
  d <- d[order(d$date), , drop = FALSE]

  # CHANGE OF POLICY ##########################################################

  difference_between_values <- diff(d$value) != 0

  # Keep the first value as a starting point for comparisons
  changed <- c(TRUE, difference_between_values)
  changes <- d[changed, , drop = FALSE]

  # CALCULATIONS OF THE POLICY #################################################

  current_rate <- tail(d$value, 1)
  latest_observation <- tail(d$date, 1)

  # Handle cases where no change can be identified
  if (nrow(changes) < 2L) {

    cat(
      "Latest available information on the Swedish policy rate:\n",
      "Observation date:", as.character(latest_observation), "\n",
      "Policy rate:", current_rate, "%\n"
    )

    if (nrow(d) == 1L) {
      cat("Only one observation is available; changes cannot be assessed.\n")
    } else {
      cat(
        "No change was observed between",
        as.character(d$date[1]), "and",
        as.character(latest_observation), ".\n"
      )
    }

    return(invisible(NULL))
  }

  # Find the latest change and the value before it
  latest <- tail(changes, 1)
  previous_rate <- changes$value[nrow(changes) - 1L]

  difference_in_policy_rate <- latest$value - previous_rate

  # DIRECTION OF THE CHANGE ####################################################

  if (difference_in_policy_rate > 0) {
    direction_of_policy_rate <- "increased"
  } else {
    direction_of_policy_rate <- "decreased"
  }

  # PRINT THE RESULT ##########################################################

  cat(
    "Latest available information on the Swedish policy rate:\n",
    "Observation date:", as.character(latest_observation), "\n",
    "Policy rate:", current_rate, "%\n",
    "The last observed change was on:", as.character(latest$date), "\n",
    "The policy rate", direction_of_policy_rate,
    "by", abs(difference_in_policy_rate)*100, "percentage points.\n"
  )
}
