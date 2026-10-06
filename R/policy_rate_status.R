policy_rate_status <- function(ts = "SECBREFEFF") {

  # I call the function to grab the timeseries of policy rate
  d <- riksbanken_serie(ts)

  # CHANGE OF POLICY ###########################################################
  # We are interested in knowing whetehr the policy has actually changed.
  # we must first look in the difference between values to see if there is an
  # actual difference

  difference_between_values <- diff(d$value) !=0
  changed <- c(TRUE, difference_between_values)
  # then I save all the changed values
  changes <- d[changed, ]


  # CALCULATIONS OF THE POLICY #################################################

  # I want to check the current value of the policy rate
  current_rate <- tail(d$value, 1)

  # Then I need to find the last change in policy
  latest <- tail(changes, 1)

  # As I saved the latest changes in the policy rate, I can pick out the last one
  # before it was changed
  previous_rate <- changes$value[nrow(changes) - 1]

  # FUNCTION TO SEE IF POLICY RATE HAS CHANGED #################################

  # Before the function begins to see what the difference is I need to know if the
  # the difference is negative, positive, or 0.
  difference_in_policy_rate <- latest$value - previous_rate

  # Then if differnce is positive I know it was increase, if its negative it increased, and 0 it remained the same
  if (difference_in_policy_rate > 0){
    direction_of_policy_rate <- "increased"
  } else if (difference_in_policy_rate < 0){direction <- "decreased"} else{
    direction_of_policy_rate <- "unchanged"
  }

  # PRINT THE RESULT
  cat(
    "Current Information on the Swedish Policy Rate:","\n",
    "Current policy rate is:", current_rate, "%\n",
    "The Last change in policy was in:", as.character(latest$date), "\n",
    "The policy rate has", direction,
    "by", abs(difference_in_policy_rate), "percentage points.\n"
  )
}
