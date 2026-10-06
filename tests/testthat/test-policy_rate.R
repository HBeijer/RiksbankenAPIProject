test_that("en höjning beräknas korrekt", {
  local_mocked_bindings(
    riksbanken_serie = function(ts) {
      data.frame(
        date = c("2026-01-01", "2026-01-02"),
        value = c(2, 2.25)
      )
    }
  )

  expect_output(
    policy_rate_status(),
    "increased by 0.25 percentage points.",
    fixed = TRUE
  )
})

test_that("tom data ger ett fel", {
  local_mocked_bindings(
    riksbanken_serie = function(ts) {
      data.frame(date = character(), value = numeric())
    }
  )

  expect_error(policy_rate_status(), "No observations were found")
})
