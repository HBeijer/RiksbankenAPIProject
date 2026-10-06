# The first argument of test_that() describes the behavior we want.
# The code inside { ... } checks that behavior.
test_that("invalid series identifiers are rejected", {
  for (bad_ts in list(123, NA_character_, "", "   ", c("A", "B"))) {
    expect_error(riksbanken_serie(ts = bad_ts),
                 regexp = "ts must be one nonmissing, nonempty character string")}
  }
  )

test_that("invalid dates and reversed intervals are rejected", {
  expect_error(riksbanken_serie("SECBREPOEFF", from = "2026-10-05siu"),
               regexp = "from must be a valid date")

  expect_error(riksbanken_serie("SECBREPOEFF", to = "invalid"),
               regexp = "to must be a valid date")

  expect_error(riksbanken_serie("SECBREPOEFF", from = "2026-10-05", to = "2026-10-04"),
               regexp = "from must be earlier than or equal to to")
  }
  )

test_that("a fixed query returns the expected observation", {
  result <- riksbanken_serie("SECBREPOEFF",
                             from = "2026-10-05",
                             to = "2026-10-05")

  expect_s3_class(result, "data.frame")
  expect_equal(result$date, as.Date("2026-10-05"))
  expect_equal(result$value, 1.75)
  }
  )

test_that("a large query returns many observations", {
  result <- riksbanken_serie("SECBREPOEFF", to = "2025-12-31")
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 1000)
})

test_that("the API rejects identifiers longer than 25 characters", {
  expect_error(riksbanken_serie("ABCDEFGHIJKLMNOPQRSTUVWXYZ",
                                from = "2026-10-04",
                                to = "2026-10-05"),
               class = "httr2_http_400")
  }
  )

#err <- tryCatch(
#  riksbanken_serie(
#    "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
#    from = "2025-01-02",
#    to = "2025-01-02"
#  ),
#  error = function(e) e
#)
#class(err)
#inherits(err,"httr2_http_400")
#conditionMessage(err)
#https://api.riksbank.se/swea/v1/Observations/ABCDEFGHIJKLMNOPQRSTUVWXYZ/2025-01-02/2025-01-02
