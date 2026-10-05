test_that("available_series returns a correctly structured series table", {
  result <- available_series()
  expect_s3_class(result, "data.frame")
  expect_equal(names(result), c("series_id", "description"))
  expect_type(result$series_id, "character")
  expect_type(result$description, "character")
  expect_true(nrow(result) > 0)
  expect_true("SECBREPOEFF" %in% result$series_id)
  }
  )
