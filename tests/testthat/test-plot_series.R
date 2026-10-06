test_that("Checking that data is getting the correct date, title, and interval.", {

  test_data <- data.frame(
    date = as.Date(c("2026-01-01", "2026-01-02")),
    value = c(2, 2.25)
  )

  local_mocked_bindings(
    riksbanken_serie = function(ts, from, to) {
      expect_equal(from, "2026-01-01")
      expect_equal(to, "2026-01-02")
      test_data
    },

    available_series = function() {
      data.frame(
        series_id = c("OTHER", "TEST"),
        description = c("Annan serie", "Testserie")
      )
    }
  )

  p <- plot_series(
    "TEST",
    from = "2026-01-01",
    to = "2026-01-02"
  )

  expect_true(inherits(p, "ggplot"))
  expect_equal(p$data, test_data)
  expect_equal(p$labels$title, "Testserie")

  line_data <- ggplot2::layer_data(p)

  expect_equal(line_data$x, as.numeric(test_data$date))
  expect_equal(line_data$y, test_data$value)
})


test_that("Empty data should give an error", {

  local_mocked_bindings(
    riksbanken_serie = function(ts, from, to) {
      data.frame(
        date = as.Date(character()),
        value = numeric()
      )
    }
  )

  expect_error(
    plot_series("TEST"),
    "No observations were found"
  )
})
