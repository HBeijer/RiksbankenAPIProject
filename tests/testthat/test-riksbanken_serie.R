test_that("riksbanken_serie returns a data frame",{
  resultat <- riksbanken_serie(ts="SECBREFEFF")
  expect_s3_class(resultat,"data.frame")
})
