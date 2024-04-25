test_that("Error when incorrect species", {
  expect_error(download_functional_terms("not a species"))
})
