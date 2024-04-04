test <- readRDS("../test_data/test_object.rds")


test_that("Missing columns in data throw errors", {
  dummy <- data.frame(x = 1, y = 2)

  expect_error(run_app(dummy, test$data, test$metadata, test$features, test$features))
  expect_error(run_app(test$de, dummy, test$metadata, test$features, test$features))
  expect_error(run_app(test$de, test$data, dummy, test$features, test$features))
  expect_error(run_app(test$de, test$data, test$metadata, dummy, test$features))
})
