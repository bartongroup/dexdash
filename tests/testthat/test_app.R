test <- readRDS("../test_data/test_object.rds")


test_that("Correct object returned by dexdash_set()", {
  dx <- dexdash_set(test$dex$test$de, test$dex$test$data, test$dex$test$metadata, name = "test")
  expect_equal(dx, test$de$test)
})

test_that("Missing columns in data throw errors", {
  dummy <- data.frame(x = 1, y = 2)

  expect_error(dexdash_set(dummy, test$dex$test$data, test$dex$test$metadata, name = "test"))
  expect_error(dexdash_set(test$dex$test$de, dummy, test$dex$test$metadata, name = "test"))
  expect_error(dexdash_set(test$dex$test$de, test$dex$test$data, dummy, name = "test"))
})

test_that("Incorrect variable arguments throw errors", {
  expect_error(run_app(test$dex, test$features, list(test = test$fterms), x_variable = "wrong"))
  expect_error(run_app(test$dex, test$features, list(test = test$fterms), value_variable = "wrong"))
  expect_error(run_app(test$dex, test$features, list(test = test$fterms), colour_variable = "wrong"))
  expect_error(run_app(test$dex, test$features, list(test = test$fterms), volma_fdr_limit = "wrong"))
  expect_error(run_app(test$dex, test$features, list(test = test$fterms), volma_fdr_limit = 42))
  expect_error(run_app(test$dex, test$features, list(test = test$fterms), volma_logfc_limit = "wrong"))
  expect_error(run_app(test$dex, test$features, list(test = test$fterms), volma_logfc_limit = -42))
  expect_error(run_app(test$dex, test$features, list(test = test$fterms), enrichment_fdr_limit = "wrong"))
  expect_error(run_app(test$dex, test$features, list(test = test$fterms), enrichment_fdr_limit = 42))
})