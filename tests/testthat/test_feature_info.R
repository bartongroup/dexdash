test <- readRDS("../test_data/test_object.rds")

test_ids <- c("G1", "G2")
test_ctr <- "C1"
test_sname <- "test"

test_output <- tibble::tribble(
  ~id, ~Name, ~Description, ~Expression, ~logFC, ~p_value, ~FDR,
  "G1", "N1", "D1", 1, 1, 0.1, 0.1,
  "G2", "N2", "D2", 1, 1, 0.1, 0.1
)

test_that("make_feature_info_table returns correct values", {
  tb <- make_feature_info_table(dexset = test$dex, features = test$features, ids = test_ids,
                                ctr = test_ctr, set_name = test_sname)
  expect_is(tb, "data.frame")
  expect_setequal(
    names(tb),
    c("id", "Name", "Description", "Expression", "logFC", "p_value", "FDR")
  )
  expect_equal(tb, test_output)
})

test_that("make_feature_info_table returns error message when too many points requested", {
  tb <- make_feature_info_table(dexset = test$dex, features = test$features, ids = test_ids,
                                ctr = test_ctr, set_name = test_sname, max_points = 1)
  expect_is(tb, "data.frame")
  expect_setequal(
    names(tb),
    c("Error")
  )
})
