test <- readRDS("../test_data/test_object.rds")

test_ids <- c("G1", "G2")
test_ctr <- "C1"
test_sname <- "test"

test_output <- tibble::tribble(
  ~id, ~log_fc, ~expr, ~p_value, ~fdr, ~contrast, ~name, ~description,
  "G1", 1, 1, 0.1, 0.1, "C1", "N1", "D1",
  "G2", 1, 1, 0.1, 0.1, "C1", "N2", "D2"
)

test_that("make_feature_info_table returns correct values", {
  tb <- make_feature_info_table(dexset = test$dex, features = test$features, ids = test_ids,
                                ctr = test_ctr, sname = test_sname)
  expect_is(tb, "data.frame")
  expect_setequal(
    names(tb),
    c("id", "log_fc", "expr", "p_value", "fdr", "contrast", "name", "description")
  )
  expect_equal(tb, test_output)
})

test_that("make_feature_info_table returns error message when too many points requested", {
  tb <- make_feature_info_table(dexset = test$dex, features = test$features, ids = test_ids,
                                ctr = test_ctr, sname = test_sname, max_points = 1)
  expect_is(tb, "data.frame")
  expect_setequal(
    names(tb),
    c("Error")
  )
})
