test_de <- tibble::tribble(
  ~id, ~log_fc, ~expr, ~p_value, ~fdr, ~contrast,
  "G1", 1, 1, 0.1, 0.1, "C1",
  "G2", 1, 1, 0.1, 0.1, "C1",
  "G3", 1, 1, 0.1, 0.1, "C1",
  "G1", 1, 1, 0.1, 0.1, "C2",
  "G2", 1, 1, 0.1, 0.1, "C2",
)

test_features <- tibble::tribble(
  ~id, ~name, ~description,
  "G1", "N1", "D1",
  "G2", "N2", "D2",
  "G3", "N3", "D3"
)

test_ids <- c("G1", "G2")
test_ctr <- "C1"

test_output <- tibble::tribble(
  ~id, ~log_fc, ~expr, ~p_value, ~fdr, ~contrast, ~name, ~description,
  "G1", 1, 1, 0.1, 0.1, "C1", "N1", "D1",
  "G2", 1, 1, 0.1, 0.1, "C1", "N2", "D2"
)

test_that("make_feature_info_table returns correct values", {
  tb <- make_feature_info_table(test_de, test_features, test_ids, test_ctr)
  expect_is(tb, "data.frame")
  expect_setequal(
    names(tb),
    c("id", "log_fc", "expr", "p_value", "fdr", "contrast", "name", "description")
  )
  expect_equal(tb, test_output)
})

test_that("make_feature_info_table returns error message when too many points requested", {
  tb <- make_feature_info_table(test_de, test_features, test_ids, test_ctr, max_points = 1)
  expect_is(tb, "data.frame")
  expect_setequal(
    names(tb),
    c("Error")
  )
})
