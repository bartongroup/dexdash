test_data <- tibble::tribble(
  ~id, ~sample, ~value,
  "G1", "A1", 10,
  "G1", "A2", 20,
  "G1", "B1", 10,
  "G1", "B2", 20,
  "G2", "A1", 30,
  "G2", "B1", 40
)

test_features <- tibble::tribble(
  ~id, ~name, ~description,
  "G1", "N1", "D1",
  "G2", "N2", "D2"
)

test_meta <- tibble::tribble(
  ~sample, ~group, ~replicate,
  "A1", "A", 1,
  "A2", "A", 2,
  "B1", "B", 1,
  "B2", "B", 2
)

test_that("plot_features returns correct object for one gene", {
  d <- test_data |>
    dplyr::filter(id == "G1") |>
    dplyr::left_join(test_features, by = "id")
  plt <- plot_features(d, test_meta, "lin")
  expect_true(ggplot2::is.ggplot(plt))

  dm <- d |>
    dplyr::left_join(test_meta, by = "sample")

  data_used <- ggplot2::ggplot_build(plt)$data[[1]]
  expect_equal(data_used$x |> as.numeric(), dm$group |> as.factor() |> as.numeric())
  expect_equal(data_used$y, d$value)
  expect_equal(plt$labels$title, d$name[1])
})


test_that("plot_features returns correct object for two genes", {
  d <- test_data |>
    dplyr::left_join(test_features, by = "id")
  plt <- plot_features(d, test_meta, "lin")
  expect_true(ggplot2::is.ggplot(plt))

  dm <- d |>
    dplyr::left_join(test_meta, by = "sample")

  data_used <- ggplot2::ggplot_build(plt)$data[[1]]
  expect_equal(data_used$x |> as.numeric(), dm$sample |> as.factor() |> as.numeric())
  expect_equal(data_used$y |> as.numeric(), dm$name |> as.factor() |> as.numeric())
})


test_that("plot_features returns NULL when data are empty", {
  d <-  test_data |>
    dplyr::filter(id == "none")
  plt <- plot_features(d, test_meta, "lin")
  expect_null(plt)
})
