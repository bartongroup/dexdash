test <- readRDS("../test_data/test_object.rds")

test_that("plot_features returns correct object for one gene", {
  d <- test$data |>
    dplyr::filter(id == "G1") |>
    dplyr::left_join(test$features, by = "id") |>
    dplyr::left_join(test$meta, by = "sample")
  plt <- plot_features(d)
  expect_true(ggplot2::is.ggplot(plt))

  data_used <- ggplot2::ggplot_build(plt)$data[[1]]
  expect_equal(data_used$x |> as.numeric(), d$group |> as.factor() |> as.numeric())
  expect_equal(data_used$y, d$value)
  expect_equal(plt$labels$title, d$name[1])
})


test_that("plot_features returns correct object for two genes", {
  d <- test$data |>
    dplyr::left_join(test$features, by = "id") |>
    dplyr::left_join(test$meta, by = "sample")
  plt <- plot_features(d)
  expect_true(ggplot2::is.ggplot(plt))

  dm <- d |>
    dplyr::left_join(test$meta, by = "sample")

  data_used <- ggplot2::ggplot_build(plt)$data[[1]]
  expect_equal(data_used$x |> as.numeric(), dm$sample |> as.factor() |> as.numeric())
  expect_equal(data_used$y |> as.numeric(), dm$name |> as.factor() |> as.numeric())
})


test_that("plot_features returns NULL when data are empty", {
  d <-  test$data |>
    dplyr::filter(id == "none")
  plt <- plot_features(d)
  expect_null(plt)
})
