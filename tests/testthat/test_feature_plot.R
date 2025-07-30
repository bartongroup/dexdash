test <- readRDS("../test_data/test_object.rds")
test_data <- test$dex$test

test_that("plot_features returns correct object for one gene", {
  d <- test_data$data |>
    dplyr::filter(id == "G1") |>
    dplyr::left_join(test$features, by = "id") |>
    dplyr::left_join(test_data$metadata, by = "sample")
  plt <- plot_features(d, what = "value", x_var = "group", colour_var = "group")
  expect_true(ggplot2::is_ggplot(plt))

  data_used <- ggplot2::ggplot_build(plt)$data[[1]]
  expect_equal(data_used$x |> as.numeric(), d$group |> as.factor() |> as.numeric())
  expect_equal(data_used$y, d$value)
  expect_equal(plt$labels$title, d$name[1])
})


test_that("plot_features returns correct object for two genes", {
  d <- test_data$data |>
    dplyr::left_join(test$features, by = "id") |>
    dplyr::left_join(test_data$metadata, by = "sample")
  plt <- plot_features(d, what = "value", x_var = "sample", colour_var = "sample")
  expect_true(ggplot2::is_ggplot(plt))

  dm <- d |>
    dplyr::left_join(test_data$metadata, by = "sample")

  data_used <- ggplot2::ggplot_build(plt)$data[[1]]
  expect_equal(data_used$x |> as.numeric(), dm$sample |> as.factor() |> as.numeric())
  expect_equal(data_used$y |> as.numeric(), dm$name |> as.factor() |> as.numeric())
})


test_that("plot_features returns NULL when data are empty", {
  d <-  test_data$data |>
    dplyr::filter(id == "none")
  plt <- plot_features(d, x_var = "sample", colour_var = "sample")
  expect_null(plt)
})
