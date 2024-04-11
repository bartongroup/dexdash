#' Assert that a data frame contains specific columns
#'
#' This function checks whether a given data frame contains all of the required columns.
#' It throws an error if any of the specified columns are missing. This is useful for
#' ensuring that data frames meet expected specifications before performing operations
#' that depend on those columns.
#'
#' @param df A data frame whose columns are to be checked.
#' @param cols A character vector specifying the required column names.
#' @param df_name A character string representing the name of the data frame variable
#'   as it should appear in error messages. This improves the readability of error messages
#'   by indicating which data frame has missing columns.
#'
#' @return Invisibly returns `TRUE` if all specified columns are present in the data frame.
#'   If any columns are missing, an error is thrown with a message specifying which
#'   columns are absent.
#'
#' @importFrom methods is
#' @noRd
assert_colnames <- function(df, cols, df_name) {
  assertthat::assert_that(is(df, "data.frame"))
  n_missing <- sum(!(cols %in% colnames(df)))
  colstr <- stringr::str_c(cols, collapse = ", ")
  assertthat::assert_that(
    n_missing == 0,
    msg = stringr::str_glue("Some columns in '{df_name}' are missing. Required columns are: {colstr}.")
  )
}
