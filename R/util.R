assert_colnames <- function(df, cols, df_name) {
  assertthat::assert_that(is(df, "data.frame"))
  n_missing <- sum(!(cols %in% colnames(df)))
  colstr <- stringr::str_c(cols, collapse = ", ")
  assertthat::assert_that(
    n_missing == 0,
    msg = stringr::str_glue("Some columns in '{df_name}' are missing. Required columns are: {colstr}.")
  )
}
