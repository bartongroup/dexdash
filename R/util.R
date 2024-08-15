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

#' Create a Download Link
#'
#' This function creates a Shiny download link with a specified ID.
#'
#' @param id A character string specifying the ID for the download link.
#' @return A Shiny download link UI component.
#' @noRd
download_link <- function(id) {
  shiny::downloadLink(
    outputId = shiny::NS(id, "handle_download"),
    label = bsicons::bs_icon("download")
  )
}

#' Create an Info Icon with Popover
#'
#' This function creates an info icon that, when clicked, displays a popover
#' with content from a markdown file.
#'
#' @param helper A character string specifying the name of the markdown file
#'   (without the extension) located in the "inst/helpers" directory.
#' @return A Shiny UI component with an info icon and a popover.
#' @noRd
info_icon <- function(helper) {
  bslib::popover(
    bsicons::bs_icon("info-circle"),
    htmltools::includeMarkdown(system.file(stringr::str_glue("helpers/{helper}.md"), package = "dexdash")),
    options = list(customClass = "info-pop")
  )
}

#' Create a Gear Icon with Popover
#'
#' This function creates a gear icon that, when clicked, displays a popover with the specified content.
#'
#' @param ... Additional arguments to be passed to the popover content.
#' @return A Shiny UI component with a gear icon and a popover.
#' @noRd
gear_icon <- function(...) {
  bslib::popover(
    bsicons::bs_icon("gear"),
    ...
  )
}

#' Create reactive container for app state
#'
#' Creates a container for app state to exchange information between modules. The
#' initialisation of fields is not necessary, as by default all undefined
#' fields are NULL. However, this seems like a good spot to gather all the field
#' names and comments together.
#' 
#' @param x_variable A startup variable to display in the x-axis in the feature plot.
#' @param colour_variable A startup variable to to use for colour in the feature plot.
#'
#' @return A reactive values list with pre-initialised fields.
#' @noRd
new_app_state <- function(x_variable, colour_variable) {
  shiny::reactiveValues(
    set_name = NULL,         # data set selection from the drop-down menu
    contrast = NULL,         # contrast selection from the drop-down menu
    search = NULL,           # name selection from the search box
    sel_brush = NULL,        # id selection in the main plot: brush
    sel_hover = NULL,        # id selection in the main plot: hover
    sel_tab = NULL,          # id selection from the feature info table: click row
    sel_term = NULL,         # id selection from the enrichment table: click row, select annotated feature ids
    sel_feature_info = NULL, # id selection from communication: features to show in feature info
    sel_feature_plot = NULL, # id selection from communication: features to show in feature plot
    sel_enrichment = NULL,   # id selection from brush for enrichment
    sel_volma_highlight = NULL, # id selection from communication: features to highlight in the main plot
    x_variable = x_variable, # a startup variable to display in the x-axis in the feature plot
    colour_variable = colour_variable # a startup variable to to use for colour in the feature plot
  )
}
