# Module FEATURE INFO
#
# Creates an DT table with basing feature information, from a selection of feature IDs.
#
# Input:
#    state$sel_feature_info - selection of feature IDs to display
#    state$contrast - which contrast information to display
#    state$experiment - which experiment to select
#
# Output:
#    state$sel_tab - one feature ID selected in this info table
#
# Uses:
#    data_set$de - a tibble with differential expression values
#    data_set$features - a tibble with feature ID and feature name
#

# ----- UI definitions -----

mod_feature_info_ui <- function(id) {
  ns <- shiny::NS(id)

  info <- bslib::popover(
    bsicons::bs_icon("info-circle"),
    htmltools::includeMarkdown(system.file("helpers/feature_info.md", package = "dexdash")),
    options = list(customClass = "info-pop")
  )

  bslib::card(
    bslib::card_header(
      "Feature information",
      info,
      class = "d-flex justify-content-between"
    ),
    DT::dataTableOutput(
      outputId = ns("feature_info")
    )
  )

}

# ----- Server logic -----

mod_feature_info_server <- function(id, data_set, state) {

  server <- function(input, output, session) {
    contrast <- name <- description <- log_fc <- fdr <- logFC <- FDR <- NULL

    # Prepare updated feature info table
    feature_info_table <- shiny::reactive({
      ids <- state$sel_feature_info
      ctr <- state$contrast
      expm <- state$experiment
      shiny::req(ids, ctr, expm)
      make_feature_info_table(data_set$de, data_set$features, ids, ctr, expm)
    })

    # Wait for row selection in the feature info table, pass it to app state
    shiny::observeEvent(input$feature_info_rows_selected, ignoreNULL = FALSE, {
      rows_sel <- input$feature_info_rows_selected
      if(is.null(rows_sel)) {
        state$sel_tab <- NULL
      } else {
        ge <- feature_info_table()
        ids <- ge[rows_sel, ]$id
        state$sel_tab <- ids
      }
    })

    # feature_info table
    output$feature_info <- DT::renderDataTable({
      ge <- feature_info_table()
      if(!("Error" %in% colnames(ge))) {
        ge <- ge |>
          dplyr::select(Name = name, Description = description, logFC = log_fc, FDR = fdr) |>
          dplyr::mutate(dplyr::across(c(logFC, FDR), ~signif(.x, 3)))
      }
      DT::datatable(
        ge,
        options = list(paging = FALSE, dom = "t"),
        style = "bootstrap",
        selection = "single",
        rownames = FALSE
      ) |>
        DT::formatStyle(columns = colnames(ge), fontSize = '80%')
    })
  }

  shiny::moduleServer(id, server)
}


# ----- Functions used in the module -----

#' Generate a Feature Information Table
#'
#' Creates a table containing detailed information about selected features based
#' on their identifiers. This function is designed to work with differential
#' expression data and can filter this data by specific identifiers and a
#' contrast of interest. It joins this filtered data with a set of features to
#' provide a comprehensive overview. If the number of selected identifiers
#' exceeds the `max_points` threshold, the function returns an error message
#' instead of a data frame.
#'
#' @param de A data frame containing differential expression data. This data
#'   should include at least two columns: `id` for the feature identifier and
#'   `contrast` for the comparison of interest.
#' @param features A data frame with feature ids and names.
#' @param ids A vector of identifiers for the features of interest. This
#'   parameter allows the user to specify which features should be included in
#'   the information table.
#' @param ctr A character string specifying the contrast to filter the
#'   differential expression data.
#' @param expm A character string specyifying the experiment to filter the DE
#'   data.
#' @param max_points The maximum number of points (features) that can be
#'   processed and included in the table at once. This parameter helps to
#'   prevent performance issues with very large datasets. The default value is
#'   3000.
#'
#' @return Returns a data frame containing detailed information about the
#'   features specified by `ids` and filtered by `ctr`. If the number of ids
#'   exceeds `max_points`, it returns a tibble with an error message.
#' @noRd
make_feature_info_table <- function(de, features, ids, ctr, expm, max_points = 3000) {
  id <- contrast <- experiment <- name <- NULL

  if (length(ids) <= max_points) {
    df <- de |>
      dplyr::filter(id %in% ids & contrast == ctr & experiment == expm) |>
      dplyr::left_join(features, by = "id") |>
      dplyr::arrange(name) |>
      dplyr::mutate(name = stringr::str_replace_all(name, ";", "; "))
  } else {
    df <- tibble::tibble(Error = stringr::str_glue("Only {max_points} points can be selected."))
  }
  return(df)
}

