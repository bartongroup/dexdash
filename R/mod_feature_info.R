# Module FEATURE INFO
#
# Creates an DT table with basing feature information, from a selection of feature IDs.
#
# Input:
#    state$sel_feature_info - selection of feature IDs to display
#    state$contrast - which contrast information to display
#    state$set_name - which set to select
#
# Output:
#    state$sel_tab - one feature ID selected in this info table
#
# Uses:
#    data_set$dex - user data
#    data_set$features - a tibble with feature ID and feature name
#

# ----- UI definitions -----

mod_feature_info_ui <- function(id) {
  ns <- shiny::NS(id)

  info <- info_icon("feature_info")

  download <- shiny::uiOutput(ns("download_table"))

  bslib::card(
    bslib::card_header(
      "Feature information",
      shiny::span(info, download),
      class = "d-flex justify-content-between"
    ),
    bslib::card_body(
      DT::dataTableOutput(
        outputId = ns("feature_info")
      )
    )
  )

}

# ----- Server logic -----

mod_feature_info_server <- function(id, data_set, state) {

  server <- function(input, output, session) {
    contrast <- name <- description <- log_fc <- fdr <- logFC <- FDR <- NULL

    # Wait for row selection in the feature info table, pass it to app state
    shiny::observeEvent(input$feature_info_rows_selected, ignoreNULL = FALSE, {
      rows_sel <- input$feature_info_rows_selected
      if(is.null(rows_sel)) {
        state$sel_tab <- NULL
      } else {
        ge <- make_table()
        ids <- ge[rows_sel, ]$id
        state$sel_tab <- ids
      }
    })

    # Make the table
    make_table <- shiny::reactive({
      ids <- state$sel_feature_info
      ctr <- state$contrast
      set_name <- state$set_name
      shiny::req(ids, ctr, set_name)
      make_feature_info_table(data_set$dex, data_set$features, ids, ctr, set_name)
    })

    # Output DT table
    output$feature_info <- DT::renderDataTable({
      Name <- Description <- logFC <- FDR <- NULL
      ge <- make_table()
      if(!("Error" %in% colnames(ge))) {
        ge <- ge |>
          dplyr::select(Name, Description, logFC, FDR)
        DT::datatable(
          ge,
          options = list(paging = FALSE, dom = "t"),
          style = "bootstrap",
          selection = "single",
          rownames = FALSE
        ) |>
          DT::formatStyle(columns = colnames(ge), fontSize = '80%')
      }
    })

    # Display download icon only when there are data to show
    output$download_table <- shiny::renderUI({
      shiny::req(state$sel_feature_info, state$contrast, state$set_name)
      download_link(id)
    })

    # Download handler
    output$handle_download <- shiny::downloadHandler(
      filename = "feature_info.csv",
      content = function(file) {
        make_table() |>
          readr::write_csv(file)
      }
    )
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
#' @param dexset A \code{dexset_list} object with all user data.
#' @param features A data frame with feature ids and names.
#' @param ids A vector of identifiers for the features of interest. This
#'   parameter allows the user to specify which features should be included in
#'   the information table.
#' @param ctr A character string specifying the contrast to filter the
#'   differential expression data.
#' @param set_name A character string specyifying the set to extraxt the DE
#'   data from.
#' @param max_points The maximum number of points (features) that can be
#'   processed and included in the table at once. This parameter helps to
#'   prevent performance issues with very large datasets. The default value is
#'   3000.
#'
#' @return Returns a data frame containing detailed information about the
#'   features specified by `ids` and filtered by `ctr`. If the number of ids
#'   exceeds `max_points`, it returns a tibble with an error message.
#' @noRd
make_feature_info_table <- function(dexset, features, ids, ctr, set_name, max_points = 3000) {
  id <- contrast <- name <- description <- expr <- log_fc <- p_value <- fdr <- NULL

  if (length(ids) <= max_points) {
    df <- dexset[[set_name]]$de |>
      dplyr::filter(id %in% ids & contrast == ctr) |>
      dplyr::left_join(features, by = "id") |>
      dplyr::arrange(name) |>
      dplyr::mutate(name = stringr::str_replace_all(name, ";", "; ")) |>
      dplyr::select(id, Name = name, Description = description, Expression = expr, logFC = log_fc, p_value, FDR = fdr) |>
      dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), ~signif(.x, 3)))
  } else {
    df <- tibble::tibble(Error = stringr::str_glue("Only {max_points} points can be selected."))
  }

  return(df)
}

