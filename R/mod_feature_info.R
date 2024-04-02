# Module FEATURE INFO
#
# Creates an DT table with basing feature information, from a selection of feature IDs.
#
# Input:
#    state$sel_feature_info - selection of feature IDs to display
#    state$contrast - which contrast information to display
#
# Output:
#    state$sel_tab - one feature ID selected in this info table
#
# Uses:
#    DATA$de - a tibble with differential expression values
#    DATA$features - a tibble with feature ID and feature name
#

require(DT)
require(dplyr)
require(tibble)
require(shiny)

# ----- UI definitions -----

mod_feature_info_ui <- function(id) {
  ns <- NS(id)

  card(
    card_header("Feature information"),
    DT::dataTableOutput(
      outputId = ns("feature_info")
    )
  )

}

# ----- Server logic -----

mod_feature_info_server <- function(id, data_set, state) {

  server <- function(input, output, session) {

    make_feature_info_table <- function(de, ids, ctr) {
      if (length(ids) <= CONFIG$max_points) {
        df <- de |>
          dplyr::filter(id %in% ids & contrast == ctr) |>
          dplyr::left_join(data_set$features, by = "id") |>
          dplyr::arrange(name) |>
          dplyr::mutate(name = str_replace_all(name, ";", "; "))
      } else {
        df <- tibble::tibble(Error = str_glue("Only {CONFIG$max_points} points can be selected."))
      }
      return(df)
    }

    # Prepare updated feature info table
    feature_info_table <- reactive({
      ids <- state$sel_feature_info
      ctr <- state$contrast
      req(ids, ctr)
      make_feature_info_table(data_set$de, ids, ctr)
    })

    # Wait for row selection in the feature info table, pass it to app state
    observeEvent(input$feature_info_rows_selected, ignoreNULL = FALSE, {
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
          dplyr::mutate(across(c(logFC, FDR), ~signif(.x, 3)))
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

  moduleServer(id, server)
}
