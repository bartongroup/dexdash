# Module FUNCTIONAL ENRICHMENT
#
# Creates a table with functional enrichment for a selection of features from a
# brush.
#
# Input:
#    state$sel_functional_enrichment - selection of feature IDs to use in functional enrichment.
#
# Output:
#    state$sel_term - feature IDs corresponding to the functional term selected in the table.
#
# Uses:
#    data_set$fterms - a list of fterm objects prepared by fterm
#    data_set$de - differential expression results (only to find the full list of featrures)
#

# ----- UI definitions -----

mod_enrichment_ui <- function(id) {
  ns <- shiny::NS(id)

  ontology <- shinyWidgets::radioGroupButtons(
    inputId = ns("ontology"),
    label = "Ontology",
    choices = ""
  )

  fdr_limit <- shiny::numericInput(
    inputId = ns("fdr_limit"),
    label = "FDR limit",
    value = 0.05,
    min = 0,
    max = 1
  )

  gear <- bslib::popover(
    bsicons::bs_icon("gear"),
    ontology,
    fdr_limit
  )

  info <- bslib::popover(
    bsicons::bs_icon("info-circle"),
    htmltools::includeMarkdown("man/helpers/enrichment.md"),
    options = list(customClass = "info-pop")
  )

  bslib::card(
    bslib::card_header(
      "Functional enrichment",
      shiny::span(info, gear),
      class = "d-flex justify-content-between"
    ),

    DT::dataTableOutput(
      outputId = ns("enrichment")
    )
  )

}

# ----- Server logic -----

mod_enrichment_server <- function(id, data_set, state) {

  server <- function(input, output, session) {

    # Update dummy ontology selections from data
    ontologies <- names(data_set$fterms)
    shiny::observe({
      shinyWidgets::updateRadioGroupButtons(
        session = session,
        inputId = "ontology",
        choices = ontologies
      )
    })

    # Observe state$sel_brush - a selection brushed from a Volcano/MA plot.
    # Create and return a functional enrichment table using fenr. Requires
    # data_set$de - differential expression results and data_set$fterms - prepared for
    # fenr.
    enrichment_table <- shiny::reactive({
      sel_ids <- state$sel_functional_enrichment
      shiny::req(!is.null(sel_ids) & length(sel_ids) > 1)
      all_ids <- unique(data_set$de$id)
      make_functional_enrichment(sel_ids, all_ids, data_set$fterms[[input$ontology]], data_set$id2name, input$fdr_limit)
    })

    # Wait for row selection in the enrichment table, find annotated features,
    # pass it to the app state.
    shiny::observeEvent(input$enrichment_rows_selected, ignoreNULL = FALSE, {
      rows_sel <- input$enrichment_rows_selected
      if(!is.null(rows_sel)) {
        fe <- enrichment_table()
        term_id <- fe[rows_sel, ]$TermId
        ids <- fenr::get_term_features(data_set$fterms[[input$ontology]], term_id)
      } else {
        ids <- NULL
      }
      state$sel_term <- ids
    })

    output$enrichment <- DT::renderDataTable({
      fe <- enrichment_table()
      DT::datatable(
        fe,
        options = list(paging = FALSE, dom = "t"),
        style = "bootstrap",
        selection = "single",
        rownames = FALSE
      ) |>
        DT::formatStyle(columns = colnames(fe), fontSize = '80%')
    })


  }

  shiny::moduleServer(id, server)
}


# ----- Functions used in the module -----


#' Make functional enrichment table
#'
#' @param sel_ids Selected feature IDs
#' @param all_ids All feature IDs (background)
#' @param trms An fterm object with functional term data
#' @param id2name A named vector converting feature IDs into feature names
#' @param fdr_limit FDR limit for selection of terms
#' @param max_points Maximum number of points selectable
#'
#' @return A tibble with significant functional terms
#' @noRd
make_functional_enrichment <- function(sel_ids, all_ids, trms, id2name, fdr_limit, max_points = 3000) {
  p_adjust <- term_id <- term_name <- n_with_sel <- odds_ratio <- ids <- NULL

  n <- length(sel_ids)

  fe <- NULL
  if(n > 1 && n <= max_points) {
    fe <- fenr::functional_enrichment(all_ids, sel_ids, trms, id2name)
    if(!is.null(fe)) {
      fe <- fe |>
        dplyr::filter(p_adjust < fdr_limit) |>
        dplyr::select(TermId = term_id, Name = term_name, n = n_with_sel, OR = odds_ratio, ids)
    }
  } else if (n > max_points) {
    fe <- tibble::tibble(Error = stringr::str_glue("Only {max_points} points can be selected."))
  }
  fe
}


