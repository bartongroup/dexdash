
CONFIG <- list(
  title = "Test",
  ontologies = c("go", "kegg", "reactome"),
  max_points = 3000,
  default_data_column = "rpkm"
)

#' Launches an Interactive Differential Expression (DE) Data Explorer
#'
#' This function creates and launches a Shiny application designed for exploring
#' differential expression (DE) data. It integrates various data inputs and
#' initializes interactive visualization modules for an enhanced data
#' exploration experience. The application offers a sidebar layout with themed
#' UI components and a range of interactive modules including global input,
#' volume-magnitude plot, feature plot, feature information, and enrichment
#' analysis.
#'
#' @param de Differential expression data as a data frame, expected to contain
#'   the following columns: `id` - feature id, `log_fc` - log-fold change,
#'   `expr` - expression or abundance, e.g. gene read count, `p_value` - p-value
#'   from the DE test, `fdr` - false discovery rate, which is p-value corrected
#'   for multiple tests, `contrast` - name of the contrast used for the test.
#' @param data A data frame containing the primary dataset for exploration,
#'   expected to contain the following columns: `id` - feature id, the same as
#'   in `de` data frame, `sample` - sample name, the same as in the `metadata`
#'   data frame, `value` - the expression or abundance.
#' @param metadata Metadata associated with the `data` parameter, providing
#'   additional context or grouping for the samples or features included in
#'   `data`. Expected to contain the following columns: `sample` - must match
#'   samples in the `data` object, `group` - a grouping variable, e.g.,
#'   condition or treatment, `replicate` - replicate name.
#' @param features A data frame that maps feature identifiers to names and
#'   descriptions. Expected to contain the following columns: `id` - feature id,
#'   must match the identifier in the `data` object, `name` - human-friendly
#'   name of the feature, e.g. gene symbol, `description` - a brief description
#'   of the feature.
#' @param fterms An object containing functional term information, created using
#'   function \code{download_functional_terms}.
#'
#' @return The function does not return a value but launches a Shiny application
#'   in the user's default web browser, allowing for interactive exploration of
#'   the differential expression data.
#'
#' @examples
#' \dontrun{
#'   de_interactive(de = de_data,
#'                  data = expression_data,
#'                  metadata = sample_metadata,
#'                  features = feature_mapping,
#'                  fterms = feature_terms)
#' }
#' @export
de_interactive <- function(de, data, metadata, features, fterms) {

  data_set <- list(
    de = de,
    data = data,
    metadata = metadata,
    fterms = fterms,
    features = features,
    id2name = rlang::set_names(features$name, features$id),
    name2id = rlang::set_names(features$id, features$name)
  )


  ui <- bslib::page_sidebar(
    theme = bslib::bs_theme(bootswatch = "journal"),
    title = "DE explorer",

    sidebar = bslib::sidebar(
      title = CONFIG$title,
      mod_global_input_ui("global_input")
    ),

    bslib::layout_column_wrap(
      width = 1/3,
      bslib::layout_column_wrap(
        width = 1,
        mod_volma_plot_ui("volma_plot"),
        mod_feature_plot_ui("feature_plot")
      ),
      mod_feature_info_ui("feature_info"),
      mod_enrichment_ui("enrichment"),
    )
  )

  server <- function(input, output, session) {
    # Prevents RStudio from crashing when Shiny window closed manually
    session$onSessionEnded(function() {
      shiny::stopApp()
    })

    # Initialise app state, reactive object for communication between modules
    # Note: an undefined reactive value defaults to NULL, so we can start with an empty list
    app_state <- shiny::reactiveValues()

    # server logic: modules
    mod_global_input_server("global_input", data_set, app_state)
    mod_volma_plot_server("volma_plot", data_set, app_state)
    mod_feature_plot_server("feature_plot", data_set, app_state)
    mod_feature_info_server("feature_info", data_set, app_state)
    mod_enrichment_server("enrichment", data_set, app_state)
    mod_communication_server("communication", data_set, app_state)
  }

  shiny::shinyApp(ui, server)
}
