#' Download Functional Annotation Terms
#'
#' Retrieves Gene Ontology (GO), Reactome, and KEGG terms for a given species
#' and prepares them to use with the Shiny app.
#'
#' @param species A character string specifying the species for which to
#'   download data.
#' @param species_file (Optional) A character string providing the path to a
#'   JSON file containing species information. If `NULL`, the default
#'   species.json file from the package will be used. See Details for more
#'   information.
#' @param feature_name The name of the column in the \code{mapping} tibble to be
#'   used as the feature identifier. It can be "gene_symbol" or "gene_id". If
#'   your data contain gene symbols (e.g. "BRCA1" or "FOXP1"), use \code{feature_name
#'   = "gene_symbol"}. If your data contain Ensembl identifiers (e.g. "ENSG00000012048"
#'    or  "ENSG00000114861"), use \code{feature_name
#'   = "gene_id"}.
#' @param all_features (Optional) A vector of all possible features (such as
#'   gene symbols) to prepare the data for enrichment analysis.
#' @return A list of three elements named "go", "reactome" and "kegg", each
#'   containing \code{fenr_terms} objects required for fast functional
#'   enrichment.
#' @details The GO, KEGG and Reactome databases use different species
#'   designation names. For example, designation for yeast is "sgd",
#'   "Saccharomyces cerevisiae" and "sce", for GO, Reactome and KEGG,
#'   respectively. In oder to interrogate these databases, the correct
#'   designations must be passed on. This package contains a small JSON file
#'   (can be found at \code{system.file("extdata", "species.json", package =
#'   "dexdash")}), with designation information for a few species. If
#'   your species is not included, you need to create a JSON file in the same
#'   format, as the included file. The species designations can be found using
#'   `fenr::fetch_go_species()`, `fenr::fetch_reactome_species()` and
#'   `fenr::fetch_kegg_species()`. These three functions return data frames,
#'   where column `designation` contains the species designation required.
#' @examples
#' fterms <- download_functional_terms(species = "yeast", feature_name = "gene_id")
#' @export
download_functional_terms <- function(species, species_file = NULL,
                                      feature_name = c("gene_symbol" ,"gene_id"), all_features = NULL) {
  feature_name <- match.arg(feature_name)

  if(!is.null(species_file)) {
    assertthat::assert_that(file.exists(species_file))
    sf <- species_file
  } else {
    sf <- system.file("extdata", "species.json", package = "dexdash")
  }

  all_sp <- jsonlite::read_json(sf)
  assertthat::assert_that(species %in% names(all_sp))
  sp <- all_sp[[species]]

  message("Loading GO term data")
  go <- fenr::fetch_go(species = sp$go)
  message("Loading Reactome data")
  re <- fenr::fetch_reactome(sp$reactome)
  message("Loading KEGG data")
  kg <- fenr::fetch_kegg(sp$kegg)

  terms <- list(
    go = go,
    reactome = re,
    kegg = kg
  )

  message("Preparing data for fenr.")
  ontologies <- names(terms)
  purrr::map(ontologies, function(ont) {
    trm <- terms[[ont]]
    fenr::prepare_for_enrichment(trm$terms, trm$mapping, all_features, feature_name = feature_name)
  }) |>
    rlang::set_names(ontologies)
}




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
#' if(interactive()) {
#'   data(de, data, metadata, features)
#'   fterms <- download_functional_terms("yeast", feature_name = "gene_id")
#'   run_app(de, data, metadata, features, fterms)
#' }
#' @export
run_app <- function(de, data, metadata, features, fterms) {

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
    title = "Volcano enricher",

    sidebar = bslib::sidebar(
      title = "DE explorer",
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
