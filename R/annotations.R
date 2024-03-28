#' Download Functional Annotation Terms
#'
#' Retrieves Gene Ontology (GO), Reactome, and KEGG terms for a given species
#' and prepares them to use with the Shiny app.
#'
#' @param species A character string specifying the species for which to
#'   download data.
#' @param species_file (Optional) A character string providing the path to a
#'   JSON file containing species information. If `NULL`, the default
#'   species.json file from the package will be used.
#' @param all_features (Optional) A vector of all possible features (such as
#'   gene symbols) to prepare the data for enrichment analysis.
#' @return A list of three elements, each containing data frames with the terms
#'   and mappings for GO, Reactome, and KEGG data respectively. The list
#'   elements are named "go", "reactome", and "kegg".
#' @details This function checks for the existence of a specified species file
#'   and verifies that the provided species is present within it. It then
#'   proceeds to load the term data using `fenr` functions for the different
#'   biological databases. If `all_features` is provided, it also prepares the
#'   data for enrichment analysis with `fenr::prepare_for_enrichment`.
#' @examples
#' terms <- download_functional_terms(species = "yeast")
#' @export
download_functional_terms <- function(species, species_file = NULL, all_features = NULL) {

  if(!is.null(species_file)) {
    assertthat::assert_that(file.exists(species_file))
    sf <- species_file
  } else {
    #sf <- system.file("extdata", "species.json", package = "DEexplorer")
    sf <- "inst/extdata/species.json"
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
    fenr::prepare_for_enrichment(trm$terms, trm$mapping, all_features, feature_name = "gene_symbol")
  }) |>
    rlang::set_names(ontologies)
}


CONFIG <- list(
  title = "Test",
  ontologies = c("go", "kegg", "reactome"),
  max_points = 3000,
  default_data_column = "rpkm"
)
EXPERIMENTS <- list()

