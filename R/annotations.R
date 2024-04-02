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
#' @param feature_name The name of the column in the \code{mapping} tibble to be
#'   used as the feature identifier. It can be "gene_symbol" or "gene_id". If
#'   your data contain gene symbols (e.g. "BRCA1" or "FOXP1"), use \code{feature_name
#'   = "gene_symbol"}. If your data contain Ensembl identifiers (e.g. "ENSG00000012048"
#'    or  "ENSG00000114861"), use \code{feature_name
#'   = "gene_id"}.
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
#' \dontrun{
#' terms <- download_functional_terms(species = "yeast", feature_name = "gene_id")
#' }
#' @export
download_functional_terms <- function(species, species_file = NULL,
                                      feature_name = c("gene_symbol" ,"gene_id"), all_features = NULL) {
  feature_name <- match.arg(feature_name)

  if(!is.null(species_file)) {
    assertthat::assert_that(file.exists(species_file))
    sf <- species_file
  } else {
    sf <- system.file("extdata", "species.json", package = "volcenrich")
    #sf <- "inst/extdata/species.json"
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


