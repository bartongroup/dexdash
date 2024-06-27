#' Load Species Data from JSON File
#'
#' Loads species configuration data from a JSON file. If a file is not specified,
#' it attempts to load the default species.json from the package's 'extdata' directory.
#'
#' @param species_file A string specifying the path to the species JSON file. If
#'        NULL, the function will attempt to load the default file included with
#'        the package.
#'
#' @return A list containing species data.
#' @noRd
load_species_file <- function(species_file = NULL) {
  if(!is.null(species_file)) {
    assertthat::assert_that(file.exists(species_file))
    sf <- species_file
  } else {
    sf <- system.file("extdata", "species.json", package = "dexdash")
  }

  jsonlite::read_json(sf)
}

#' Load Specific Species Configuration
#'
#' Extracts and validates the configuration for a specified species from a JSON data structure.
#' Ensures that the required fields and sub-fields are present in the JSON.
#'
#' @param species The name of the species to retrieve configurations for.
#' @param species_file Optional custom path to the species JSON file.
#' @param field_names A vector of top-level fields expected in the species data.
#' @param ontology_names A vector of expected ontology field names.
#' @param ensembl_names A vector of expected Ensembl field names.
#'
#' @return A list containing the configuration data for the specified species.
#' @noRd
load_species <- function(species, species_file = NULL, field_names = c("ensembl", "ontology"),
                              ontology_names = c("go", "kegg", "reactome"),
                              ensembl_names = c("biomart", "dataset", "host")) {

  all_sp <- load_species_file(species_file)

  names_str <- stringr::str_c(names(all_sp), collapse = ", ")
  assertthat::assert_that(species %in% names(all_sp),
    msg = stringr::str_glue("Species '{species}' not recognized. Valid species names are {names_str}.")
  )

  sp <- all_sp[[species]]
  field_str <- stringr::str_c(field_names, collapse = ", ")
  assertthat::are_equal(sort(names(sp)), sort(field_names),
    msg = stringr::str_glue("Incorrect fields for species {species} in JSON file. Required names are {field_str}.")
  )

  ont_str <- stringr::str_c(ontology_names, collapse = ", ")
  assertthat::assert_that(all(names(sp$ontology) %in% ontology_names),
    msg = stringr::str_glue("Incorrect ontology names(s) for species {species} in JSON file. Valid names are {ont_str}.")
  )

  ens_str <- stringr::str_c(ensembl_names, collapse = ", ")
  assertthat::are_equal(sort(names(sp$ensembl)), sort(ensembl_names),
    msg = stringr::str_glue("Incorrect Ensembl field names(s) for species {species} in JSON file. Required names are {ens_str}.")
  )

  return(sp)
}


#' List Available Species
#'
#' Retrieves a list of species names used in the package. These names are
#' specified available in the JSON configuration file.
#'
#' @param species_file Optional path to the species JSON file. If 'NULL', the
#'   default file, available at \code{system.file("extdata", "species.json",
#'   package = "dexdash")} will be used.
#'
#' @return A character vector of species names.
#' @export
list_species <- function(species_file = NULL) {
  sp <- load_species_file(species_file)
  names(sp)
}

#' Download Functional Annotation Terms
#'
#' Retrieves Gene Ontology (GO), Reactome, and KEGG terms for a given species.
#'
#' @param species A character string specifying the species for which to
#'   download data. Available species can be listed using function
#'   `list_species()`.
#' @param species_file (Optional) A character string providing the path to a
#'   JSON file containing species information. If `NULL`, the default
#'   species.json file from the package will be used. See Details for more
#'   information.
#' @return A list of three elements named "go", "reactome" and "kegg", each
#'   containing two data frames with term descriptions and feature mapping.
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
#' \dontrun{
#' fterms <- download_functional_terms(species = "yeast", feature_name = "gene_id")
#' }
#' @export
download_functional_terms <- function(species, species_file = NULL) {

  sp <- load_species(species, species_file)

  message("Downloading GO data")
  go <- fenr::fetch_go(species = sp$ontology$go)
  message("Downloading Reactome data")
  reactome <- fenr::fetch_reactome(species = sp$ontology$reactome)
  message("Downloading KEGG data")
  kegg <- fenr::fetch_kegg(species = sp$ontology$kegg)

  # Backwards compatibility with fenr before 1.0.6
  if("gene_synonym" %in% colnames(go$mapping))
    go$mapping$gene_id <- go$mapping$gene_synonym

  # Convert gene synonyms to upper case
  go$mapping$gene_symbol <- toupper(go$mapping$gene_symbol)
  reactome$mapping$gene_symbol <- toupper(reactome$mapping$gene_symbol)
  kegg$mapping$gene_symbol <- toupper(kegg$mapping$gene_symbol)

  list(
    go = go,
    reactome = reactome,
    kegg = kegg
  )
}


#' Prepare functional terms for fast enrichment
#'
#' @param terms A list of three elements, named "go", "reactome" and "kegg".
#'   Each element is a list with two data frames, "terms" and "mapping". See
#'   \code{vignette("fenr")} for more details. In a normal workflow, this object
#'   is created with \code{download_functional_terms}.
#' @param feature_name The name of the column in the \code{mapping} tibble to be
#'   used as the feature identifier. It can be "gene_symbol" or "gene_id". If
#'   your data contain gene symbols (e.g. "BRCA1" or "FOXP1"), use \code{feature_name
#'   = "gene_symbol"}. If your data contain other identifiers (e.g. "ENSG00000012048"
#'    or  "ENSG00000114861"), use \code{feature_name
#'   = "gene_id"}.
#' @param all_features (Optional) A vector of all possible features (such as
#'   gene symbols) to prepare the data for enrichment analysis.
#'
#' @return An object to be used by the Shiny app.
#'
#' @examples
#' \dontrun{
#' terms <- download_functional_terms(species = "yeast")
#' fterms <- prepare_functional_terms(terms)
#' }
#' @export
prepare_functional_terms <- function(terms, feature_name = c("gene_symbol" ,"gene_id"), all_features = NULL) {
  feature_name <- match.arg(feature_name)

  ontologies <- names(terms)
  purrr::map(ontologies, function(ont) {
    trm <- terms[[ont]]
    fenr::prepare_for_enrichment(trm$terms, trm$mapping, all_features, feature_name = feature_name)
  }) |>
    rlang::set_names(ontologies)
}


#' Download feature (gene or protein) information for a given species
#'
#' Connects to the Ensembl BioMart for the specified species and downloads
#' feature information including feature IDs, gene symbols, and descriptions. It
#' cleans up the gene descriptions and handles missing gene names and duplicate
#' entries.
#'
#' @param species A character string specifying the species for which to
#'   download data. Available species can be listed using function
#'   `list_species()`.
#' @param species_file (Optional) A character string providing the path to a
#'   JSON file containing species information. If `NULL`, the default
#'   species.json file from the package will be used. See Details for more
#'   information.
#' @param id A string with the BioMart attribute used as identifier. Default
#'   value is "ensembl_gene_id", referring to Ensembl gene ID. For UniProt
#'   identifiers, use "uniprotswissprot"; for NCBI identifiers use
#'   "entrezgene_id".
#' @details BioMart requires a biomart name, a host name and a dataset, to
#'   connect to the right database. For example, for human, these arguments are
#'   "ensembl", "https://www.ensembl.org", and "hsapiens_gene_ensembl",
#'   respectively. This package contains a small JSON file
#'   (can be found at \code{system.file("extdata", "species.json", package =
#'   "dexdash")}), with Ensembl information for a few species. If
#'   your species is not included, you need to create a JSON file in the same
#'   format, as the included file.
#'
#' @return A tibble containing gene IDs, gene names (cleaned), and gene
#'   descriptions (cleaned).
#' @examples
#' \dontrun{
#' gene_info <- download_gene_information("mouse")
#' }
#' @export
download_feature_information <- function(species, species_file = NULL, id = "ensembl_gene_id") {
  name <- description <- NULL

  sp <- load_species(species, species_file)

  mart <- biomaRt::useMart(biomart = sp$ensembl$biomart, host = sp$ensembl$host, dataset = sp$ensembl$dataset)
  attr <- c(
    id = id,
    name = "external_gene_name",
    description = "description"
  )

  biomaRt::getBM(attributes = attr, mart = mart, useCache = TRUE) |>
    tibble::as_tibble() |>
    dplyr::rename(!!attr) |>
    dplyr::mutate(
      id = as.character(id),
      id = dplyr::na_if(id, ""),
      description = stringr::str_remove(description, "(;|\\s\\[).+$"),
      name = dplyr::na_if(name, "")
    ) |>
    dplyr::mutate(name = dplyr::if_else(is.na(name), id, name)) |>
    # If UniProt is used as ID, there can be multiple entires per id
    dplyr::group_by(id) |>
    dplyr::summarise(
      name = stringr::str_c(name, collapse = ";"),
      description = stringr::str_c(description, collapse = ";")
    )
}


#' Create *dexdash* data set
#'
#' This function creates a data set for *dexdash* Shiny app, based on
#' user-provided data.
#'
#' @param de Differential expression data as a data frame, expected to contain
#'   the following columns: `id` - feature id, `log_fc` - log-fold change,
#'   `expr` - expression or abundance, e.g. gene read count, `p_value` -
#'   uncorrected p-value from the DE test, `contrast` - name of the contrast
#'   used for the test.
#' @param data A data frame containing the primary dataset for exploration,
#'   expected to contain the following columns: `id` - feature id, the same as
#'   in `de` data frame, `sample` - sample name, the same as in the `metadata`
#'   data frame, `value` - the expression or abundance.
#' @param metadata Metadata associated with the `data` parameter, providing
#'   additional context or grouping for the samples or features included in
#'   `data`. Expected to contain the following columns: `sample` - must match
#'   samples in the `data` object, `group` - a grouping variable, e.g.,
#'   condition or treatment, `replicate` - replicate name.
#' @param name A string to identify the name of the set.
#'
#' @return An object of class \code{dexdash_set} required by the Shiny app
#'   launcher \code{run_app}.
#' @export
#'
#' @examples
#' data(yeast_de, yeast_data, yeast_metadata)
#' dexset <- dexdash_set(yeast_de, yeast_data, yeast_metadata, "Yeast")
dexdash_set <- function(de, data, metadata, name) {
  assert_colnames(de, c("id", "log_fc", "expr", "p_value", "contrast"), deparse(substitute(de)))
  assert_colnames(data, c("id", "sample", "value"), deparse(substitute(data)))
  assert_colnames(metadata, c("sample"), deparse(substitute(metadata)))
  assertthat::assert_that(ncol(metadata) > 1,
    msg = "metadata requires at least one additional column apart from 'sample'.")
  assertthat::is.string(name)

  list(
    de = de,
    data = data,
    metadata = metadata,
    name = name
  ) |>
    structure(class = "dexdash_set")
}


#' Create a list of *dexdash* data sets.
#'
#' Merge multiple \code{dexdash_set} objects into a \code{dexdah_list}. This
#' function is used to merge multiple data sets into one object, that can be fed
#' into the Shiny app. This allows for browsing results from, e.g., different
#' experiments in one app.
#'
#' @param ... One or more objects of class `dexdash_set`.
#'
#' @return A named list of `dexdash_set` objects with class attribute
#'   `dexdash_list`.
#' @export
dexdash_list <- function(...) {
  dexes <- list(...)

  purrr::map(dexes, ~assertthat::assert_that(is(.x, "dexdash_set")))
  names <- purrr::map_chr(dexes, function(x) x$name)
  assertthat::assert_that(length(names) == length(unique(names)),
              msg = "dexdash names must be unique")

  rlang::set_names(dexes, names) |>
    structure(class = "dexdash_list")
}


#' Launches an interactive differential expression (DE) data explorer
#'
#' This function creates and launches a Shiny application designed for exploring
#' differential expression (DE) data. It integrates various data inputs and
#' initializes interactive visualization modules for an enhanced data
#' exploration experience. The application offers a sidebar layout with themed
#' UI components and a range of interactive modules including global input,
#' volume-magnitude plot, feature plot, feature information, and enrichment
#' analysis.
#'
#' @param dexset Either a \code{dexdash_set} object containing data from one
#'   set, or a \code{dexdash_list} object containing data from multiple sets.
#'   The former one is created using \code{dexdash_set()} function, the latter
#'   one is created with \code{dexdash_list()} function.
#' @param features A data frame that maps feature identifiers to names and
#'   descriptions. Expected to contain the following columns: `id` - feature id,
#'   must match the identifier in the `data` object, `name` - human-friendly
#'   name of the feature, e.g. gene symbol, `description` - a brief description
#'   of the feature. This data frame can be obtained using function
#'   \code{download_gene_informarion()}.
#' @param fterms An object containing functional term information, created using
#'   function \code{download_functional_terms}.
#' @param title A string with a short title, which is presented at the top of
#'   the side bar.
#'
#' @return The function does not return a value but launches a Shiny application
#'   in the user's default web browser, allowing for interactive exploration of
#'   the differential expression data.
#'
#' @importFrom methods is
#' @importFrom stats p.adjust
#'
#' @examples
#' if(interactive()) {
#'   data(de, data, metadata, features)
#'   dexset <- dexdash_set(de, data, metadata, "Yeast")
#'   fterms <- download_functional_terms("yeast", feature_name = "gene_id")
#'   run_app(dexset, features, fterms)
#' }
#' @export
run_app <- function(dexset, features, fterms, title = "DE explorer") {
  p_value <- contrast <- NULL

  assertthat::assert_that(is(dexset, "dexdash_set") | is(dexset, "dexdash_list"))
  assert_colnames(features, c("id", "name", "description"), deparse(substitute(features)))
  assertthat::assert_that(is(fterms, "list"))
  assertthat::is.string(title)
  purrr::map(fterms, ~assertthat::assert_that(is(.x, "fenr_terms"),
    msg = "fterms argument needs to be a list of 'fenr_terms' objects. Did you run 'prepare_functional_terms'?"))

  if(is(dexset, "dexdash_set")) {
    dexset <- list(dexset) |> rlang::set_names(dexset$name)
  }

  # Mutliple test corrections
  for(nm in names(dexset)) {
    dexset[[nm]]$de <- dexset[[nm]]$de |>
      dplyr::group_by(contrast) |>
      dplyr::mutate(fdr = p.adjust(p_value, method = "BH")) |>
      dplyr::ungroup()
  }

  data_set <- list(
    dex = dexset,
    names = names(dexset),
    fterms = fterms,
    features = features,
    id2name = rlang::set_names(features$name, features$id),
    name2id = rlang::set_names(features$id, features$name)
  )

  version <- paste("version", utils::packageVersion("dexdash"))

  ui <- bslib::page_sidebar(
    theme = bslib::bs_theme(bootswatch = "spacelab", spacer = "0.8rem") |>
      bslib::bs_add_rules(".info-pop { max-width: 500px; background-color: #f0f9e8; }"),
    title = "DEXDASH",

    sidebar = bslib::sidebar(
      title = title,
      mod_global_input_ui("global_input"),
      shiny::tags$hr(),
      shiny::tags$span(style = "font-size: 0.7em; color: #191970", version)
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
    app_state <- new_app_state()

    # server logic: modules
    mod_global_input_server("global_input", data_set, app_state)
    mod_volma_plot_server("volma_plot", data_set, app_state)
    mod_feature_plot_server("feature_plot", data_set, app_state)
    mod_enrichment_server("enrichment", data_set, app_state)
    mod_feature_info_server("feature_info", data_set, app_state)
    mod_communication_server("communication", data_set, app_state)
  }

  shiny::shinyApp(ui, server)
}
