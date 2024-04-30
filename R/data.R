#' Expression data for yeast RNA-seq.
#'
#' A subset of 6 + 6 replicates was selected from data set reported in
#' https://doi.org/10.1093/bioinformatics/btv425
#'
#' @usage data(yeast_data)
#' @format A tibble with 3 columns
#' @return Expression data from a yeast RNA-seq experiment.
"yeast_data"

#' Experimental design from an yeast RNA-seq experiment.
#'
#' A subset of 6 + 6 replicates was selected from data set reported in
#' https://doi.org/10.1093/bioinformatics/btv425
#'
#' @usage data(yeast_metadata)
#' @format A tibble with 2 columns
#' @return Design of the experiment containing expression `data` and
#'   differential expression `de`.
"yeast_metadata"

#' Differential expression results for yeast RNA-seq.
#'
#' A subset of 6 + 6 replicates was selected from data set reported in
#' https://doi.org/10.1093/bioinformatics/btv425
#'
#' @usage data(yeast_de)
#' @format A tibble with 5 columns
#' @return Results for differential expression for yeast RNA-seq.
"yeast_de"

#' Feature description for an example RNA-seq experiment.
#'
#' @usage data(yeast_features)
#' @format A tibble with 3 columns
#' @return Feature description table, downloaded from Ensembl.
"yeast_features"

#' Expression data for yeast RNA-seq in wide matrix format.
#'
#' A subset of 6 + 6 replicates was selected from data set reported in
#' https://doi.org/10.1093/bioinformatics/btv425
#'
#' @usage data(yeast_mtx)
#' @format A matrix with 6298 rows and 12 columns.
#' @return Expression data from a yeast RNA-seq experiment.
"yeast_mtx"

#' Expression data for mouse RNA-seq in wide matrix format.
#'
#' A subset of 3 time points from data reported in
#' https://doi.org/10.1016/j.celrep.2021.109943
#'
#' @usage data(mouse_mtx)
#' @format A matrix with 29134 rows and 12 columns.
#' @return Expression data from a mouse RNA-seq experiment.
"mouse_mtx"

#' Feature description for an example RNA-seq experiment.
#'
#' @usage data(mouse_genes)
#' @format A tibble with 3 columns
#' @return Feature description table, downloaded from Ensembl.
"mouse_genes"


#' Functional terms for mouse
#'
#' Downloaded using \code{mouse_fterms <- prepare_functional_terms(mouse_terms,
#' feature_name = "gene_symbol")}.
#'
#' @usage data(mouse_terms)
#' @format A list with data frames
#' @return Functional terms for mouse.
"mouse_terms"
