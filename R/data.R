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

#' Functional terms for yeast.
#'
#' @usage data(mouse_fterms)
#' @format A 'fenr_terms' object
#' @return Functional terms for yeast.
"yeast_fterms"


#' Expression data for mouse RNA-seq in wide matrix format.
#'
#' @usage data(mouse_mtx)
#' @format A matrix with 50892 rows and 16 columns.
#' @return Expression data from a mouse RNA-seq experiment.
"yeast_mtx"

#' Feature description for an example RNA-seq experiment.
#'
#' @usage data(mouse_genes)
#' @format A tibble with 3 columns
#' @return Feature description table, downloaded from Ensembl.
"mouse_genes"


#' Functional terms for mouse
#'
#' @usage data(mouse_terms)
#' @format A list with data frames
#' @return Functional terms for mouse.
"mouse_terms"
