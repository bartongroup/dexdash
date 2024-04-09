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
#' @format A tibble with 3 columns
#' @return Design of the experiment containing expression `data` and
#'   differential expression `de`.
"yeast_metadata"

#' Differential expression results for yeast RNA-seq.
#'
#' A subset of 6 + 6 replicates was selected from data set reported in
#' https://doi.org/10.1093/bioinformatics/btv425
#'
#' @usage data(yeast_de)
#' @format A tibble with 6 columns
#' @return Results for differential expression for yeast RNA-seq.
"yeast_de"

#' Feature description for an example RNA-seq experiment.
#'
#' @usage data(yeast_features)
#' @format A tibble with 6 columns
#' @return Feature description table.
"yeast_features"