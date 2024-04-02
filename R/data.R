#' Expression data for yeast RNA-seq.
#'
#' A subset of 6 + 6 replicates was selected from data set reported in
#' https://doi.org/10.1093/bioinformatics/btv425
#'
#' @usage data(data)
#' @format A tibble with 3 columns
#' @return Expression data from a yeast RNA-seq experiment.
"data"

#' Experimental design from an yeast RNA-seq experiment.
#'
#' A subset of 6 + 6 replicates was selected from data set reported in
#' https://doi.org/10.1093/bioinformatics/btv425
#'
#' @usage data(metadata)
#' @format A tibble with 3 columns
#' @return Design of the experiment containing expression `data` and
#'   differential expression `de`.
"metadata"

#' Differential expression results for yeast RNA-seq.
#'
#' A subset of 6 + 6 replicates was selected from data set reported in
#' https://doi.org/10.1093/bioinformatics/btv425
#'
#' @usage data(de)
#' @format A tibble with 6 columns
#' @return Results for differential expression for yeast RNA-seq.
"de"

#' Feature description for an example RNA-seq experiment.
#'
#' @usage data(features)
#' @format A tibble with 6 columns
#' @return Feature description table.
"features"
