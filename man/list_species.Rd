% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/explorer.R
\name{list_species}
\alias{list_species}
\title{List Available Species}
\usage{
list_species(species_file = NULL)
}
\arguments{
\item{species_file}{Optional path to the species JSON file. If 'NULL', the
default file, available at \code{system.file("extdata", "species.json",
package = "dexdash")} will be used.}
}
\value{
A character vector of species names.
}
\description{
Retrieves a list of species names used in the package. These names are
specified available in the JSON configuration file.
}
