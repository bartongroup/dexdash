# Differential EXpression Downstream Analysis SHiny app

Maintainer: Marek Gierlinski (M.Gierlinski@dundee.ac.uk)

This R package is designed for interactive exploration and analysis of differential expression (DE) results. It combines Shiny-based visualization tools with rapid functional enrichment analysis. Users can select genes from a volcano or MA plot to find their names, descriptions and plot expression patterns. The build-in fast functional enrichment (based on *fenr* package) allows to see instantly which biological pathways and processes those genes are involved in, based on enrichment analysis of Gene Ontology (GO), KEGG, and Reactome databases. The input for the app consists of five data frames containing DE results, expression or abundance data, experimental design, gene names and descriptions. A build-in function allows for easy downloading of functional term data. Once started, the app is fast and easy to use.

## Live demo

Live dexdash demo is available on [our server](https://shiny.compbio.dundee.ac.uk/mgierlinski/public/dexdash_demo/).

## Installation

```
install.packages("remotes")
remotes::install_github("bartongroup/dexdash", build_vignettes = TRUE)
```

## Example

```
# Additional data is required to run this example
remotes::install_github("bartongroup/dexdata")

library(dexdash)
library(dexdata)

# Data examples available in the dexdata package
data(yeast_de, yeast_data, yeast_metadata)

# Create the dexdash data set
yeast_dexset <- dexdash_set(yeast_de, yeast_data, yeast_metadata, name = "Yeast")

# The slow bit: download feature information and functional term data
yeast_features <- download_feature_information(species = "yeast")
yeast_terms <- download_functional_terms(species = "yeast")
yeast_fterms <- prepare_functional_terms(yeast_terms, feature_name = "gene_id")

# The fast bit: interactive app
run_app(yeast_dexset, yeast_features, yeast_fterms)
```

## More information

The package [tutorial](https://www.compbio.dundee.ac.uk/user/mgierlinski/dexdash/dexdash.html) and the [reference manual](https://www.compbio.dundee.ac.uk/user/mgierlinski/dexdash/dexdash.pdf) are available on our server.

