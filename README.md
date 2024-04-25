# Differential EXpression Downstream Analysis SHiny app

Maintainer: Marek Gierlinski (M.Gierlinski@dundee.ac.uk)

This R package is designed for interactive exploration and analysis of differential expression (DE) results. It combines Shiny-based visualization tools with rapid functional enrichment analysis. Users can select genes from a volcano or MA plot to find their names, descriptions and plot expression patterns. The build-in fast functional enrichment (based on *fenr* package) allows to see instantly which biological pathways and processes those genes are involved in, based on enrichment analysis of Gene Ontology (GO), KEGG, and Reactome databases. The input for the app consists of five data frames containing DE results, expression or abundance data, experimental design, gene names and descriptions. A build-in function allowes for easy downloading of functional term data. Once started, the app is fast and easy to use.


## Installation

```
install.packages("remotes")
remotes::install_github("bartongroup/dexdash", build_vignettes = TRUE)
```

## Example

```
library(dexdash)

# Data examples available in the package
data(yeast_de, yeast_data, yeast_metadata, yeast_features)

# The slow bit: download functional term data
yeast_terms <- download_functional_terms("yeast")
yeast_fterms <- prepare_functional_terms(terms, feature_name = "gene_id")

# The fast bit: interactive app
run_app(yeast_de, yeast_data, yeast_metadata, yeast_features, yeast_fterms)
```

## More information

Please read the package's vignette:

```
vignette("dexdash")
```
