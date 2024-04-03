# VolcEnrich

Maintainer: Marek Gierlinski

Interactive explorer for differential expression data. Centred around an interactive volcano (or MA) plot, allows for manual selection of individual genes or groups of genes, provides plots and fast functional enrichment.

## Installation

```
install.packages("remotes")
remotes::install_github("bartongroup/volcenrich")
```

## Example

```
library(volcenrich)

# Load example data
data(de, data, metadata, features)

# Download functional terms from GO, KEGG and Reactome
fterms <- download_functional_terms("yeast", feature_name = "gene_id")

# Run interactive explorer
run_app(de, data, metadata, features, fterms)
```
