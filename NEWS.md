## Version 0.1.0

 - First version
 
 ## Version 0.1.1
 
  - A lot of minor tweaks
  - Documentation updates

## Version 0.1.2

 - Added functions `list_species()` and `download_feature_information()`.
 - Split function `download_functional_terms()` into `download_functional_terms()` and `prepare_functional_terms()` to allow for more flexibility with feature identifiers.
 - Wrote new section of the vignette with an advanced example.
 - Added mouse RNA-seq data for the example.
 - Added Ensembl settings for all species in the `species.json` file.

## Version 0.1.3

 - Bug fixes
 - Lazy data recompression
 
## Version 0.1.4

 - Fixed a bug in SelectizeInput
 - Improving the vignette
 - Reducing size of data
 
## Version 0.1.5
 
  - Fixed missing helpers (pop-up documents when info icon clicked)

## Version 0.1.6

 - New feature: a title can be added to the dashboard
 
## Version 0.1.7

 - New feature: additional columns can be provided in `metadata` to colour points in the feature plot

## Version 0.2.0

 - Data structure overhaul - all user data are now packed into one object class `dexdash_set` or `dexdash_list`.
 - Allows for multiple data sets, selected from the drop-down menu
 - New functions: `dexdash_set()` and `dexdash_list()`.
 
## Version 0.2.1

 - Improvements to the documentation
 - Changed column names in example metadata to better reflect the nature of the variable
 - Small tweaks
 
## Version 0.2.2

 - New feature: each plot and table can be now downloaded by clicking the "download" icon
 - Minor improvements to the code

## Version 0.2.3

 - A few minor changes

## Version 0.2.4

 - Fixed a bug where NAs in any metadata column caused filtering data out in the feature plot

## Version 0.2.5

 - Moved all example data to a separate package bartongroup/dexdata to reduce footprint
 - Move the large vignette to a separate directory and created minimal vignette to reduce footprint
 - Fixed a bug in feature info testing
 - Added new  `run_app` arguments to set the startup values of the x- and colour-variables in the feature plot
 - Added ability to provide own FDR column in differential expression table

## Version 0.2.6

 - Fixed an issue with mean normalisation in the feature plot that tried to take a logarithm of zero by calculating a log-ratio.
 - Minor tweaks to the plot feature code.
 - Fixing `dexdata` package references.

## Version 0.2.7

 - Bug fix: missing x-axis column error if `x_variable` argument not specified in `run_app`.

## Version 0.2.8

 - Bug fix: card_body() was missing in three cards.

## Version 0.2.9

 - Typo fix
 - Allow for continuous (numeric) variables for x-axis and colour scale in feature plot
 
## Version 0.2.10

 - Changed the default FDR limit from 0.01 to 0.05 (I know!)

## Version 0.2.12

 - Make sure that the y axis in the volcano plot always starts from zero, even if there are no data points near zero.

## Version 0.2.13

 - Replaced shinyWidgets::radioGroupButtons with shiny::selectInput in enrichment panel, because with larger amount of ontologies the selection box went outside the browser window and became invisible.