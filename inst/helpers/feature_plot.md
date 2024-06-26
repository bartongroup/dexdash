## Feature plot

This card displays the expression or abundance levels of selected features (genes or proteins). For a single feature, it presents an intensity versus group plot. When multiple features are selected, it shows a heatmap of features against samples or groups.

### Plot settings

Adjust your preferences by clicking the gear icon:
 - 'X-axis variable' - variable name (column name in the `metdata` data frame) to use on the X axis
 - 'Colour variable' - variable name (column name in the `metdata` data frame) to set colour to points
 - 'Heatmap normalised per row' - if enabled, each row (gene) is normalised to its own mean; if unselected, the original expression or abundance data are shown
 - 'Intensity scale' - Choose between a linear or logarithmic scale. This option is unavailable when normalization to row is active.
 
