# DIFFERENTIAL EXPRESSION EXPLORER
#
# Main UI
#

require(bslib)

page_sidebar(
  theme = bs_theme(bootswatch = "journal"),
  title = "DE explorer",

  sidebar = sidebar(
    title = CONFIG$title,
    mod_global_input_ui("global_input")
  ),

  layout_column_wrap(
    width = 1/3,
    layout_column_wrap(
      width = 1,
      mod_volma_plot_ui("volma_plot"),
      mod_feature_plot_ui("feature_plot")
    ),
    mod_feature_info_ui("feature_info"),
    mod_enrichment_ui("enrichment"),
  )
)

