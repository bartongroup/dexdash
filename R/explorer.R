require(shiny)
require(bslib)


CONFIG <- list(
  title = "Test",
  ontologies = c("go", "kegg", "reactome"),
  max_points = 3000,
  default_data_column = "rpkm"
)
EXPERIMENTS <- list()


de_interactive <- function(de, data, metadata, features, fterms) {

  data_set <- list(
    de = de,
    data = data,
    metadata = metadata,
    fterms = fterms,
    features = features,
    id2name = rlang::set_names(features$name, features$id),
    name2id = rlang::set_names(features$id, features$name)
  )


  ui <- page_sidebar(
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

  server <- function(input, output, session) {
    # Prevents RStudio from crashing when Shiny window closed manually
    session$onSessionEnded(function() {
      stopApp()
    })

    # Initialise app state, reactive object for communication between modules
    # Note: an undefined reactive value defaults to NULL, so we can start with an empty list
    app_state <- reactiveValues()

    # server logic: modules
    mod_global_input_server("global_input", data_set, app_state)
    mod_volma_plot_server("volma_plot", data_set, app_state)
    mod_feature_plot_server("feature_plot", data_set, app_state)
    mod_feature_info_server("feature_info", data_set, app_state)
    mod_enrichment_server("enrichment", data_set, app_state)
    mod_communication_server("communication", data_set, app_state)
  }



  shiny::shinyApp(ui, server)
}
