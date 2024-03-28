# DIFFERENTIAL EXPRESSION EXPLORER
#
# Main server
#


server <- function(input, output, session) {
  # Prevents RStudio from crashing when Shiny window closed manually
  session$onSessionEnded(function() {
    stopApp()
  })
  
  # Initialise app state, reactive object for communication between modules
  # Note: an undefined reactive value defaults to NULL, so we can start with empty list
  app_state <- reactiveValues()
  
  # server logic: modules
  mod_global_input_server("global_input", app_state)
  mod_volma_plot_server("volma_plot", app_state)
  mod_feature_plot_server("feature_plot", app_state)
  mod_feature_info_server("feature_info", app_state)
  mod_enrichment_server("enrichment", app_state)
  mod_communication_server("communication", app_state)
}
