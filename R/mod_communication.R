# Module COMMUNICATION
#
# Facilitates communication between modules by deciding which feature IDs
# selections are passed to which module. For each module a priority list is
# constructed.


mod_communication_server <- function(id, state) {
  
  server <- function(input, output, session) {
    
    # Select feature IDs to be displayed by feature plot and shown in feature
    # table. Order of commands defines priority, the last one being most
    # important.
    observe({
      ids <- NULL
      if(!is.null(state$sel_brush))
        ids <- state$sel_brush
      if(!is.null(state$sel_term))
        ids <- state$sel_term
      if(!is.null(state$sel_hover))
        ids <- state$sel_hover
      if(!is.null(state$sel_tab))
        ids <- state$sel_tab
      if(!is.null(state$search))
        ids <- DATA[[state$experiment]]$name2id[state$search]
      state$sel_feature_plot <- ids
    })
    
    # Selection for feature info. It is not the same as feature plot, because
    # one can select a row from the feature info table and display it as a plot.
    observe({
      ids <- NULL
      if(!is.null(state$sel_brush))
        ids <- state$sel_brush
      if(!is.null(state$sel_term))
        ids <- state$sel_term
      if(!is.null(state$sel_hover))
        ids <- state$sel_hover
      if(!is.null(state$search))
        ids <- DATA[[state$experiment]]$name2id[state$search]
      state$sel_feature_info <- ids
    })
    
    # For functional enrichment we only take brushed IDs from the Volcano/MA
    # plot
    observe({
      state$sel_functional_enrichment <- state$sel_brush
    })
    
    # Selection of feature IDs to highlight in the Volcano/MA plot
    observe({
      ids <- NULL
      if(!is.null(state$sel_term))
        ids <- state$sel_term
      if(!is.null(state$sel_tab))
        ids <- state$sel_tab
      if(!is.null(state$search))
        ids <- DATA[[state$experiment]]$name2id[state$search]
      state$sel_volma_highlight <- ids
    })
    
  }
  
  moduleServer(id, server)
}
