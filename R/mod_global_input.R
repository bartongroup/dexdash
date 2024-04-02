# Module for global input for all other modules

require(shiny)
require(dqshiny)

# ----- UI definitions -----

mod_global_input_ui <- function(id) {
  ns <- NS(id)

  tagList(
    selectInput(
      inputId = ns("contrast"),
      label = "Contrast",
      choices = c("")
    ),
    autocomplete_input(
      id = ns("search"),
      label = "Search",
      options = NULL
    ),
    actionButton(
      inputId = ns("clear"),
      label = "Clear"
    )
  )
}

# ----- Server logic -----

mod_global_input_server <- function(id, data_set, state) {

  server <- function(input, output, session) {

    # Update dummy contrast selection
    contrasts <- levels(data_set$de$contrast)
    observe({
      updateSelectInput(session, "contrast", choices = contrasts)
    })

    # Update bases and search input when experiment changed
    observeEvent(input$experiment, {
      all_names <- data_set$features$name |> unique()
      update_autocomplete_input(session, "search", options = c("", all_names))
    })

    # Observe base selection, copy to state, update contrast selection
    observeEvent(input$base, {
      state$base <- input$base
      ctrs <- data_set$de |>
        pull(contrast) |>
        unique()
      updateSelectInput(session, "contrast", choices = ctrs)
    })

    # Observe contrast experiment, selection, copy to state
    observeEvent(input$contrast, state$contrast <- input$contrast)

    # Observe search input, copy to state, convert "" to NULL
    observeEvent(input$search, {
      state$search <- input$search;
      if(state$search == "")
        state$search <- NULL
    },
    ignoreNULL = FALSE
    )

    # Clear input
    observeEvent(input$clear, update_autocomplete_input(session, "search", value = "", placeholder = ""))

  }

  moduleServer(id, server)
}
