# Module for global input for all other modules

require(shiny)
require(dqshiny)

# ----- UI definitions -----

mod_global_input_ui <- function(id) {
  ns <- NS(id)

  tagList(
    selectInput(
      inputId = ns("experiment"),
      label = "Experiment",
      choices = c(EXPERIMENTS)
    ),
    selectInput(
      inputId = ns("base"),
      label = "DE approach",
      choices = c("")
    ),
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

mod_global_input_server <- function(id, state) {

  server <- function(input, output, session) {

    # Update dummy base selection
    bases <- unique(DATA[[EXPERIMENTS[1]]]$de$base)
    observe({
      updateSelectInput(session, "base", choices = bases)
    })

    # Update dummy contrast selection
    contrasts <- levels(DATA[[EXPERIMENTS[1]]]$de$contrast)
    observe({
      updateSelectInput(session, "contrast", choices = contrasts)
    })

    # Update bases and search input when experiment changed
    observeEvent(input$experiment, {
      state$experiment <- input$experiment
      bases <- unique(DATA[[input$experiment]]$de$base)
      updateSelectInput(session, "base", choices = bases)
      all_names <- DATA[[input$experiment]]$features$name |> unique()
      update_autocomplete_input(session, "search", options = c("", all_names))
    })

    # Observe base selection, copy to state, update contrast selection
    observeEvent(input$base, {
      state$base <- input$base
      ctrs <- DATA[[input$experiment]]$de |>
        filter(base == input$base) |>
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
