# Module for global input for all other modules

# ----- UI definitions -----

mod_global_input_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::selectInput(
      inputId = ns("contrast"),
      label = "Contrast",
      choices = c("")
    ),
    dqshiny::autocomplete_input(
      id = ns("search"),
      label = "Search",
      options = NULL
    ),
    shiny::actionButton(
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
      shiny::updateSelectInput(session, "contrast", choices = contrasts)
    })

    # Update bases and search input when experiment changed
    observeEvent(input$experiment, {
      all_names <- data_set$features$name |> unique()
      dqshiny::update_autocomplete_input(session, "search", options = c("", all_names))
    })

    # Observe base selection, copy to state, update contrast selection
    shiny::observeEvent(input$base, {
      state$base <- input$base
      ctrs <- data_set$de |>
        dplyr::pull(contrast) |>
        dplyr::unique()
      shiny::updateSelectInput(session, "contrast", choices = ctrs)
    })

    # Observe contrast experiment, selection, copy to state
    shiny::observeEvent(input$contrast, state$contrast <- input$contrast)

    # Observe search input, copy to state, convert "" to NULL
    shiny::observeEvent(input$search, {
      state$search <- input$search;
      if(state$search == "")
        state$search <- NULL
    },
    ignoreNULL = FALSE
    )

    # Clear input
    shiny::observeEvent(input$clear, dqshiny::update_autocomplete_input(session, "search", value = "", placeholder = ""))

  }

  moduleServer(id, server)
}
