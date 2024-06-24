# Module for global input for all other modules

# ----- UI definitions -----

mod_global_input_ui <- function(id) {
  ns <- shiny::NS(id)

  experiment_info <- bslib::popover(
    bsicons::bs_icon("info-circle"),
    htmltools::includeMarkdown(system.file("helpers/experiment.md", package = "dexdash")),
    options = list(customClass = "info-pop")
  )

  contrast_info <- bslib::popover(
    bsicons::bs_icon("info-circle"),
    htmltools::includeMarkdown(system.file("helpers/contrast.md", package = "dexdash")),
    options = list(customClass = "info-pop")
  )

  search_info <- bslib::popover(
    bsicons::bs_icon("info-circle"),
    htmltools::includeMarkdown(system.file("helpers/search.md", package = "dexdash")),
    options = list(customClass = "info-pop")
  )

  shiny::tagList(
    shiny::selectInput(
      inputId = ns("experiment"),
      label = shiny::span("Experiment", experiment_info),
      choices = NULL
    ),
    shiny::selectInput(
      inputId = ns("contrast"),
      label = shiny::span("Contrast", contrast_info),
      choices = NULL
    ),
    shiny::selectizeInput(
      inputId = ns("search"),
      label = shiny::span("Search", search_info),
      selected = NULL,
      choices = NULL
    ),
    shiny::actionButton(
      inputId = ns("clear"),
      label = "Clear"
    )
  )
}

# ----- Server logic -----

mod_global_input_server <- function(id, data_set, state) {
  contrast <- NULL

  get_contrasts <- function(expm) {
    data_set$de |>
      dplyr::filter(experiment == expm) |>
      dplyr::pull(contrast) |>
      unique()
  }

  server <- function(input, output, session) {

    # Update dummy experiment selection
    experiments <- unique(data_set$de$experiment)
    shiny::observe({
      shiny::updateSelectInput(
        session = session,
        inputId = "experiment",
        choices = experiments
      )
    })

    # update dummy contrast selection
    shiny::observe({
      shiny::updateSelectInput(
        session = session,
        inputId = "contrast",
        choices = get_contrasts(experiments[1])
      )
    })

    # Update dummy feature selection
    shiny::observe({
      shiny::updateSelectizeInput(
        session = session,
        inputId = "search",
        choices = c("", unique(data_set$features$name)),
        server = TRUE
      )
    })

    # Observe experiment, copy to state, update contrasts
    shiny::observeEvent(input$experiment, {
      state$experiment <- input$experiment
      contrasts <- get_contrasts(input$experiment)
      shiny::updateSelectInput(
        session = session,
        inputId = "contrast",
        choices = contrasts
      )
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
    shiny::observeEvent(input$clear, {
      shiny::updateSelectizeInput(
        session = session,
        inputId = "search",
        choices = c("", unique(data_set$features$name)),
        selected = NULL,
        server = TRUE
      )
    })
  }

  shiny::moduleServer(id, server)
}
