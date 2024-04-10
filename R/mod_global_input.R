# Module for global input for all other modules

# ----- UI definitions -----

mod_global_input_ui <- function(id) {
  ns <- shiny::NS(id)

  contrast_info <- bslib::popover(
    bsicons::bs_icon("info-circle"),
    htmltools::includeMarkdown("man/helpers/contrast.md"),
    options = list(customClass = "info-pop")
  )

  search_info <- bslib::popover(
    bsicons::bs_icon("info-circle"),
    htmltools::includeMarkdown("man/helpers/search.md"),
    options = list(customClass = "info-pop")
  )

  shiny::tagList(
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

  server <- function(input, output, session) {

    # Update dummy contrast selection
    contrasts <- levels(data_set$de$contrast)
    shiny::observe({
      shiny::updateSelectInput(
        session = session,
        inputId = "contrast",
        choices = contrasts
      )
    })

    # Update dummy feature selection
    features <- c("", unique(data_set$features$name))
    shiny::observe({
      shiny::updateSelectizeInput(
        session = session,
        inputId = "search",
        choices = features,
        server = TRUE
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
      shiny::updateSelectizeInput(session, "search", selected = "")
    })
  }

  shiny::moduleServer(id, server)
}
