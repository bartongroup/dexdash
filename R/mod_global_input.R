# Module for global input for all other modules

# ----- UI definitions -----

mod_global_input_ui <- function(id) {
  ns <- shiny::NS(id)

  data_set_info <- info_icon("data_set")
  contrast_info <- info_icon("contrast")
  search_info <- info_icon("search")

  shiny::tagList(
    shiny::selectInput(
      inputId = ns("set_name"),
      label = shiny::span("Data set", data_set_info),
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

  get_contrasts <- function(set_name) {
    data_set$dex[[set_name]]$de |>
      dplyr::pull(contrast) |>
      as.character() |>
      unique()
  }

  server <- function(input, output, session) {

    # Update dummy set selection
    shiny::observe({
      shiny::updateSelectInput(
        session = session,
        inputId = "set_name",
        choices = data_set$names
      )
    })

    # update dummy contrast selection
    shiny::observe({
      shiny::updateSelectInput(
        session = session,
        inputId = "contrast",
        choices = get_contrasts(data_set$names[1])
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

    # Observe data set, copy to state, update contrasts
    shiny::observeEvent(input$set_name, {
      set_name <- input$set_name
      shiny::req(set_name)
      state$set_name <- set_name
      contrasts <- get_contrasts(set_name)
      shiny::updateSelectInput(
        session = session,
        inputId = "contrast",
        choices = contrasts
      )
    })

    # Observe contrast, selection, copy to state
    shiny::observeEvent(input$contrast, state$contrast <- input$contrast)

    # Observe search input, copy to state, convert "" to NULL
    shiny::observeEvent(input$search, {
      state$search <- input$search
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
