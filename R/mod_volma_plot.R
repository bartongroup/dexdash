# Module VOLMA PLOT
#
# Creates a volcano or MA plot. Takes hover or brush inputs from the plot.
#
# Input:
#    state$sel_volma_highlight - selection of feature IDs to highlight in the plot
#
# Output:
#    state$sel_brush - selection of brushed feature IDs
#    state$sel_hover - a hovered feature ID
#
# Uses:
#    data_set$de - a tibble with differential expression results
#

# ----- UI definitions -----

mod_volma_plot_ui <- function(id) {
  ns <- shiny::NS(id)

  plot_type <- shinyWidgets::radioGroupButtons(
    inputId = ns("plot_type"),
    label = "Main plot type",
    choices = c("Volcano", "MA")
  )

  fc_limit <- shiny::numericInput(
    inputId = ns("logfc_limit"),
    label = "|log FC| significance limit",
    value = 0,
    min = 0,
    max = 100
  )

  fdr_limit <- shiny::numericInput(
    inputId = ns("fdr_limit"),
    label = "FDR significance limit",
    value = 0.01,
    min = 0,
    max = 1
  )

  gear <- bslib::popover(
    bsicons::bs_icon("gear"),
    plot_type,
    fc_limit,
    fdr_limit
  )

  info <- bslib::popover(
    bsicons::bs_icon("info-circle"),
    htmltools::includeMarkdown(system.file("helpers/volma.md", package = "dexdash")),
    options = list(customClass = "info-pop")
  )

  bslib::card(
    bslib::card_header(
      "Volcano/MA plot ",
      shiny::span(info, gear),
      class = "d-flex justify-content-between"
    ),
    bslib::card_body(
      min_height = 250,
      shiny::plotOutput(
        outputId = ns("main_plot"),
        width = "100%",
        brush = ns("plot_brush"),
        hover = ns("plot_hover")
      )
    )
  )
}

# ----- Server logic -----

# Requires the following parameters passed in state:
#  - state$contrast - contrast selection
#  - state$plot_type - Volcano or MA

mod_volma_plot_server <- function(id, data_set, state) {

  server <- function(input, output, session) {

    # When brush or hover detected...
    to_listen <- shiny::reactive({
      list(input$plot_brush, input$plot_hover)
    })

    # ...update app state with the selection
    shiny::observeEvent(to_listen(), {
      xy_data <- get_volma_data(data_set$de, state$contrast, state$experiment, input$plot_type)
      ids_brush <- NULL
      ids_hover <- NULL
      if(!is.null(input$plot_brush)){
        brushed <- stats::na.omit(shiny::brushedPoints(xy_data, input$plot_brush))
        ids_brush <- brushed$id
      } else if(!is.null(input$plot_hover)) {
        near <- shiny::nearPoints(xy_data, input$plot_hover, threshold = 20, maxpoints = 1)
        ids_hover <- near$id
      }
      state$sel_brush <- ids_brush
      state$sel_hover <- ids_hover
    })

    output$main_plot <- shiny::renderPlot({
      xy_data <- get_volma_data(data_set$de, state$contrast, state$experiment, input$plot_type)
      main_plot(xy_data, input$plot_type, state$sel_volma_highlight,
                   fdr_limit = input$fdr_limit, logfc_limit = input$logfc_limit)
    })
  }

  shiny::moduleServer(id, server)
}

# ----- Functions used by the module -----


#' Retrieve Processed Data for Volcano or MA Plot
#'
#' This function processes differential expression data to generate data
#' suitable for either volcano or MA (log-ratio vs. mean average) plot
#' visualization. It filters the differential expression data based on the
#' specified contrast and then calculates the coordinates (x and y values)
#' appropriate for the selected plot type.
#'
#' @param de A dataframe containing differential expression data, including log
#'   fold change, p-values, and expression levels. The dataframe must contain
#'   columns that match the variables used for filtering and calculations within
#'   the function.
#' @param ctr A character string specifying the contrast to filter the
#'   differential expression data.
#' @param expm A character string specyifying the experiment to filter the DE
#'   data.
#' @param plot_type A character string indicating the type of plot for which
#'   data is being prepared. The accepted values are "Volcano" for volcano plots
#'   (log fold change vs. -log10 p-value) and "MA" for MA plots (average
#'   expression vs. log fold change). Any other input will result in an error.
#'
#' @return Returns a dataframe with x and y columns calculated based on the
#'   selected plot type. For a volcano plot, x represents log fold change and y
#'   represents -log10 transformed p-values. For an MA plot, x corresponds to
#'   expression levels and y to log fold changes.
#' @noRd
get_volma_data <- function(de, ctr, expm, plot_type) {
  contrast <- experiment <- log_fc <- p_value <- expr <- NULL

  de <- de |>
    dplyr::filter(contrast == ctr & experiment == expm)
  if(plot_type == "Volcano") {
    xy_data <- de |>
      dplyr::mutate(x = log_fc, y = -log10(p_value))
  } else if(plot_type == "MA") {
    xy_data <- de |>
      dplyr::mutate(x = expr, y = log_fc)
  } else {
    stop("What the fuck?")
  }
  return(xy_data)
}



#' Lower level xy plot
#'
#' @param d Data containing columns x, y, sig, sel
#' @param point_size Standard point size
#' @param sig_size Significant point size
#' @param sel_size Selected point size
#' @param text_size Text size
#' @param point_colour Standard point colour
#' @param sig_colour Significant point colour
#' @param sel_fill Selected point fill
#' @param sel_colour Selected point outline colour
#'
#' @return A ggplot object
#' @noRd
plot_xy <- function(d, point_size = 0.2, sig_size = 0.4, sel_size = 3, text_size = 13,
                       point_colour = "grey70", sig_colour = "black", sel_fill = "blue", sel_colour = "yellow") {
  x <- y <- NULL

  g <- ggplot2::ggplot(mapping = ggplot2::aes(x, y)) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      text = ggplot2::element_text(size = text_size)
    )
  # No selection, show significant genes
  if(sum(d$sel) == 0) {
    g <- g +
      ggplot2::geom_point(data = d[!d$sig, ], size = point_size, colour = point_colour) +
      ggplot2::geom_point(data = d[d$sig, ], size = sig_size, colour = sig_colour)
  } else {
    # With selection, do not show significant genes, grey background better for clarity
    g <- g +
      ggplot2::geom_point(data = d, size = point_size, colour = point_colour) +
      ggplot2::geom_point(data = d[d$sel, ], size = sel_size, shape = 21, fill = sel_fill, colour = sel_colour)
  }
  g
}

#' Create a Volcano Plot
#'
#' Utilizes the `plot_xy` function to create a volcano plot. A volcano plot is a
#' type of scatter plot that shows statistical significance (P-value) versus
#' magnitude of change (fold change), helping in the identification of
#' statistically significant gene changes.
#'
#' @param d Dataframe with the necessary data for plotting, formatted as per
#'   `plot_xy` requirements.
#' @param ... Additional arguments passed to `plot_xy`.
#'
#' @return A ggplot object representing the volcano plot.
#' @noRd
plot_volcano <- function(d, ...) {
  d |>
    plot_xy(...) +
    ggplot2::geom_vline(xintercept = 0, colour = "grey70") +
    ggplot2::labs(x = expression(log[2]~FC), y = expression(-log[10]~P)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.03)))
}

#' Create an MA Plot
#'
#' Uses the `plot_xy` function framework to generate an MA plot, which plots the log ratio of changes against the average magnitude of the quantities measured. This type of plot is commonly used in genomic studies, particularly for visualizing expression data.
#'
#' @param d Dataframe with data structured for `plot_xy`.
#' @param ... Additional arguments passed to `plot_xy`.
#'
#' @return A ggplot object representing the MA plot.
#' @noRd
plot_ma <- function(d, ...) {
  d |>
    plot_xy(...) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey70") +
    ggplot2::labs(x = expression(log[2]~CPM), y = expression(log[2]~FC))
}

#' Main plot: volcano or MA
#'
#' @param d DE data set
#' @param plot_type String, "Volcano" or "MA
#' @param sel_ids A character vector with a group of features selection
#' @param fdr_limit Limit for FDR to mark significant features
#' @param logfc_limit Limit for absolute log FC to mark significant features
#' @param ... Other params passed down to plotting function
#' @noRd
main_plot <- function(d, plot_type, sel_ids, fdr_limit, logfc_limit, ...) {
  fdr <- log_fc <- NULL

  d <- d |>
    dplyr::mutate(sig = fdr < fdr_limit & abs(log_fc) > logfc_limit)

  if(!is.null(sel_ids)) {
    d$sel <- d$id %in% sel_ids
  } else {
    d$sel <- FALSE
  }

  if(plot_type == "Volcano") {
    g <- plot_volcano(d)
  } else if(plot_type == "MA") {
    g <- plot_ma(d)
  }
  return(g)
}

