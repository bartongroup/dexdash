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
#    DATA$de - a tibble with differential expression results
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

  bslib::card(
    bslib::card_header(
      "Volcano/MA plot",
      gear,
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
      xy_data <- get_volma_data(data_set$de, state$contrast, input$plot_type)
      ids_brush <- NULL
      ids_hover <- NULL
      if(!is.null(input$plot_brush)){
        brushed <- na.omit(brushedPoints(xy_data, input$plot_brush))
        ids_brush <- brushed$id
      } else if(!is.null(input$plot_hover)) {
        near <- shiny::nearPoints(xy_data, input$plot_hover, threshold = 20, maxpoints = 1)
        ids_hover <- near$id
      }
      state$sel_brush <- ids_brush
      state$sel_hover <- ids_hover
    })

    output$main_plot <- shiny::renderPlot({
      xy_data <- get_volma_data(data_set$de, state$contrast, input$plot_type)
      sh_main_plot(xy_data, input$plot_type, state$sel_volma_highlight,
                   fdr_limit = input$fdr_limit, logfc_limit = input$logfc_limit)
    })
  }

  moduleServer(id, server)
}



# Functions used by the module

get_volma_data <- function(de, ctr, plot_type) {
  de <- de |>
    dplyr::filter(contrast == ctr)
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


# Functions used in the module

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
sh_plot_xy <- function(d, point_size = 0.2, sig_size = 0.4, sel_size = 3, text_size = 13,
                       point_colour = "grey70", sig_colour = "black", sel_fill = "blue", sel_colour = "yellow") {
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

sh_plot_volcano <- function(d, ...) {
  d |>
    sh_plot_xy(...) +
    ggplot2::geom_vline(xintercept = 0, colour = "grey70") +
    ggplot2::labs(x = expression(log[2]~FC), y = expression(-log[10]~P)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.03)))
}

sh_plot_ma <- function(d, ...) {
  d |>
    sh_plot_xy(...) +
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
sh_main_plot <- function(d, plot_type, sel_ids, fdr_limit, logfc_limit, ...) {

  d <- d |>
    dplyr::mutate(sig = fdr < fdr_limit & abs(log_fc) > logfc_limit)

  if(!is.null(sel_ids)) {
    d$sel <- d$id %in% sel_ids
  } else {
    d$sel <- FALSE
  }

  if(plot_type == "Volcano") {
    g <- sh_plot_volcano(d)
  } else if(plot_type == "MA") {
    g <- sh_plot_ma(d)
  }
  return(g)
}

