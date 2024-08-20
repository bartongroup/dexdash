# Module FEATURE PLOT
#
# Creates an intensity vs group plot for one feature ID or a sample vs ID vs
# intensity heatmap for multiple feature IDs,
#
# Input:
#    state$sel_feature_plot - selection of feature IDs to plot
#
# Uses:
#    data_set$dex - user data
#    data_set$features - a tibble with feature ID and feature name
#    data_set$metadata - a tibble with data grouping
#

# ----- UI definitions -----

mod_feature_plot_ui <- function(id) {
  ns <- shiny::NS(id)

  x_variable <- shiny::selectInput(
    inputId = ns("x_var"),
    label = "X-axis variable",
    choices = NULL
  )

  colour_variable <- shiny::selectInput(
    inputId = ns("colour_var"),
    label = "Colour variable",
    choices = NULL
  )

  norm_mean <- shiny::checkboxInput(
    inputId = ns("norm_mean"),
    label = "Heatmap normalised per row",
    value = TRUE
  )

  intensity_scale <- shiny::radioButtons(
    inputId = ns("intensity_scale"),
    label = "Intesity scale",
    choices = c("Linear" = "lin", "Logarithmic" = "log"),
    inline = TRUE
  )

  info <-info_icon("feature_plot")

  download <- shiny::uiOutput(ns("download_plot"))

  gear <- gear_icon(
    x_variable,
    colour_variable,
    norm_mean,
    intensity_scale
  )

  bslib::card(
    bslib::card_header(
      "Feature plot",
      shiny::span(info, download, gear),
      class = "d-flex justify-content-between"
    ),
    shiny::plotOutput(
      outputId = ns("feature_plot"),
      width = "100%",
      height = "300px"
    )
  )

}

# ----- Server logic -----

mod_feature_plot_server <- function(id, data_set, state) {

  server <- function(input, output, session) {

    # Update dummy colour variable selections from data
    shiny::observe({
      set_name <- state$set_name
      shiny::req(set_name)
      shiny::updateSelectInput(
        session = session,
        inputId = "colour_var",
        choices = setdiff(names(data_set$dex[[set_name]]$metadata), c("sample")),
        selected = state$colour_variable
      )
    })

    # Update dummy x-variable selections from data
    shiny::observe({
      set_name <- state$set_name
      shiny::req(set_name)
      shiny::updateSelectInput(
        session = session,
        inputId = "x_var",
        choices = names(data_set$dex[[set_name]]$metadata),
        selected = state$x_variable
      )
    })

    # Function to make plot
    make_plot <- shiny::reactive({
      ids <- state$sel_feature_plot
      set_name <- state$set_name
      shiny::req(ids, set_name)
      data_set$dex[[set_name]]$dat |>
        dplyr::filter(id %in% ids) |>
        dplyr::left_join(data_set$features, by = "id") |>
        dplyr::left_join(data_set$dex[[set_name]]$metadata, by = "sample") |>
        plot_features(what = "value", x_var = input$x_var, colour_var = input$colour_var,
                      scale = input$intensity_scale, max_n_lab = 50, norm_mean = input$norm_mean)
    })

    # Output plot
    output$feature_plot <- shiny::renderPlot({
      make_plot()
    })

    # Display download icon only when there are data to plot
    output$download_plot <- shiny::renderUI({
      shiny::req(state$sel_feature_plot)
      download_link(id)
    })

    # Download handler
    output$handle_download <- shiny::downloadHandler(
      filename = "feature_plot.pdf",
      content = function(file) {
        ggplot2::ggsave(file, plot = make_plot(), device = "pdf", width = 6, height = 6)
      }
    )
  }

  shiny::moduleServer(id, server)
}


# ----- Functions used in the module -----


#' Plot one feature
#'
#' @param d Tibble with feature intensities. Columns needed: x, val, replicate
#' @param ylab Label on y axis
#' @param colour_var Variable used for point colours
#' @param text_size Text size
#' @param point_size Point size
#' @param cex Point spread scaling for beeswarm
#'
#' @return ggplot object
#' @noRd
plot_one_feature <- function(d, ylab, colour_var, text_size, point_size, cex) {
  val <- shape <- fill <- x <- NULL
  okabe_ito_palette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00",
    "#CC79A7", "grey80", "grey30", "black")

  d <- d |>
    dplyr::mutate(
      shape = dplyr::if_else(val == 0, 24, 21),
      fill = get(colour_var)
    )
  
  ncond <- length(unique(d$x))
  vlines <- tibble::tibble(x = seq(1.5, ncond - 0.5, 1))

  nm <- dplyr::first(d$name)

  g <- ggplot2::ggplot() +
    ggplot2::theme_bw() +
    ggplot2::theme(
      text = ggplot2::element_text(size = text_size),
      panel.grid = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    ) +
    ggplot2::scale_shape_identity() +  # necessary for shape mapping
    ggbeeswarm::geom_beeswarm(data = d, ggplot2::aes(x = x, y = val, fill = fill, shape = shape),
                  colour = "grey40", size = point_size, cex = cex) +
    ggplot2::geom_vline(data = vlines, ggplot2::aes(xintercept = x), colour = "grey80", alpha = 0.5) +
    ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(shape = 21))) +
    ggplot2::labs(x = NULL, y = ylab, title = nm, fill = colour_var)

  # If too many colours, use viridis
  if(length(unique(d$fill)) <= length(okabe_ito_palette)){
    g <- g + ggplot2::scale_fill_manual(values = okabe_ito_palette)
  } else {
    g <- g + ggplot2::scale_fill_viridis_d(option = "cividis")
  }

  return(g)
}

#' Plot a heatmap of multiple features
#'
#' @param d Feature data
#' @param ylab Label on z axis
#' @param text_size Text size
#' @param max_n_lab Limit of features above which feature names are not displayed
#' @param norm_mean Logical, normalise in each feature to its mean and plot data 
#'   centered around the mean.
#' @param max_name_len Numeric, maximum length of the name; longer names will be 
#'   shortened in the plot.
#'
#' @return ggplot object
#' @noRd
plot_feature_heatmap <- function(d, lab, text_size, max_n_lab, norm_mean, max_name_len = 18) {
  id <- val <- M <- name <- x <- unique_name <- NULL

  i2n <- d |>
    dplyr::select(id, name) |>
    dplyr::distinct() |>
    dplyr::mutate(unique_name = make.unique(name))

  if (norm_mean) {
    d <- d |>
      dplyr::group_by(id) |>
      dplyr::mutate(M = mean(val, na.rm = TRUE)) |>
      dplyr::mutate(val = val - M) |>
      dplyr::ungroup()
    lab <- paste(lab, "- mean")
  }

  d <- d |>
    dplyr::group_by(id, x) |>
    dplyr::summarise(val = mean(val, na.rm = TRUE), .groups = "drop") |>
    dplyr::left_join(i2n, by = dplyr::join_by(id)) |>
    dplyr::mutate(name = dplyr::if_else(nchar(name) < max_name_len,
                            name,
                            stringr::str_c(stringr::str_sub(name, end = max_name_len), "...")
                          )
           )

  g <- d |>
    ggplot2::ggplot(ggplot2::aes(x = x, y = unique_name, fill = val)) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      text = ggplot2::element_text(size = text_size),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    ) +
    ggplot2::geom_tile() +
    ggplot2::scale_x_discrete(expand = c(0, 0)) +
    ggplot2::scale_y_discrete(expand = c(0, 0)) +
    ggplot2::labs(x = NULL, y = NULL, fill = lab)

  if(norm_mean) {
    g <- g + ggplot2::scale_fill_distiller(type = "div", palette = "RdBu")
  } else {
    g <-  g + ggplot2::scale_fill_viridis_c(option = "cividis")
  }

  if(length(unique(d$id)) > max_n_lab)
    g <- g + ggplot2::theme(axis.text.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank())

  return(g)
}

#' Make a feature plot: intensity vs sample or group
#'
#' @param dat Tibble with feature intensities
#' @param scale Intensity scale, "lin" or  "log"
#' @param what Which column to plot
#' @param x_var Variable to put on x-axis
#' @param colour_var Variable used for point colours
#' @param text_size Text size
#' @param point_size Point size
#' @param cex Point spread scaling for beeswarm
#' @param max_n_lab Limit of features above which feature names are not displayed
#' @param norm_mean Logical, normalise in each feature to its mean
#'
#' @return A ggplot object
#' @noRd
plot_features <- function(dat, what, x_var, colour_var, scale = "lin", text_size = 14,
                          point_size = 3, cex = 2, max_n_lab = 30, norm_mean = FALSE) {
  val <- x <- NULL

  if(nrow(dat) == 0) return(NULL)

  dat$val <- dat[[what]]
  dat$x <- dat[[x_var]]
  dat <- dat |>
    dplyr::filter(!is.na(val) & !is.na(x))

  if(nrow(dat) == 0) return(NULL)

  lab <- what

  if(scale == "log"){
    dat <- dat |> 
      dplyr::filter(val > 0) |> 
      dplyr::mutate(val = log10(val))
    lab <-  stringr::str_glue("log {what}")
  }

  n_feat <- length(unique(dat$id))
  if(n_feat == 1) {
    plot_one_feature(dat, lab, colour_var, text_size, point_size, cex)
  } else {
    plot_feature_heatmap(dat, lab, text_size, max_n_lab, norm_mean)
  }
}
