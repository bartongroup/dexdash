# Module FEATURE PLOT
#
# Creates an intensity vs group plot for one feature ID or a sample vs ID vs
# intensity heatmap for multiple feature IDs,
#
# Input:
#    state$sel_feature_plot - selection of feature IDs to plot
#
# Uses:
#    data_set$data - a tibble with values per feature
#    data_set$features - a tibble with feature ID and feature name
#    data_set$metadata - a tibble with data grouping
#

# ----- UI definitions -----

mod_feature_plot_ui <- function(id) {
  ns <- shiny::NS(id)


  colour_variable <- shiny::selectInput(
    inputId = ns("colour_var"),
    label = "Colour variable",
    choices = NULL
  )

  group_mean <- shiny::checkboxInput(
    inputId = ns("group_mean"),
    label = "Heatmap averaged across replicates",
    value = FALSE
  )

  norm_fc <- shiny::checkboxInput(
    inputId = ns("norm_fc"),
    label = "Heatmap normalised per row",
    value = TRUE
  )

  intensity_scale <- shiny::radioButtons(
    inputId = ns("intensity_scale"),
    label = "Intesity scale",
    choices = c("Linear" = "lin", "Logarithmic" = "log"),
    inline = TRUE
  )

  info <- bslib::popover(
    bsicons::bs_icon("info-circle"),
    htmltools::includeMarkdown(system.file("helpers/feature_plot.md", package = "dexdash")),
    options = list(customClass = "info-pop")
  )

  gear <- bslib::popover(
    bsicons::bs_icon("gear"),
    colour_variable,
    group_mean,
    norm_fc,
    shiny::conditionalPanel(
      condition = 'input.norm_fc == 0',
      ns = ns,
      intensity_scale
    )
  )

  bslib::card(
    bslib::card_header(
      "Feature plot",
      shiny::span(info, gear),
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
    meta_variables <- setdiff(names(data_set$metadata), "sample")
    shiny::observe({
      shiny::updateSelectInput(
        session = session,
        inputId = "colour_var",
        choices = meta_variables,
        selected = "group"
      )
    })

    output$feature_plot <- shiny::renderPlot({
      ids <- state$sel_feature_plot
      shiny::req(ids)
      data_set$data |>
        dplyr::filter(id %in% ids) |>
        dplyr::left_join(data_set$features, by = "id") |>
        dplyr::left_join(data_set$metadata, by = "sample") |>
        plot_features(what = "value", colour_var = input$colour_var, scale = input$intensity_scale,
                      max_n_lab = 50, norm_fc = input$norm_fc, group_mean = input$group_mean)
    })
  }

  shiny::moduleServer(id, server)
}


# ----- Functions used in the module -----


#' Plot one feature
#'
#' @param d Tibble with feature intensities. Columns needed: group, val, replicate
#' @param ylab Label on y axis
#' @param colour_var (Optional) variable used for point colours
#' @param scale Scale of y axis (lin or log)
#' @param text_size Text size
#' @param point_size Point size
#' @param cex Point spread scaling for beeswarm
#'
#' @return ggplot object
#' @noRd
plot_one_feature <- function(d, ylab, colour_var = NULL,scale = c("lin", "log"),
                             text_size, point_size, cex) {
  val <- group <- shape <- fill <- x <- NULL
  okabe_ito_palette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00",
    "#CC79A7", "grey80", "grey30", "black")

  d <- d |>
    dplyr::mutate(shape = dplyr::if_else(val == 0, 24, 21))

  if(is.null(colour_var))
    colour_var <- "group"

  d <- d |> dplyr::mutate(fill = get(colour_var))

  ncond <- length(unique(d$group))
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
    ggbeeswarm::geom_beeswarm(data = d, ggplot2::aes(x = group, y = val, fill = fill, shape = shape),
                  colour = "grey40", size = point_size, cex = cex) +
    ggplot2::geom_vline(data = vlines, ggplot2::aes(xintercept = x), colour = "grey80", alpha = 0.5) +
    ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(shape = 21))) +
    ggplot2::labs(x = NULL, y = ylab, title = nm, fill = colour_var)

  # If too many colours, use viridis
  if(length(unique(d$fill)) <= length(okabe_ito_palette)){
    g <- g +ggplot2::scale_fill_manual(values = okabe_ito_palette)
  } else {
    g <- g + ggplot2::scale_fill_viridis_d(option = "cividis")
  }

  return(g)
}

#' Plot a heatmap of multiple features
#'
#' @param d Feature data
#' @param lab Label on y axis
#' @param text_size Text size
#' @param max_n_lab Limit of features above which feature names are not displayed
#' @param norm_fc Logical, normalise in each feature to its mean and plot logFC
#' @param group_mean logical, to average data across groups (conditions)
#' @param max_name_len Numeric, maximum length of the name; longer names will be shortened in the plot.
#'
#' @return ggplot object
#' @noRd
plot_feature_heatmap <- function(d, lab, text_size, max_n_lab, norm_fc, group_mean, max_name_len = 18) {
  id <- val <- M <- name <- group <- NULL

  if (norm_fc) {
    d <- d |>
      #dplyr::mutate(val = get(CONFIG$default_data_column)) |>
      dplyr::group_by(id) |>
      dplyr::mutate(M = mean(val, na.rm = TRUE)) |>
      dplyr::mutate(val = log2(val / M)) |>
      dplyr::ungroup()
    lab <- expression(log[2]~FC)
  }

  if(group_mean) {
    d <- d |>
      dplyr::group_by(id, name, group) |>
      dplyr::summarise(val = mean(val, na.rm = TRUE)) |>
      dplyr::mutate(sample = group)
  }

  d <- d |>
    dplyr::mutate(name = dplyr::if_else(nchar(name) < max_name_len,
                            name,
                            stringr::str_c(stringr::str_sub(name, end = max_name_len), "...")
                          )
           )

  g <- d |>
    ggplot2::ggplot(ggplot2::aes(x = sample, y = name, fill = val)) +
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

  if(norm_fc) {
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
#' @param colour_var (Optional) variable used for point colours
#' @param text_size Text size
#' @param point_size Point size
#' @param cex Point spread scaling for beeswarm
#' @param max_n_lab Limit of features above which feature names are not displayed
#' @param norm_fc Logical, normalise in each feature to its mean and plot logFC
#' @param group_mean logical, to average data across groups (conditions)
#'
#' @return A ggplot object
#' @noRd
plot_features <- function(dat, what = "value", colour_var = NULL, scale = "lin", text_size = 14,
                          point_size = 3, cex = 2, max_n_lab = 30, norm_fc = FALSE,
                          group_mean = FALSE) {
  val <- NULL

  if(nrow(dat) == 0) return(NULL)

  dat$val <- dat[[what]]
  dat <- dat |>
    tidyr::drop_na()

  if(nrow(dat) == 0) return(NULL)

  lab <- what

  if(scale == "log"){
    dat$val <- log10(dat$val)
    lab <-  stringr::str_glue("Log {what}")
  }

  n_feat <- length(unique(dat$id))
  if(n_feat == 1) {
    plot_one_feature(dat, lab, colour_var, scale, text_size, point_size, cex)
  } else {
    plot_feature_heatmap(dat, lab, text_size, max_n_lab, norm_fc, group_mean)
  }
}
