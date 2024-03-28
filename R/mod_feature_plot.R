# Module FEATURE PLOT
#
# Creates an intensity vs group plot for one feature ID or a sample vs ID vs
# intensity heatmap for multiple feature IDs,
#
# Input:
#    state$sel_feature_plot - selection of feature IDs to plot
#
# Uses:
#    DATA$data - a tibble with values per feature
#    DATA$features - a tibble with feature ID and feature name
#    DATA$metadata - a tibble with data grouping
#

require(shiny)
require(shinyWidgets)
require(bsicons)
require(dplyr)
require(tibble)
require(tidyr)
require(ggplot2)
require(ggbeeswarm)

okabe_ito_palette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "grey80", "grey30", "black")

# ----- UI definitions -----

mod_feature_plot_ui <- function(id) {
  ns <- NS(id)

  group_mean <- checkboxInput(
    inputId = ns("group_mean"),
    label = "Heatmap averaged across replicates",
    value = TRUE
  )

  norm_fc <- checkboxInput(
    inputId = ns("norm_fc"),
    label = "Heatmap normalised per row",
    value = TRUE
  )

  intensity_scale <- radioButtons(
    inputId = ns("intensity_scale"),
    label = "Intesity scale",
    choices = c("Linear" = "lin", "Logarithmic" = "log"),
    inline = TRUE
  )

  gear <- popover(
    bs_icon("gear"),
    group_mean,
    norm_fc,
    conditionalPanel(
      condition = 'input.norm_fc == 0',
      ns = ns,
      intensity_scale
    )
  )

  card(
    card_header(
      "Feature plot",
      gear,
      class = "d-flex justify-content-between"
    ),

    plotOutput(
      outputId = ns("feature_plot"),
      width = "100%",
      height = "300px"
    )
  )

}

# ----- Server logic -----

mod_feature_plot_server <- function(id, state) {

  server <- function(input, output, session) {

    output$feature_plot <- renderPlot({
      ids <- state$sel_feature_plot
      req(ids)
      d <- DATA[[state$experiment]]$data |>
        dplyr::filter(id %in% ids) |>
        dplyr::left_join(DATA[[state$experiment]]$features, by = "id")
      sh_plot_features(d, meta = DATA[[state$experiment]]$metadata, scale = input$intensity_scale,
                    what = CONFIG$default_data_column, max_n_lab = 50, norm_fc = input$norm_fc,
                    group_mean = input$group_mean)
    })
  }

  moduleServer(id, server)
}



#' Plot one feature
#'
#' @param d Tibble with feature intensities. Columns needed: group, val, replicate
#' @param ylab Label on y axis
#' @param scale Scale of y axis (lin or log)
#' @param text_size Text size
#' @param point_size Point size
#' @param cex Point spread scaling for beeswarm
#'
#' @return ggplot object
sh_plot_one_feature <- function(d, ylab, scale = c("lin", "log"), text_size, point_size, cex) {

  d <- d |>
    dplyr::mutate(shape = if_else(val == 0, 24, 21))

  ncond <- length(unique(d$group))
  vlines <- tibble::tibble(x = seq(0.5, ncond + 0.5, 1))

  nm <- first(d$name)

  g <- ggplot() +
    theme_bw() +
    theme(
      text = element_text(size = text_size),
      panel.grid = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    ) +
    scale_shape_identity() +  # necessary for shape mapping
    geom_beeswarm(data = d, aes(x = group, y = val, fill = replicate, shape = shape),
                  colour = "grey40", size = point_size, cex = cex) +
    geom_vline(data = vlines, aes(xintercept = x), colour = "grey80", alpha = 0.5) +
    #scale_fill_viridis_d(option = "cividis") +
    scale_fill_manual(values = okabe_ito_palette) +
    guides(fill = guide_legend(override.aes = list(shape = 21))) +
    labs(x = NULL, y = ylab, title = nm)

  # if(scale == "lin")
  #   g <- g + scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA))

  return(g)
}

#' Plot a heatmap of multiple features
#'
#' @param d Feature data
#' @param ylab Label on y axis
#' @param text_size Text size
#' @param max_n_lab Limit of features above which feature names are not displayed
#' @param norm_fc Logical, normalise in each feature to its mean and plot logFC
#' @param group_mean logical, to average data across groups (conditions)
#'
#' @return ggplot object
sh_plot_feature_heatmap <- function(d, lab, text_size, max_n_lab, norm_fc, group_mean, max_name_len = 18) {

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
    mutate(name = if_else(nchar(name) < max_name_len,
                            name,
                            str_c(str_sub(name, end = max_name_len), "...")
                          )
           )

  g <- d |>
    ggplot(aes(x = sample, y = name, fill = val)) +
    theme_bw() +
    theme(
      panel.grid = element_blank(),
      text = element_text(size = text_size),
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    geom_tile() +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    labs(x = NULL, y = NULL, fill = lab)

  if(norm_fc) {
    g <- g + scale_fill_distiller(type = "div", palette = "RdBu")
  } else {
    g <-  g + scale_fill_viridis_c(option = "cividis")
  }

  if(length(unique(d$id)) > max_n_lab)
    g <- g + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

  return(g)
}

#' Make a feature plot: intensity vs sample or group
#'
#' @param dat Tibble with feature intensities
#' @param meta Metadata with grouping of samples
#' @param scale Intensity scale, "lin" or  "log"
#' @param what Which column to plot
#' @param text_size Text size
#' @param point_size Point size
#' @param cex Point spread scaling for beeswarm
#' @param max_n_lab Limit of features above which feature names are not displayed
#' @param norm_fc Logical, normalise in each feature to its mean and plot logFC
#' @param group_mean logical, to average data across groups (conditions)
#'
#' @return A ggplot object
sh_plot_features <- function(dat, meta, scale, what = "rpkm", text_size = 14, point_size = 3, cex = 2,
                          max_n_lab = 30, norm_fc = FALSE, group_mean = FALSE) {

  if(nrow(dat) == 0) return(NULL)

  d <- dat |>
    dplyr::mutate(val = get(what)) |>
    dplyr::left_join(meta, by = "sample") |>
    tidyr::drop_na()

  if(nrow(d) == 0) return(NULL)

  lab <- what

  if(scale == "log"){
    d$val <- log10(d$val)
    lab <-  str_glue("Log {what}")
  }

  n_feat <- length(unique(d$id))
  if(n_feat == 1) {
    sh_plot_one_feature(d, lab, scale, text_size, point_size, cex)
  } else {
    sh_plot_feature_heatmap(d, lab, text_size, max_n_lab, norm_fc, group_mean)
  }
}
