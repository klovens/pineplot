#' Plots a heatmap of a symmetric matrix.
#'
#' @param sym_matrix A matrix of symmetric values indicating a relationship between variables of the rows/columns.
#' @param fill_name Name of values in the symmetric matrix.
#' @param xlabel Label to include on x-axis.
#' @param ylabel Label to include on y-axis.
#' @param limits Limits of the fill values.
#' @param pyramid Boolean indicates lower triangle of the heamap be removed (TRUE) or not (FALSE).
#' @param midpoint The midpoint of the legend scale (if legend included).
#' @param low Lower colour for fill gradient.
#' @param mid Midpoint colour for fill gradient.
#' @param high Higher colour for fill gradient.
#' @param breaks Position of the tick marks in the heatmap legend.
#' @return A ggplot heatmap object that can be further manipulated using ggplot components if necessary.
#' @export
draw_heatmap <- function(sym_matrix, fill_name = "Value", xlabel = "", ylabel = "", limits = NULL,
                         pyramid = TRUE, low = "blue", mid = "white", high = "red", midpoint = 0,
                         breaks = waiver()) {
  overlap_melted <- massage_data(sym_matrix, pyramid, fill_name)
  # use ggplot2 to visualize create a ggplot2 object
  plt <- ggplot(
    overlap_melted,
    aes(x = factor(Var1), y = factor(Var2), fill = Value)
  ) +
    geom_tile() +
    coord_equal(expand = FALSE, clip = "off") +
    scale_x_discrete(labels = rownames(sym_matrix), expand = c(0, 0), position = "top") +
    scale_y_discrete(labels = rownames(sym_matrix), expand = c(0, 0)) +
    scale_fill_gradient2(
      low = low, mid = mid, high = high, midpoint = midpoint,
      space = "Lab", na.value = "transparent",
      limits = limits, guide = FALSE,
      breaks = breaks
    ) +
    theme(
      plot.margin = margin(0, 0, 0, 0, "null"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      plot.background = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_text(angle = 90, hjust = 0),
      legend.background = element_blank()
    )

  return(plt)
}

massage_data <- function(sym.matrix, pyramid, fill.name) {
  sym.matrix <- unname(sym.matrix) # strip rownames and colnames so melt doesn't use them
  number.of.experiments <- dim(sym.matrix)[2]
  # to remove the redundancy in the heatmap, set all cells in matrix under the diagonal
  # to NA
  if (pyramid == TRUE) {
    # check if matrix is symmetric
    if (!all(sym.matrix == t(sym.matrix))) {
      stop("The matrix is not symmetric!")
    }

    # if matrix is symmetric, then remove the redundant triangle of the matrix by setting to NA
    sym.matrix[lower.tri(sym.matrix)] <- NA
  }

  # reshape the matrix to get row/column pairs and their value (Note: dataframes will not work as expected
  # of matrices here)
  overlap.melted <- reshape2::melt(sym.matrix, value.name = fill.name)
  return(overlap.melted)
}

#' Make the pine plot for a list of heatmaps and organize in a stacked, pine tree shape.
#'
#' @param h_maps List of heat maps to add to a pineplot.
#' @param num_heatmaps The total number of heat maps to add to pine plot.
#' @param height The height of the file to start plotting the heat maps.
#' @param width The width of the file to start plotting the heat maps.
#' @param leg Boolean indicating if plots have a legend.
#' @param angle angle to rotate each heat map.
#' @param legend_x_offset Depending on label size and length, may want to adjust position of the legend using offset.
#' @param legend_y_offset Depending on label size and length, may want to adjust the y position of the pine plot.
#' @param hm_y_offset Changes the y posiiton of heatmap shifting them higher or lower in the viewport.
#' @param hm_margin Changes the space left between the heat maps if they need to be closer or farther appart for athstetic purpose.
#' @return Pine plot written to view or a file if open. No return object.
#' @export
write_pine_plot <- function(h_maps, num_heatmaps = length(h_maps), height = 30, width = 10, leg = FALSE,
                            angle = c(-45), legend_x_offset = 0.02, legend_y_offset = 0, hm_y_offset = 0,
                            hm_margin = 0) {
  # check if the number of heatmaps is or num_heatmaps empty
  if (num_heatmaps < 1 | length(h_maps) == 0) {
    stop("Heatmaps required.")
  }

  # check if different lengths provided for the number of heatmaps
  if (num_heatmaps != length(h_maps)) {
    print("The number of heatmaps in h_maps (length) does not match num_heatmaps. Ensure this is intentional.")
  }

  # original viewport (width and height of page specified)
  pushViewport(viewport(width = unit(width, "cm"), height = unit(height, "cm")))
  # store one heatmap in list (h_maps) with hm
  hm <- c()

  # for all the heatmaps in h_maps
  for (n in 1:num_heatmaps) {
    # get heatmap at position n in the list h_maps
    hm <- h_maps[[n]]
    # set width as the height of viewport divided by number of heatmaps
    w <- (height / num_heatmaps)
    # set height as the height of viewport divided by number of heatmaps
    h <- (height / num_heatmaps)

    # calculate the diagonal size of each heatmap to determine placement along y-axis
    hm_size <- sqrt((w**2) + (h**2)) / 2 + hm_margin


    if (leg == TRUE) {
      # extract and save the legend
      tmp <- ggplot_gtable(ggplot_build(hm))
      legend <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
      legend <- tmp$grobs[[legend]]
      # remove any legend from the heatmaps if it is present
      hm <- hm + theme(
        legend.position = "none",
        plot.margin = unit(c(1, 1, 1, 1), "cm")
      )
      # place the legend or the trunk of the tree
      vp <- viewport(
        x = unit((width / 2) + legend_x_offset, "cm"),
        y = unit((height - 0.5) - ((hm_size) * (num_heatmaps + 1)) + (hm_size / 2) + legend_y_offset, "cm")
      )
      legend$vp <- vp
      grid.draw(legend)
    }
    # create viewport with equal width and height for heatmap centered
    # and placed below y coordinates of previous heatmap
    # at a 45 degree angle
    vp <- viewport(
      width = unit(w, "cm"),
      height = unit(h, "cm"),
      x = 0.5,
      y = unit((height - 0.5) - ((hm_size) * (n)) + hm_y_offset, "cm"),
      angle = angle
    )

    # push viewport turning heatmaps at a 45 degree angle
    pushViewport(vp)
    # make a ggplot table
    hm <- ggplot_gtable(ggplot_build(hm))
    # turn off clipping to avoid clipping issues when rotating ggplot2 object
    hm$layout$clip[hm$layout$name == "panel"] <- "off"
    hm$layout$clip <- "off"
    # draw the ggplot object
    grid.draw(hm)
    # return to the original viewport (height x width)
    upViewport()
  }
}

#' Make a pine plot for a list of symmetric matrices.
#'
#' @description
#' The [generate_pineplot] function creates a pine plot, a series of modified ggplot heatmaps,
#' from a list of symmetric matrices.
#' Adjustments to each ggplot heatmap can be performed by passing a function to \code{annotate_fn}.
#' The function should accept two arguments, a ggplot heatmap object, and general purpose data.
#' A second callback function can customize the heatmap grob through the \code{customize_fn} parameter.
#' If a legend is desired, the \code{legend} parameter should be set to TRUE.
#' The \code{legend} parameters can be used to customize the appearance.
#' A \code{legend} grob is added to the return value, regardless of the \code{legend} parameter, for later use.
#'
#' @param sym_matrices List of heat maps to add to a pineplot. The names are important if
#' @param annotation_fn Annotation function for ggplot heatmaps.
#' @param customize_fn Customization function for the heatmap viewports.
#' @param scale Scaling coefficient applied to each heatmap. Necessary in some instances to prevent overlap or reduce the whitespace between plots.
#' @param legend.show Logical value indicating whether to include a colourbar legend or not.
#' @param legend.scale Scaling factor for legend height.
#' @param legend.args List of arguments to pass to the ggplot function \code{\link{guide_colourbar}} function.
#' @param ... Additional arguments to be passed to \code{\link{draw_heatmap}}.
#' @return A pine plot as a grob (i.e. gtable).
#' @examples
#' ms <- replicate(3, outer(-3:3, -3:3, "+"), simplify = FALSE)
#' gr <- generate_pineplot(ms)
#' ggplot2::ggsave('example.pdf', gr)
#' @export
generate_pineplot <- function(sym_matrices,
                              annotation_fn,
                              customize_fn,
                              scale = 1.0,
                              legend.show = TRUE,
                              legend.scale = 1.0,
                              legend.args = list(),
                              ...) {
  if (length(sym_matrices) == 0) {
    stop("ERROR: No symmetric matrices provided.")
  }
  if (!all(sapply(sym_matrices, isSymmetric))) {
    stop("ERROR: One or more of the matrices provided are not symmetric.")
  }
  legend.defaults = list(direction = "horizontal",
                         title.position = "top",
                         title.hjust = .5)
  for (name in names(legend.args)) {
    legend.defaults[[name]] <- legend.args[[name]]
  }

  heatmaps <- lapply(sym_matrices,
    draw_heatmap,
    ...
  )

  # call annotation_fun on each heatmap
  # the name of each heatmap is passed as an argument
  if (!missing(annotation_fn)) {
    for (name in names(heatmaps)) {
      heatmaps[[name]] <- annotation_fn(heatmaps[[name]], name)
    }
  }

  # extract a legend
  grob <- ggplotGrob(heatmaps[[1]] + guides(fill = do.call(guide_colorbar, legend.defaults)))
  legend_grob <- grob$grobs[[which(sapply(grob$grobs, function(x) x$name) == "guide-box")]]
  legend_height <- convertHeight(sum(legend_grob$heights), unitTo = "in")

  grobs <- lapply(heatmaps, ggplotGrob)

  if (!missing(customize_fn)) {
    for (name in names(grobs)) {
      customize_fn(grobs[[name]], name)
    }
  }

  data_margin <- convertWidth(sum(grobs[[1]]$widths), unitTo = "in")
  grobs <- lapply(grobs,
    editGrob,
    vp = viewport(
      angle = -45,
      y = 1 / sqrt(2) * data_margin,
      width = unit(scale * 1.2, "npc"),
      height = unit(scale * 1.2, "npc")
    )
  )

  heights <- unit.c(rep(unit(1, "null"), length(heatmaps)), legend.scale * 1.25 * legend_height)
  gtbl <- gtable(
    widths = unit(scale * 1.2 * sqrt(2), "null"),
    heights = heights,
    respect = T
  )

  # set background to theme plot.background
  gtbl <- gtable_add_grob(
    gtbl,
    element_grob(theme_get()$plot.background),
    t = 1, l = 1, b = length(gtbl$heights), r = 1
  )

  # add heatmaps
  gtbl <- gtable_add_grob(
    gtbl,
    grobs,
    t = seq_along(grobs), l = 1
  )

  # generate and add annotations
  if (!missing(customize_fn)) {
    gtbl <- gtable_add_grob(
      gtbl,
      lapply(names(grobs), function(x) customize_fn(grobs[[x]], x)),
      t = seq_along(grobs), l = 1,
      name = seq_along(grobs)
    )
  }

  # add legend
  if (legend.show) {
    gtbl <- gtable_add_grob(gtbl, legend_grob, length(heatmaps) + 1, 1)
  }

  for (i in seq_along(gtbl$grobs)) {
    gtbl$grobs[[i]]$layout$clip <- "off"
  }

  gtbl$legend_grob <- legend_grob
  return(gtbl)
}
