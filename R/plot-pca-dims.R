#' Plot A PCA Dimension
#'
#' Plots either the projection or the loadings (rotation) of
#'   `"prcomp"` object. This is a base function that is
#'   wrapped into driver parent functions in the plotting of
#'   principal components, most notably [plot_rotation()]
#'   and [plot_projection()].
#'
#' @family PCA plots
#' @inheritParams pca
#' @param data.prcomp A prcomp class object. Typically the object returned by
#'   [prcomp2()].
#' @param value Character. The type of plot to be generated. For projection
#'   enter "x", for rotation enter "rotation".
#' @param classes Optional. A vector indicating the classes of
#'   observations (either features or samples) used for coloring the
#'   points, depending on whether a rotation or a projection is to be
#'   plotted. Must be the same length as the plot type, i.e. number
#'   of samples for projection and features for rotation.
#' @param scores Optional. KS scores to pass through for the coloring
#'   of the points during plotting. If a training data set is passed and
#'   `scores = NULL`, then KS-distances (scores) will be calculated under the
#'   hood and used to determine point color. Can be either a single value
#'   (e.g. "red") or a vector of color values the same length as the number of
#'   observations. This overrides the point color assigned by `classes` above.
#' @param main Character. A string containing the title for the plot.
#' @param col Character. The color of the points. Can be either a single value
#'   (e.g. "red") or a vector of color values the same length as the number of
#'   observations. This parameter overrides the point colors determined by
#'   both the `classes` and `scores` parameters above.
#' @param xlab Character. A string indicating the label of the x axis.
#' @param ylab Character. A string indicating the label of the y axis.
#' @param legend.pos Character. Position of the legend, if plotted.
#'   Options include "left", "right", "bottom", or "top", or "none"
#'   if no legend is desired. Ignored if `value = "r"` (rotation) and
#'   classes are not specified.
#' @param xlim Numeric. Optional limits for the x-axis in the format `c(0, 0)`.
#' @param ylim Numeric. Optional limits for the y-axis in the format `c(0, 0)`.
#' @param pt.cex Numeric. Character expansion for the points.
#' @param pt.pch Numeric. Shape of the points. Accepted values are 0-25.
#' @param add.ellipse Logical. Should an ellipse be added to the
#'   rotation? Ignored if `value = "x"` (projection).
#' @return A points plot of either rotation or projection space.
#' @author Michael Mehan, Amanda Hiser
#' @examples
#' # Prepare data
#' feat <- grep("^seq", names(sim_adat), value = TRUE)
#' for (i in feat) sim_adat[[i]] <- log10(sim_adat[[i]])
#' pca <- center_scale(sim_adat, center = TRUE, scale = FALSE) |>
#'   strip_meta() |>
#'   prcomp2()
#' scores <- withr::with_seed(101, rnorm(length(feat)))
#'
#' # Rotation space, with scores to define point colors
#' plot_pca_dims(pca, value = "r", dims = 1:2L, classes = NULL,
#'               scores = rnorm(length(feat)))
#'
#' # Projection space, with classes to define point colors
#' plot_pca_dims(pca, value = "x", dims = 1:2L,
#'               classes = sim_adat$class_response)
#' @importFrom graphics plot points identify abline
#' @importFrom utils head
#' @importFrom ggplot2 ggplot aes theme rel labs element_text discrete_scale
#' @importFrom ggplot2 geom_hline geom_vline geom_point stat_ellipse
#' @importFrom ggplot2 scale_color_manual scale_size_identity ylim xlim
#' @export
plot_pca_dims <- function(data.prcomp, value = c(NA, "rotation", "x"),
                          dims, classes,
                          main = sprintf("PCA Plot (%s)",
                                         ifelse(value == "x",
                                                "projection", value)),
                          scores = NULL, col = NULL,
                          xlab = NULL, ylab = NULL,
                          legend.pos = "right",
                          xlim = NULL, ylim = NULL,
                          pt.cex = 2.5, pt.pch = 19,
                          add.ellipse = FALSE, ...) {

  value <- match.arg(value)

  if ( is.na(value) ) {
    stop(
      "Invalid `value = ` argument: NA. ",
      "Must be either `x` or `rotation`", call. = FALSE
    )
  }
  if ( value == "projection" && add.ellipse ) {
    signal_info("Value is 'projection', no ellipse will be plotted!")
  }

  # Silence legend for non-projection plots
  if ( value != "x" && is.null(classes) ) {
    legend.pos <- "right"
  }

  type_len <- nrow(data.prcomp[[value]])

  if ( is.null(col) ) {
    if ( is.null(classes) ) {
      classes <- rep_len("none", type_len)
      legend.pos <- "none"
    } else {
      if ( length(classes) != type_len ) {
        stop("The length of `classes = ` must be the same length as ",
             "the plot type,\n i.e. number of samples for projection ",
             "and number of features for rotation", call. = FALSE)
      }
      if ( !is.factor(classes) ) {
        classes <- factor(classes)
      }
    }
    if ( !is.null(scores) ) {
      if ( length(scores) != type_len ) {
        stop("The length of `scores = ` must be the same length as ",
             "the plot type,\n i.e. number of samples for projection ",
             "and number of features for rotation", call. = FALSE)
      }
      col <- .map_color(scores, topo_colors(nrow(data.prcomp$rotation)))
      classes <- factor(seq_len(type_len)) # Each class must be unique
      legend.pos <- "none" # Prevents large legend due to factor classes
    }
  }

  if ( is.null(classes) ) {
    classes <- col
    legend.pos <- "none"
  }

  # Expand col vector if only 1 value provided
  if ( length(col) == 1L ) {
    col <- rep_len(col, type_len)
  } else if ( !is.null(col) ) {
    if ( length(col) != type_len ) {
      stop("The length of `col = ` must be either 1 (indicating one ",
           "color for all\n points), or the same length as ",
           "the plot type, i.e. number of samples for\n projection ",
           "and number of features for rotation", call. = FALSE)
    }
  }

  plot.data <- data.frame(x = data.prcomp[[value]][, dims[1L]],
                          y = data.prcomp[[value]][, dims[2L]],
                          class = classes,
                          shape = pt.pch,
                          size = pt.cex)

  # Create x and y-axis labels
  if ( is.null(xlab) ) {
    xlab <- sprintf(
      "Component %i (%s%%)", dims[1L],
      format(data.prcomp$sdev[dims[1L]]^2 / sum(data.prcomp$sdev^2) * 100,
             digits = 2)
    )
  }

  if ( is.null(ylab) ) {
    ylab <- sprintf(
      "Component %i (%s%%)", dims[2L],
      format(data.prcomp$sdev[dims[2L]]^2 / sum(data.prcomp$sdev^2) * 100,
             digits = 2)
    )
  }

  # Generate the base plot
  p <- ggplot(plot.data, aes(x = x, y = y, color = class)) +
    geom_point(size = pt.cex, shape = pt.pch, ...) +
    labs(title = main, x = xlab, y = ylab) +
    theme(legend.position = legend.pos,
          legend.title = element_blank(),
          plot.title = element_text(face = "bold", hjust = 0.5,
                                    size = rel(1.2))) +
    scale_size_identity()

  # Adjust color scale based on 'scores' or user-supplied 'col' values
  if ( !is.null(col) || !is.null(scores) ) {
    p <- p + scale_color_manual(values = unname(col))
  } else if ( is.null(col) ) {
    p <- p + discrete_scale("color", palette = function(n)
                            rep_len(unlist(col_palette, use.names = FALSE),
                                    length.out = n))
  }

  if ( value == "rotation" ) {
    if ( add.ellipse ) {
      if ( is.null(classes) ) {
        p <- p + stat_ellipse(color = col) # Add single ellipse to base plot
      } else {
        p <- p + stat_ellipse(aes(color = class)) # Add individual ellipses for each class
      }
    }
  }

  # Add [0,0] grid lines
  p <- p +
    geom_hline(yintercept = 0, color = 8, linetype = 2) +
    geom_vline(xintercept = 0, color = 8, linetype = 2)

  # Add x and y axis limits, if specified. Chosen over
  # ggplot2::coord_cartesian() for the "zoom" effect on the axes
  if ( !is.null(ylim) ) {
    p <- p + ylim(ylim)
  }

  if ( !is.null(xlim) ) {
    p <- p + xlim(xlim)
  }

  p
}


#' Color Map
#'
#' Creates a mapping between a vector of numeric
#'   values to a color gradient. Similar to [class_color()]
#'   but for quantitative data.
#'
#' @param values Numeric. A vector of values to rescale.
#' @param color.scheme a vector of colors to map into to.
#'   Typically a color gradient.
#' @return A vector of colors of the same length as `values`.
#' @noRd
.map_color <- function(values, color.scheme = topo_colors(100)) {
  lmap <- function(x, from, to) {
    (x - min(x)) / max(x - min(x)) * (to - from) + from
  }
  L   <- length(color.scheme)
  idx <- round(lmap(values, 1, L))
  color.scheme[idx]
}
