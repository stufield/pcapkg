#' Plot PCA Paired Dimensions
#'
#' Plots the pairwise dimensions for each rotation
#' and projection for a `prcomp` object. Please note that generating
#' pairs plots for >3 dimensions can be slow.
#'
#' Please keep in mind that generating pairs plots for >3 dimensions
#' can be slow.
#'
#' @family PCA plots
#' @inheritParams pca
#' @inheritParams plotPeelWrapper
#' @inheritParams supervised_peel
#' @param tr.data Optional data frame of class `tr_data`.
#'   Used to generate `apt.scores`. If `sample.classes`
#'   is _not_ passed, the "Response" column is used to color the projection.
#' @param apt.scores See `scores =` argument in [plot_rotation()].
#' @param sample.classes See `classes =` argument in [plot_projection()].
#' @param sample.scores See `scores =` argument in [plot_projection()].
#' @param apt.col See `col =` argument in [plot_rotation()].
#' @param apt.classes See `classes =` argument in [plot_rotation()].
#' @param auto_ident Logical. See [plot_rotation()].
#' @param ... Arguments passed to either [plot_rotation()] or [plot_projection()].
#' @author Michael Mehan, Amanda Hiser
#' @examples
#' tr <- libml::create_train(sim_adat, group.var = class_response) |>
#'   ungroup() |> log_rfu() |> center_scale(center = TRUE, scale = FALSE)
#' pca <- ungroup(tr) |> feature_matrix() |> prcomp2()
#' apts <- withr::with_seed(123, sample(get_analytes(sim_adat), 10L))
#'
#' # Default colors w/ marked aptamers
#' plot_pca_pairs(pca, aptamers = apts)
#'
#' # Color projection plots by training data
#' plot_pca_pairs(pca, aptamers = apts, tr.data = tr)
#'
#' # Override default colors with "col" and "scores" args
#' scores <- withr::with_seed(101, rnorm(nrow(tr)))
#' plot_pca_pairs(pca, sample.scores = scores, apt.col = "red")
#' @importFrom ggplot2 ggplot theme_void geom_text scale_y_continuous
#' @importFrom ggplot2 theme element_blank element_line unit
#' @export
plot_pca_pairs <- function(data.prcomp, dims = 1:5L, tr.data = NULL,
                           apt.scores = NULL, sample.classes = NULL,
                           sample.scores = NULL, apt.col = NULL, aptamers = NULL,
                           aptamers2 = NULL, aptamers3 = NULL, aptamers4 = NULL,
                           aptamers5 = NULL, apt.classes = NULL,
                           auto_ident = TRUE, ...) {

  if ( !inherits(data.prcomp, "prcomp") ) {
    stop(
      "Data `prcomp` incorrect.\n",
      "Are you passing the supervised peel object? ",
      "Try `$orig` or `$weighted`", call. = FALSE
    )
  }

  L <- length(dims)
  plot_list <- list()

  for ( i in 1:L ) {
    for ( j in 1:L ) {
      if ( i == j ) {
        exp.var <- (data.prcomp$sdev[dims[i]]^2) / sum(data.prcomp$sdev^2) * 100
        label_data <- data.frame(label = sprintf("PC %i\n(%0.2f %%)", dims[i], exp.var))
        p <- ggplot(data = label_data) +
          geom_text(data = label_data, aes(x = 0, y = 0, label = label)) +
          theme_void()
      } else if ( i < j ) {
        if ( is.null(sample.classes) && !is.null(tr.data) ) {
          sample.classes <- tr.data$Response
        }
        p <- plot_projection(data.prcomp, dims = c(dims[j], dims[i]),
                             classes = sample.classes, scores = sample.scores,
                             xlab = "", ylab = "", main = "",
                             legend.pos = "none", pt_cex = 1.5, ...) +
          scale_y_continuous(labels = function(x) format(x, digits = 1)) +
          theme(plot.margin  = margin(-0.5, -0.2, -0.5, -0.2, "cm"),
                panel.grid   = element_blank(),
                panel.border = element_rect(fill = NA),
                axis.text    = element_text(size = rel(0.6)),
                axis.ticks.length = unit(0.1, "cm"))
      } else {
        p <- plot_rotation(data.prcomp, dims = c(dims[j], dims[i]),
                          scores = apt.scores, xlab = "", ylab = "", main = "",
                          col = apt.col, classes = apt.classes,
                          aptamers = aptamers, aptamers2 = aptamers2,
                          aptamers3 = aptamers3, aptamers4 = aptamers4,
                          aptamers5 = aptamers5, auto_ident = auto_ident,
                          legend.pos = "none", pt_cex = 1.5, ...) +
          scale_y_continuous(labels = function(x) format(x, digits = 1)) +
          theme(plot.margin  = margin(-0.5, -0.2, -0.5, -0.2, "cm"),
                panel.grid   = element_blank(),
                panel.border = element_rect(fill = NA),
                axis.text    = element_text(size = rel(0.6)),
                axis.ticks.length = unit(0.1, "cm"))
      }
      plot_list <- c(plot_list, list(p))
    }
  }

  f <- grid.arrange(grobs = plot_list,
                    nrow = L, ncol = L, padding = 0,
                    widths = unit(rep(1.8, L), "in"),
                    heights = unit(rep(1.3, L), "in"))

  invisible(f)
}
