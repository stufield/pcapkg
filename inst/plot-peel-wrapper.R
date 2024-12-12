#' Plotting Wrapper for PCA Objects
#'
#' Generates a triplet of plots from a single decomposition in
#' the Supervised Peel framework. This function is a replacement for
#' [plotPCAwrapper()], which is now deprecated.
#'
#' The `supervised_peel` object contains 4 entries:
#' \describe{
#'   \item{orig:}{The original PCA}
#'   \item{weighted:}{The weighted PCA}
#'   \item{unweighted:}{The unweighted PCA}
#'   \item{peeled:}{The weighted PCA}
#' }
#' This wrapper individually plots each of these entries in sequence as
#' part of the S3 plot method for `supervised_peel` objects. For each
#' entry, a vertical panel of 3 plots is generated:
#' \describe{
#'   \item{Scree Plot:}{Plot of the principal components. This plot is used to
#'     determine the number of factors to retain when performing a PCA.}
#'   \item{Rotation:}{Also called a "loadings" plot. This contains the
#'     features. A scatter plot of the features in 2 dimensional space.}
#'   \item{Projection:}{A scatter plot of the projected samples into 2 dimensional space.}
#' }
#'
#' @family PCA plots
#' @inheritParams plot_pca_dims
#' @inheritParams plot_projection
#' @inheritParams plot_rotation
#' @inheritParams supervised_peel
#' @param sample.bg Deprecated. Background for sample points.
#' @param sample.classes Character. A vector corresponding to the class labels
#'   to color the PCA projection.
#' @param sample.scores Optional. KS scores to pass through for the coloring
#'   of the projection points during plotting.
#' @param apt.scores Optional. KS scores to pass through for the coloring
#'   of the rotation points during plotting.
#' @param ft.col Character. Color for the SOMAmer reagents in the plot rotation,
#'    i.e. the loadings.
#' @param sample.col Character. Color for the samples in the projection plot.
#'   This overrides the `sample.classes =` argument.
#' @param tr.data A `tr_data` class training data set. If KS distances are to
#'   be calculated for coloring the scree plot, this argument *must* be passed.
#' @param skip.layout Deprecated.
#' @param main Character. Main title for the top of the final figure in the
#'   \eqn{3 x 1} layout.
#' @param sample.xlim Numeric. Optional limits for the projection x-axis in
#'   the format `c(0, 0)`.
#' @param sample.ylim Numeric. Optional limits for the projection y-axis in
#'   the format `c(0, 0)`.
#' @param apt.bg Deprecated. Background color for the aptamers in the loadings
#'   (rotation) plots.
#' @param apt.classes A vector the same length as aptamers to color the points
#'   in the rotation.
#' @param apt.auto.ident Logical. See [plot_rotation()].
#' @param apt.xlim Numeric. Optional limits for the rotation x-axis in
#'   the format `c(0, 0)`.
#' @param apt.ylim Numeric. Optional limits for the rotation y-axis in
#'   the format `c(0, 0)`.
#' @param report.layout Deprecated.
#' @param sample.cex Numeric. Size of points for the projection plot.
#' @param apt.cex Numeric. Size of points for the rotation plot.
#' @param apt.pch Character expansion and symbols for the rotation plot.
#' @param ... Additional arguments passed to *either* [plot_rotation()]
#'   or [plot_projection()].
#' @return A vertical 3 panel plot. See _Details_.
#' @author Michael Mehan, Amanda Hiser
#' @seealso [pca()], [supervised_peel()]
#' @examples
#' pca <- center_scale(log_rfu(sim_adat), center = TRUE, scale = FALSE) |>
#'   feature_matrix() |>
#'   prcomp2()
#' tr <- libml::create_train(sim_adat, group.var = class_response)
#' apts <- withr::with_seed(1, sample(get_analytes(sim_adat), 10L))
#' plot_peel_wrapper(pca, tr.data = tr, aptamers = apts)
#'
#' # Using a "supervised_peel" object (apts included)
#' spp <- supervised_peel(tr, aptamers = apts)
#' plot_peel_wrapper(spp$orig, aptamers = spp$apts$aptamers)                # no colors
#' plot_peel_wrapper(spp$orig, aptamers = spp$apts$aptamers, tr.data = tr)  # colors
#' @importFrom gridExtra grid.arrange
#' @export
plot_peel_wrapper <- function(data.prcomp, dims = 1:2L,
                              sample.classes = NULL, samples = NULL,
                              sample.scores = NULL, apt.scores = NULL,
                              ft.col = NULL, sample.col = NULL,
                              tr.data = NULL, main = NULL,
                              aptamers = NULL, aptamers2 = NULL,
                              aptamers3 = NULL, aptamers4 = NULL,
                              aptamers5 = NULL,
                              sample.xlim = NULL, sample.ylim = NULL,
                              apt.classes = NULL, apt.auto.ident = TRUE,
                              apt.xlim = NULL, apt.ylim = NULL,
                              sample.cex = 2.5, apt.cex = 2.5, apt.pch = 21, ...) {

  if ( !is.null(tr.data) ) {
    apt.scores <- suppressWarnings(
      calcr::calc.ks(tr.data)$stat_table[get_analytes(tr.data), "ks_dist"]
    )
    if ( is.null(sample.classes) ) {
      sample.classes <- tr.data$Response
    }
  }

  if ( !is.null(data.prcomp$sdev) ) {
    scree <- screeplot_auc(data.prcomp, auc.classes = sample.classes) +
      labs(title = main)
  }

  rot <- plot_rotation(data.prcomp, dims = dims, classes = apt.classes,
                       scores = apt.scores, col = ft.col,
                       xlim = apt.xlim, ylim = apt.ylim,
                       pt_cex = apt.cex, auto_ident = apt.auto.ident,
                       aptamers = aptamers, aptamers2 = aptamers2,
                       aptamers3 = aptamers3, aptamers4 = aptamers4,
                       aptamers5 = aptamers5, ...)

  proj <- plot_projection(data.prcomp, dims = dims, classes = sample.classes,
                          scores = sample.scores, col = sample.col, samples = samples,
                          xlim = sample.xlim, ylim = sample.ylim,
                          pt_cex = sample.cex, ...) +
    theme(legend.margin = margin(t = -0.1, l = 0.07, b = 0.07, r = 0.2,
                                 unit = "cm"),
          legend.position = c(1, 0),
          legend.justification = c(1, 0),
          legend.background = element_rect(color = "black",
                                           linewidth = 0.1))

  grid.arrange(scree, rot, proj, nrow = 3L)
}

