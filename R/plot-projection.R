#' Plot PCA Projection
#'
#' Plot the samples from a PCA projection in a 2-dimensional scatter plot.
#'
#' @family PCA plots
#' @inheritParams plot_pca_dims
#'
#' @param classes Optional. A vector indicating the classes of
#'   samples used for coloring the points. Must be the same length
#'   as the number of samples.
#' @param samples Optional. A vector of sample IDs to mark
#'   on the projection plot. Specified samples are marked
#'   with a hollow diamond (see [pch()]).
#' @param ... Additional arguments passed to [plot_pca_dims()].
#'
#' @author Michael Mehan
#'
#' @examples
#' pca <- center_scale(pcapkg:::log10_ft(simdata), center = TRUE, scale = FALSE) |>
#'   feature_matrix() |>
#'   prcomp2()
#'
#' # Define color of points
#' plot_projection(pca, col = "green")
#'
#' # Use classes to define point colors
#' plot_projection(pca, classes = simdata$class_response)
#'
#' # Mark specific samples
#' plot_projection(pca, samples = rownames(simdata)[1:5L])
#' @export
plot_projection <- function(data.prcomp, dims = 1:2L,
                            classes = NULL, scores = NULL, col = NULL,
                            samples = NULL, pt_cex = 2.5, ...) {
  if ( !is.null(classes) && nrow(data.prcomp$x) != length(classes) ) {
    stop(
      "Inappropriate length of the `classes =` argument.\n",
      "  Currently: ", value(length(classes)),
      "\n  Should be: ", value(nrow(data.prcomp$x)), call. = FALSE
    )
  }

  if ( !is.null(samples) ) {
    pt_pch <- ifelse(rownames(data.prcomp$x) %in% samples, 23, 19)
    pt_cex <- ifelse(rownames(data.prcomp$x) %in% samples, 4, 2.5)
  } else {
    pt_pch <- 19
  }

  plot_pca_dims(data.prcomp, dims = dims, value = "x",
                classes = classes, scores = scores, col = col,
                pt_cex = pt_cex, pt_pch = pt_pch, ...)
}
