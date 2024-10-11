#' Plot Points Ellipse (deprecated)
#'
#' Create a points ellipse plot.
#'
#' @param x A matrix (n x 2) decomposition of either
#'   the projection ("x") or rotation ("rotation") from
#'   a `prcomp` object.
#' @param add Logical. Should the ellipse plot be added
#'   to an existing plot?
#' @param scale Numeric. Scale shift for the mean.
#' @param ... Additional arguments.
#' @examples
#' \dontrun{
#'   pca <- center_scale(log10(sim_test_data), center = TRUE, scale = FALSE) |>
#'     strip_meta() |> prcomp2()
#'   plotPointsEllipse(pca$rotation[, 1:2L], add = FALSE, lwd = 2, col = "dodgerblue")
#' }
#' @export
plotPointsEllipse <- function(x, add = TRUE, scale = 1, ...) {
  lifecycle::deprecate_stop("3.3.2", "SomaPCA::plotPointsElipse()")
}
