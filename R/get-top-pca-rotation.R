#' Get Top Features by PCA Dimension
#'
#' Interrogates the rotation in each dimension defined by `dims`
#'   and orders the features by the value of the principal
#'   components in that dimension. Each dimension is
#'   represented by a column of the resulting data frame.
#'
#' @param x A `pca` class object.
#' @param dims `integer(2)`. Vector of dimensions to interrogate.
#' @param n `integer(1)`. The limit on how many features
#'   to report (i.e. the rows of the resulting data frame).
#' @return A data frame of the top features ordered by the principal
#'   components given in the selected dimension.
#' @author Michael R. Mehan
#' @examples
#' pca <- pca(pcapkg:::log_rfu(simdata))
#' get_top_pca_rotation(pca, n = 10)
#' @export
get_top_pca_rotation <- function(x, dims = 1:5, n = 30L) {
  dims <- paste0("PC", dims)
  apply(x$rotation[, dims], 2, function(.x) {
    head(rownames(x$rotation)[order(abs(.x), decreasing = TRUE)], n)
  }) |>
    data.frame()
}
