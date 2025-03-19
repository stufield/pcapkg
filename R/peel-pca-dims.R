#' PCA Peel by Dimension (internals)
#'
#' @param data A data set (data matrix) to peel.
#' @param data.prcomp A `prcomp` class object, ideally the return object
#'   from [prcomp2()].
#' @param dims `numeric(2)`. Which dimensions to peel.
#'
#' @return New peeled data frame based on the `prcomp` object.
#'
#' @noRd
peel_pca_dims <- function(data, data.prcomp, dims) {
  for ( peel_dim in dims ) {
    new_peel <- apply(data, 1, peel_pca, data.prcomp$rotation[, peel_dim])
  }
  data.frame(t(new_peel))
}

#' Peel away a PCA dimension given a row of proteomic data
#'   and a vector of eigen vectors from a PCA rotation. Used
#'   as internal for other PCA utilities.
#'
#' @param data_row A row of proteomic data, typically pre-scaled.
#' @param eigen_vec A vector of eigen vectors from a column
#'   of a PCA rotation.
#'
#' @noRd
peel_pca <- function(data_row, eigen_vec) {
  dot_p     <- sum(data_row * eigen_vec)
  norm_f    <- sum(eigen_vec * eigen_vec)
  proj_term <- (dot_p / norm_f) * eigen_vec
  data_row - proj_term
}
