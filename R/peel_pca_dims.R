#' PCA Peel by Dimension (internals)
#'
#' @param data A data set (a RFU data matrix) to peel.
#' @param data.prcomp A `prcomp` class object, ideally the return object
#'   from [prcomp2()].
#' @param dims Numeric. Which dimensions to peel.
#' @return New peeled data frame based on the `prcomp` object.
#' @author Mike Mehan
#' @noRd
peel_pca_dims <- function(data, data.prcomp, dims) {
  for ( peel.dim in dims ) {
    new_peel <- apply(data, 1, peelPCA, data.prcomp$rotation[, peel.dim])
  }
  data.frame(t(new_peel))
}

#' Peel away a PCA dimension given a row of RFU data
#'   and a vector of eigen vectors from a PCA rotation. Used
#'   as internal for other PCA utilities.
#'
#' @param data.row A row of RFU data from an ADAT, usually pre-scaled.
#' @param eigen.vec A vector of eigen vectors from a column
#'   of a PCA rotation.
#' @author Mike Mehan
#' @noRd
peel_pca <- function(data.row, eigen.vec) {
  dot_p     <- sum(data.row * eigen.vec)
  norm_f    <- sum(eigen.vec * eigen.vec)
  proj_term <- (dot_p / norm_f) * eigen.vec
  data.row - proj_term
}
