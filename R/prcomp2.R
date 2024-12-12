#' Principal Component Analysis
#'
#' Performs a principal components analysis on the given data matrix
#' and returns the results as an object of class `prcomp`. This
#' function is stolen from [stats::prcomp()], except
#' with some extra returned components of the `SVD` that are not
#' returned by the standard function as well as hard coding
#' the projection to be returned and some simple data checks.
#'
#' @inheritParams stats::prcomp
#' @return An object of class `prcomp`.
#' @note We've simply hijacked [prcomp()] and added elements to the return value.
#' @author Stu Field & Mike Mehan
#' @seealso [prcomp()], [svd()]
#' @export
prcomp2 <- function(x, tol = NULL) {

  if ( !inherits(x, "matrix") ) {
    x  <- data.matrix(x)
  }

  if ( !all(apply(x, 2, is.numeric)) ) {
    stop(
      "Non-numerics detected in data matrix! Please run `feature_matrix(x)` upstream.",
      call. = FALSE
    )
  }

  sc <- attr(x, "scaled:scale")

  if ( any( sc == 0 ) ) {
    stop("Cannot rescale a constant/zero column to unit variance.", call. = FALSE)
  }

  s   <- base::svd(x)
  D   <- s$d
  s$d <- s$d / sqrt(max(1, nrow(x) - 1))

  if ( !is.null(tol) ) {
    # we get rank at least one even for a 0 matrix.
    signal_done("thresholding to `tol`:", value(tol))
    rank <- sum(s$d > (s$d[1L] * tol))
    if ( rank < ncol(x) ) {
      s$v <- s$v[, 1L:rank, drop = FALSE]
      s$d <- s$d[1L:rank]
    }
  }

  dimnames(s$v) <- list(colnames(x), paste0("PC", seq_len(ncol(s$v))))

  list(
    sdev        = s$d,            # variance
    rotation    = s$v,            # rotation-loadings
    x           = (x %*% s$v),    # projection-sample space
    basis       = s$u,            # basis
    single.vals = D               # eigen vectors
  ) |>
    structure(class = "prcomp")
}
