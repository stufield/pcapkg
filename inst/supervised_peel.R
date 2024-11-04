#' PCA Supervised Peel
#'
#' Perform principal component analysis (PCA) on a proteomic data matrix,
#' i.e. an ADAT, via the decomposition of the variance-covariance matrix.
#' [supervised_peel()] returns a list of class `supervised_peel`,
#' each element of which is of class `prcomp` (see [stats::prcomp()]).
#' For simple PCA analyses (i.e. without peeling) see [pca()].
#'
#' The major kernel driving this functionality is the [prcomp2()],
#' which was borrowed from [stats::prcomp()], with a few additional
#' return objects from the `SVD`, and hard coding some arguments.
#' [prcomp2()] returns an object of class `prcomp` containing the following:
#' \describe{
#'   \item{sdev:}{The standard deviations of the principal
#'     components (i.e. the square roots of the eigenvalues of the
#'     covariance/correlation matrix.}
#'   \item{rotation:}{The matrix of the variable loadings, (i.e. a matrix whose
#'     columns contain the eigenvectors).}
#'   \item{x:}{The sample projection of the PCA.}
#'   \item{basis:}{The basis matrix from the singular value decomposition.}
#'   \item{single.vals:}{Singular values from the SVD.}
#' }
#'
#' @inheritParams plot_rotation
#' @param center Logical. Should the features/variables be zero centered prior to
#'   decomposition? This is generally recommended so that variables are in the same
#'   space prior to analysis.
#' @param scale Logical. Should the features be scaled to have unit variance prior
#'   to decomposition? If `TRUE`, this corresponds to PCA in correlation space
#'   as opposed to covariance space.
#' @param num.pcs `integer(1)`. The number of principal components for the peeled
#'   decomposition.
#' @param ... Additional arguments passed to [plot_peel_wrapper()].
#' @return An object of class `supervised_peel`, a list containing
#'   multiple flavors of PCA:
#'     \item{orig}{The original PCA. This will be the same as `pca(data)`}
#'     \item{weighted}{The weighted PCA}
#'     \item{unweighted}{The unweighted PCA}
#'     \item{peeled}{The weighted PCA}
#'     \item{peeled_data}{The data matrix of peeled data}
#'     \item{center}{Whether the data was mean centered}
#'     \item{scale}{Whether the data was scaled to unit variance}
#'     \item{logged}{Whether the data was log-transformed}
#'     \item{feats}{features that were passed, in list form}
#'     \item{orig_data}{The original data matrix}
#'     \item{call}{The matched call}
#' @author Michael Mehan, Stu Field
#' @seealso [stats::prcomp()], [center_scale()]
#' @examples
#' sim <- log10(pcapkg:::log_rfu(sim_adat))
#' sim$Response <- factor(sim$class_response)  # must add Response column (plotting)
#' set1 <- attributes(sim)$sig_feats$class
#' set2 <- attributes(sim)$sig_feats$reg
#' set3 <- attributes(sim)$sig_feats$surv
#' sp1   <- supervised_peel(sim, set1 = set1, set2 = set2, set3 = set3)
#' @importFrom tibble tibble
#' @export
supervised_peel <- function(data, set1, center = TRUE, scale = FALSE, num.pcs = 1,
                            set2 = NULL, set3 = NULL, set4 = NULL, set5 = NULL, ...) {

  .call <- match.call(expand.dots = TRUE)

  if ( missing(set1) ) {
    stop(
      "You have not passed an `set1 =` argument. ",
      "This is incompatible with the intended use.", call. = FALSE
    )
  }

  logspace <- is.logspace(data)
  feats    <- get_analytes(data)

  if ( inherits(data, "grouped_df") ) {
    # if `tr_data` object; use `ungroup::tr_data()` method
    # makes c("soma_adat", "data.frame")
    data <- dplyr::ungroup(data)
  }

  # Perform centering and scaling outside of `prcomp2()`
  scaled_data <- center_scale(data, center = center, scale = scale) |>
    strip_meta()

  # careful here, we need to keep the rownames
  # but there is a as_tibble() call in center_scale()
  # must keep rownames -> rownames check below
  stopifnot(all(rownames(scaled_data) == rownames(data)))

  # Perform initial PCA
  orig_pca <- prcomp2(scaled_data)

  if ( is.null(set1) || !inherits(set1, "character") || length(set1) == 0L ) {
    print(set1)
    stop(
      "No `set1 =` argument found or in incorrect format. Please check.",
      call. = FALSE
    )
  }

  # Create weight mask from set1
  weight_mat <- map_plot_pch(feats, set1, set2, set3, set4, set5)$mask |>
    as.numeric() |> diag()
  # Perform weighted PCA
  weighted_data           <- scaled_data %*% weight_mat
  colnames(weighted_data) <- feats
  weighted_pca            <- prcomp2(weighted_data)

  # Apply weighted basis to un-weighted data
  unweighted_pca           <- weighted_pca
  # unweighted_pca$rotation <- MASS::ginv(as.matrix(scaled_data)) %*% # nolint
  #    weighted_pca$basis %*% diag(weighted_pca$single.vals)          # nolint
  unweighted_pca$rotation  <- MASS::ginv(diag(weighted_pca$single.vals)) %*%
    t(weighted_pca$basis) %*% scaled_data |> t()
  dimnames(unweighted_pca$rotation) <- dimnames(orig_pca$rotation)
  unweighted_pca$x <- scaled_data %*% unweighted_pca$rotation

  # Peel away first dimension
  peeled_data <- peelPCAdims(scaled_data, unweighted_pca, 1:num.pcs)
  # print(class(peeled_data))   # nolint
  peeled_pca <- scale(peeled_data, center = TRUE, scale = scale) |> prcomp2()

  # Undo the center/scaling
  # Hack the object here as if it were the old applyCenterScale() -> undoCenterScale()
  # I'm not sure here we actually want to log() the reference; double-log danger
  ref <- log(strip_meta(data))
  tbl <- tibble(AptName = get_analytes(data),
                means   = colMeans(ref),
                sds     = apply(ref, 2, stats::sd))
  attr(peeled_data, "par_tbl")    <- tbl
  attr(peeled_data, "center_lgl") <- TRUE
  attr(peeled_data, "scale_lgl")  <- TRUE
  peeled_data_undo <- exp(undoCenterScale(peeled_data)) # only exp() if log() above

  if ( "Response" %in% names(data) ) {
    peeled_data_undo$Response <- data$Response
  } else {
    peeled_data_undo <- cbind(data[, get_meta(data)], peeled_data_undo)
  }

  list(orig        = orig_pca,
       weighted    = weighted_pca,
       unweighted  = unweighted_pca,
       peeled      = peeled_pca,
       peeled_data = peeled_data_undo,
       center      = center,
       scale       = scale,
       logged      = logspace,
       feats       = list(set1 = set1,
                          set2 = set2,
                          set3 = set3,
                          set4 = set4,
                          set5 = set5),
       orig_data   = data,
       call = .call) |>
    add_class("supervised_peel")
}

#' @describeIn supervised_peel
#'   Generic S3 plot method for objects of class "supervised_peel".
#'
#' @inheritParams pca
#' @param x A `supervised_peel` object created via `supervised_peel()`.
#' @param ... Additional arguments passed to [plot_peel_wrapper()], such as
#'   `sample.col` (to control projection plot color) or `ft.xlim/ft.ylim` (to
#'   control x/y-axis limits in the rotation plot). These arguments are, in turn,
#'   passed to [plot_rotation()] or [plot_projection()], as appropriate.
#' @author Mike Mehan
#' @examples
#' # S3 plot method
#' plot(sp1)
#'
#' @importFrom graphics layout par
#' @export
plot.supervised_peel <- function(x, dims = 1:2, set1 = NULL,
                                 set2 = NULL, set3 = NULL,
                                 set4 = NULL, set5 = NULL, ...) {

  if ( is.null(set1) ) {
    set1 <- x$feats$set1
  }
  if ( is.null(set2) ) {
    set2 <- x$feats$set2
  }
  if ( is.null(set3) ) {
    set3 <- x$feats$set3
  }
  if ( is.null(set4) ) {
    set4 <- x$feats$set4
  }
  if ( is.null(set5) ) {
    set5 <- x$feats$set5
  }

  pch_vec <- map_plot_pch(rownames(x$orig$rotation),
                          set1, set2, set3, set4, set5)$pch

  mains <- c("Original Data", "Weighted Data", "Unweighted Data", "Peeled Data")

  plots <- liter(head(x, 4L), mains, function(.i, .main) {
    plot_peel_wrapper(.i, tr.data = x$orig_data, apt.pch = pch_vec,
                      main = .main, dims = dims,
                      set1 = set1, set2 = set2,
                      set3 = set3, set4 = set4,
                      set5 = set5, ...)
  })

  grid.arrange(grobs = plots, ncol = 4L)
}


#' @describeIn supervised_peel
#' Generic S3 print method for objects of class "supervised_peel".
#'
#' @examples
#' # S3 print method
#' sp1
#' @export
print.supervised_peel <- function(x, ...) {
  writeLines(
    signal_rule("Supervised Peel Object", lty = "double", line_col = "blue")
  )
  key <- c(
    "Original PCA",
    "Weighted PCA",
    "Un-weighted PCA",
    "Peeled PCA",
    "Centered Data",
    "Scaled Data",
    "Passed Features (len)",
    "Log-Transformed"
  ) |> pad(25)
  value <- c(
    !is.null(x$orig),
    !is.null(x$weighted),
    !is.null(x$unweighted),
    !is.null(x$peeled),
    x$center,
    x$scale,
    paste(lengths(x$feats), collapse = " | "),
    x$logged
  )
  writeLines(paste0("  ", key, value))
  writeLines(signal_rule(lty = "double", line_col = "green"))
  invisible(x)
}
