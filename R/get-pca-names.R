#' Get PCA Names
#'
#' Determine the names of the features (rotation) or samples
#'   (projection) above/below a specified cutoff (value) in the
#'   principal component dimension specified by `dim`.
#'
#' @param x A `pca` class object. See [pca()].
#' @param type `character(1)`. Matched. Either `"rotation"` or `"projection"`.
#' @param dim `integer(2)`. Which PCA dimension to select.
#' @param value `numeric(1)`. The value corresponding to the
#'   dimension to interrogate for names.
#'
#' @return A character string corresponding to the feature names
#'   that exceed `value` in the dimension `dim`.
#'
#' @author Stu Field
#' @seealso [pca()]
#'
#' @examples
#' pca <- pca(pcapkg:::log10_ft(simdata))
#' get_pca_names(pca, "r", 1, 0.1)      # feature names
#'
#' get_pca_names(pca, "p", value = 0.1) # sample names
#' @importFrom dplyr filter
#' @export
get_pca_names <- function(x, type = c("rotation", "projection"),
                          dim = 1L, value) {
  stopifnot(inherits(x, "pca"))
  type <- match.arg(type)
  dim  <- paste0("PC", dim)
  var  <- ifelse(type == "rotation", "Feature", ".id")
  gt_lt <- ifelse(value < 0, `<`, `>`)   # greater-than; less-than
  dplyr::filter(x[[type]], gt_lt(!!rlang::sym(dim), value))[[var]]
}
