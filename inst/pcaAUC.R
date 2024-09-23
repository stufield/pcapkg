#' Calculate the AUC of PCAs
#'
#' Calculate the AUC of the principal components given
#' a training set with a `Response` column.
#'
#' @param pca.data An object of class `pca`. The `projection` element _must_
#'   contain a `Response` column, which will be the case if using
#'   [SomaClassify::convert2TrainingData()] to generate the input data to [pca()].
#' @param dim Integer. Which dimension to use.
#' @return An AUC of the PCA sample projection coefficients
#'   given the binary classes defined in the `Response` column
#'   of the training data.
#' @author Mike Mehan
#' @examples
#' data <- SomaClassify::convert2TrainingData(sim_test_data, group.var = class_response)
#' x    <- pca(log10(data))
#' pcaAUC(x, 1L)
#'
#' @importFrom SomaClassify calcPepeAUC
#' @export
pcaAUC <- function(pca.data, dim) {
  stopifnot(inherits(pca.data, "pca"))
  respvec <- pca.data$projection$Response
  .col <- paste0("PC", dim)
  stopifnot(.col %in% names(pca.data$projection))
  calcPepeAUC(
    as.character(respvec),
    pca.data$projection[[.col]],
    pos.class = levels(respvec)[2L]
  )
}
