#' Plot AUC Dependent Screeplot
#'
#' Plot a screeplot with the variance bars colored according
#' to the AUC of the corresponding classes in a specified
#' training response vector.
#'
#' @family PCA plots
#' @inheritParams plotPCAdims
#' @param auc.classes A vector of the training classes used in calculating the AUC.
#' @param auc.proj An alternative projection matrix from another decomposition.
#'   By default, the projection from `data.prcomp` is used.
#' @param nPCs Integer. Number of PCs to plot. Defaults to 20.
#' @param ... Additional arguments passed to [geom_col()].
#' @author Michael Mehan, Amanda Hiser
#' @seealso [screeplot()], [plotScree()]
#' @importFrom ggplot2 ggplot aes geom_col labs scale_fill_manual
#' @importFrom ggplot2 theme element_text rel
#' @examples
#' pca <- center_scale(log10(sim_adat), center = TRUE, scale = FALSE) |>
#'   strip_meta() |>
#'   prcomp2()
#' screeplotAUC(pca, sim_adat$class_response, main = "My ScreePlot by AUC")
#' @export
screeplotAUC <- function(data.prcomp, auc.classes, auc.proj = NULL, main = NULL,
                         nPCs = 20, ...) {

  if ( is.null(auc.proj) ) {
    auc.proj <- data.prcomp$x
  }

  if ( !is.factor(auc.classes) ) {
    auc.classes <- factor(auc.classes)
  }

  nPCs <- min(ncol(auc.proj), nPCs)
  aucs <- rep(0.5, nPCs)

  if ( length(levels(auc.classes)) > 1L ) {
    aucs <- vapply(1:nPCs, function(.x) {
      caTools::colAUC(auc.proj[, .x], auc.classes)}, 0.1) |> # nolint
      as.numeric()
  }

  col_indices <- vapply(aucs, function(.x) max(1, floor((.x - 0.5) * 200)), 0.1)
  cols        <- rev(viridisLite::viridis(100))[col_indices]

  plot.data <- data.frame(PCs = seq_along(data.prcomp$sdev),
                          Variances = data.prcomp$sdev^2)

  ggplot(plot.data[1:nPCs, ], aes(x = PCs, y = Variances)) +
    geom_col(aes(fill = factor(PCs)),
             color = "black", linewidth = 0.3, ...) +
    labs(x = NULL, title = main) +
    scale_fill_manual(values = cols) +
    SomaPlotr::theme_soma() +
    theme(legend.position = "none",
          plot.title = element_text(face = "bold", hjust = 0.5,
                                    size = rel(1)))
}
