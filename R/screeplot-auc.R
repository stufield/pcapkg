#' Plot AUC Dependent Screeplot
#'
#' Plot a screeplot with the variance bars colored according
#'   to the AUC of the corresponding classes in a specified
#'   training response vector.
#'
#' @family PCA plots
#' @inheritParams plot_pca_dims
#'
#' @param auc.classes A vector of the training classes used in calculating the AUC.
#' @param auc.proj An alternative projection matrix from another decomposition.
#'   By default, the projection from `data.prcomp` is used.
#' @param nPCs `integer(1)`. Number of PCs to plot.
#' @param ... Additional arguments passed to [geom_col()].
#'
#' @author Stu Field, Michael R. Mehan
#' @seealso [screeplot()], [plot_scree()]
#'
#' @importFrom ggplot2 ggplot aes geom_col labs scale_fill_manual
#' @importFrom ggplot2 theme element_text rel
#' @examples
#' pca <- center_scale(pcapkg:::log10_ft(simdata), center = TRUE, scale = FALSE) |>
#'   feature_matrix() |>
#'   prcomp2()
#' screeplot_auc(pca, simdata$class_response, main = "My ScreePlot by AUC")
#' @export
screeplot_auc <- function(data.prcomp, auc.classes, auc.proj = NULL, main = NULL,
                          nPCs = 20L, ...) {

  if ( is.null(auc.proj) ) {
    auc.proj <- data.prcomp$x
  }

  if ( !is.factor(auc.classes) ) {
    auc.classes <- factor(auc.classes)
  }

  nPCs <- min(ncol(auc.proj), nPCs)
  aucs <- rep(0.5, nPCs)

  if ( length(levels(auc.classes)) > 1L ) {
    aucs <- vapply(1:nPCs, function(.x) auc(auc.classes, auc.proj[, .x]), 0.1)
  }

  col_indices <- vapply(aucs, function(.x) max(1, floor((.x - 0.5) * 200)), 0.1)
  cols        <- rev(viridisLite::viridis(100))[col_indices]

  plot_data <- data.frame(PCs       = seq_along(data.prcomp$sdev),
                          Variances = data.prcomp$sdev^2)

  ggplot(plot_data[1:nPCs, ], aes(x = PCs, y = Variances)) +
    geom_col(aes(fill = factor(PCs)),
             color = "black", linewidth = 0.3, ...) +
    labs(x = NULL, title = main) +
    scale_fill_manual(values = cols) +
    theme(legend.position = "none",
          plot.title = element_text(face = "bold", hjust = 0.5,
                                    size = rel(1)))
}
