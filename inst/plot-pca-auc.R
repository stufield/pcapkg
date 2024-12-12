#' Plot PCA AUCs vs Dimension
#'
#' @describeIn pca_auc
#'   Plot the dimension specific AUC against each dimension.
#' @family PCA plots
#' @param dims Integer vector. Which dimensions to plot on the x-axis.
#' @param col.point The color of the points. See [par()].
#' @param point.shape The shape of the points. See [shape()].
#' @param col.line The color of the line segments. See [par()].
#' @param linetype The linetype of the line segments. See [linetype()].
#' @author Mike Mehan, Amanda Hiser
#' @examples
#' # Can utilize the same preparation steps as pca_auc()
#' data <- libml::create_train(simdata, group.var = class_response)
#' x    <- pca(log_rfu(data))
#' plot_pca_auc(x)
#'
#' # Change point color and linetype on the plot
#' plot_pca_auc(x, col.point = "red", linetype = "dotted",
#'              col.line = "blue")
#'
#' # Restrict dimensions shown on the plot
#' plot_pca_auc(x, dims = 1:5L)
#' @importFrom ggplot2 aes geom_point geom_line
#' @export
plot_pca_auc <- function(pca.data, dims = 1:20L,
                         col.point = "black", col.line = "black",
                         point.shape = 20, linetype = 1) {

  pepeAUC <- vapply(dims, function(.x) pcaAUC(pca.data, .x), 0.1)

  df <- data.frame(x = dims, y = pepeAUC)

  ggplot(df, aes(x = x, y = y)) +
    geom_point(color = col.point, shape = point.shape) +
    geom_line(color = col.line, linetype = linetype)
}
