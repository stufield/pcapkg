#' Plot Sample Projections by Meta Data
#'
#' Plot a series of sample projections from a PCA
#'   for each column of meta data in a wide breadth search
#'   for patterns in the meta data. The same rotation underlies
#'   all projections, only the coloring of the points in the
#'   projection changes between plots. Only those meta data
#'   covariates with at least 2 factor levels are plotted.
#'
#' @family PCA plots
#' @inheritParams pca
#' @param ... Additional arguments passed to [supervised_peel()].
#' @note The _weighted_ PCA is used for this purpose (not the original).
#' @author Stu Field
#' @seealso [supervised_peel()]
#' @examples
#' ft <- attr(sim_adat, "sig_feat")$class_response
#' plot_projection_meta(pcapkg:::log_rfu(sim_adat), aptamers = ft)
#' @importFrom dplyr select select_if left_join
#' @importFrom tibble as_tibble
#' @importFrom ggplot2 ggplot aes geom_point facet_wrap
#' @export
plot_projection_meta <- function(data, dims = 1:2L, ...) {
  good_meta <- function(.x) length(unique(.x)) > 1L & length(unique(.x)) <= 8L
  meta <- dplyr::select_if(data[, get_meta(data)], good_meta) |>
    as_tibble(rownames = ".id")
  df <- supervised_peel(data, ...)$weighted$x |>
    as_tibble(rownames = ".id") |>
    dplyr::select(paste0("PC", dims), ".id") |>
    dplyr::left_join(meta, by = ".id")

  symbls <- rlang::syms(paste0("PC", dims))
  x <- symbls[[1L]]
  y <- symbls[[2L]]

  df |>
    dplyr::select(-.id) |>
    tidyr::gather(key = "meta", value = "Group", -!!x, -!!y) |>
    ggplot(aes(x = !!x, y = !!y, colour = Group)) +
    geom_point(alpha = 0.5) +
    facet_wrap(. ~ meta)
}
