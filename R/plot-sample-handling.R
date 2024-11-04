#' Plot Sample Handling (PCA)
#'
#' Quick wrapper for plotting PCA sample handling, both the sample projection
#' and the rotation (loadings) of the protein space.
#'
#' @section Legend Positions:
#' \tabular{rl}{
#'   `c(1, 1)` \tab top right \cr
#'   `c(1, 0)` \tab bottom right \cr
#'   `c(0, 1)` \tab top left \cr
#'   `c(0, 0)` \tab bottom left\cr
#' }
#'
#' @family PCA plots
#' @param data.prcomp A `prcomp` object, the result of
#'   `supervised_peel()` or [prcomp2()]. If the result of a
#'   call to `supervised_peel()`, any 1 of the 4 components may be
#'   plotted, e.g. `orig`, `weighted`, `unweighted`, or `peeled`.
#' @param extra_feat `character(n)`. vector of additional features
#'   to highlight, beyond the sample handling SMVs.
#' @param extra_feat_col `character(1)`. The color to be used for
#'   the additional features specified in `extra_feat` argument.
#' @param samples Character. A string corresponding to the samples to be used
#'   to color the points in the projection plot.
#' @param file File name if plot is to be saved to an external file.
#' @param matrix_type `character(1)`. Either "plasma" or "serum".
#' @param dims `integer(2)`. Which dimensions of the PCA to plot. Must be
#'   the same for the projection and rotation.
#' @param legend_pos Coordinates of the two legends, see Section below.
#' @return A `ggplot2` plot.
#' @author Stu Field
#' @examples
#' data <- pcapkg:::log_rfu(sim_adat[c(1:10L, 91:100L), ])
#' # Create fake matching SH features
#' new_seqs <- c(
#'   "seq.4124.24",   # cell lysis
#'   "seq.2811.27",   # platelet activation
#'   "seq.2242.51"    # complement activation
#' )
#' names(data)[31:33L] <- new_seqs   # assign into 'noise' features
#' pca <- center_scale(data, center = TRUE, scale = FALSE) |>
#'   strip_meta() |>
#'   prcomp2()
#'
#' plot_sample_handling(pca, samples = data$class_response, matrix_type = "p")
#' plot_sample_handling(pca, samples = data$class_response, matrix_type = "p",
#'                      extra_feat = "seq.2802.68")
#'
#' # Save to file
#' f_out <- tempfile("plot-", fileext = ".pdf")
#' plot_sample_handling(pca, samples = data$class_response, matrix_type = "p",
#'                    legend_pos = c(0, 0), file = f_out)
#' @importFrom dplyr case_when left_join
#' @importFrom ggplot2 geom_point scale_fill_manual scale_shape_manual ggsave
#' @importFrom ggplot2 theme guides element_blank element_rect margin
#' @export
plot_sample_handling <- function(data.prcomp, extra_feat = NULL, samples,
                                 file = NULL, matrix_type, dims = 1:2L,
                                 extra_feat_col = "firebrick3",
                                 legend_pos = c(1L, 1L)) {

  if ( inherits(data.prcomp, "supervised_peel") ) {
    stop(
      "The `data.prcomp` object should be the `prcomp2()` object only.\n",
      "Not the full list object returned by `supervised_peel()`.\n",
      "Prehaps try `data.prcomp$orig`?", call. = FALSE
    )
  }

  if ( missing(samples) ) {
    stop(
      "Must provide a `samples =` argument corresponding to ",
      "a vector of colors for the sample projection.", call. = FALSE
    )
  }

  if ( is.na(matrix_type) ) {
    stop(
      "Must provide either 'plasma' or 'serum' for ",
      "`matrix_type =` argument.", call. = FALSE
    )
  }

  matrix_type  <- match.arg(matrix_type, c("plasma", "serum"))
  pca_feats    <- rownames(data.prcomp$rotation)
  pca_feats    <- add_class(pca_feats, matrix_type)  # for the S3 method next
  sh_list      <- get_handling(x = pca_feats, add = extra_feat)
  legend_names <- c("Cell Lysis", "Platelet Activation", "Complement Activation")

  col_vec <- c(col_palette$purple,
               col_palette$lightgreen,
               col_palette$lightblue) |>
    helpr::set_Names(names(sh_list)[1:3L])

  if ( !is.null(extra_feat) ) {
    col_vec <- c(col_vec, extra_feat_col)
    legend_names <- c(legend_names, "Extra Features")
  }

  sh_colors <- rep("gray75", length(pca_feats)) |>
    helpr::set_Names(pca_feats)

  for ( i in seq_along(col_vec) ) {
    sh_colors[ pca_feats %in% sh_list[[i]] ] <- col_vec[i]
  }

  # Select shapes for overlaid points & sync names with legend labels
  pch_vec <- c(21, 24, 22, 23)[seq_along(sh_list)] |>
    helpr::set_Names(legend_names)

  rot_rnames <- rownames(data.prcomp$rotation)

  # Create rotation plot data, with annotations for colored points
  point_ann <- data.frame(seq = rot_rnames,
                          x = data.prcomp$rotation[, dims[1L]],
                          y = data.prcomp$rotation[, dims[2L]],
                          class = case_when(
                            rot_rnames %in% sh_list$cell_abuse ~ legend_names[1L], # nolint
                            rot_rnames %in% sh_list$platelet ~ legend_names[2L],   # nolint
                            rot_rnames %in% sh_list$complement ~ legend_names[3L], # nolint
                            TRUE ~ "none"))

  point_ann$class <- factor(point_ann$class,
                            levels = c(legend_names, "none"))

  if ( !is.null(extra_feat) ) {
    point_ann$class <- case_when(
      rownames(data.prcomp$rotation) %in% sh_list$extra ~ legend_names[4L],
      TRUE ~ point_ann$class
    )
  }

  names(col_vec) <- legend_names

  # Create projection plot
  p <- plot_projection(data.prcomp, dims = dims, classes = samples) +
    theme(legend.title = element_blank(),
          legend.text = element_text(size = rel(0.8)),
          legend.margin = margin(t = -0.1, l = 0.07, b = 0.07, r = 0.2,
                                 unit = "cm"),
          legend.position.inside = legend_pos,
          legend.justification   = legend_pos,
          legend.background      = element_rect(color = "black",
                                                linewidth = 0.1))
  # Create rotation plot
  r <- plot_rotation(data.prcomp, dims = dims, col = sh_colors,
                     auto_ident = TRUE) +
    geom_point(data = point_ann[point_ann$class != "none", ], # Extra feats
               aes(x = x, y = y, fill = class, shape = class),
               size = 3, color = "black") +
    scale_fill_manual(values = col_vec) + # Color for extra feats
    scale_shape_manual(values = pch_vec) +
    theme(legend.title  = element_blank(),
          legend.text   = element_text(size = rel(0.8)),
          legend.margin = margin(t = -0.1, l = 0.07, b = 0.07,
                                 r = 0.2, unit = "cm"),
          legend.position.inside = legend_pos,
          legend.justification   = legend_pos,
          legend.background      = element_rect(color = "black",
                                                linewidth = 0.1)) +
    guides(color = "none")

  gg <- withr::with_namespace("patchwork", p + r)

  if ( !is.null(file) ) {
    ht <- 4.5
    wd <- ht * 2
    ggsave(file, plot = gg, scale = 1, height = ht, width = wd, units = "in")
  }

  gg
}
