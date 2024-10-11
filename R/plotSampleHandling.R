#' Plot Sample Handling (PCA)
#'
#' Quick wrapper for plotting PCA sample handling, both the sample projection
#' and the rotation (loadings) of the protein space.
#'
#' For the legend positions:
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
#' @param add.apts Vector of additional SOMAmer reagents to highlight, beyond
#'   the sample handling SMVs.
#' @param samples Character. A string corresponding to the samples to be used
#'   to color the points in the projection plot.
#' @param filename Optional. File name if plot is to be saved to an
#'   external file. Include file extension (ex. `filename = "example_file.png"`),
#'   as file type will be inferred from the extension.
#' @param scale Optional. Scale (as a numeric value) for the saved plot.
#' @param matrix.type Which matrix type ("plasma" or "serum") to use for the
#'   cell abuse SMVs. Partial matching is allowed.
#' @param dims Numeric. Length 2. Which dimensions of the PCA to plot. Must be
#'   the same for the projection and rotation.
#' @param add.apt.col Character. The color to be used for the additional apts
#'   specified in `add.apts` argument.
#' @param legend.pos Coordinates of the two legends, in `c(0, 0)` format. See Details.
#' @return A sample handling plot.
#' @author Stu Field, Amanda Hiser
#' @seealso [points()]
#' @examples
#' data <- log10(sim_test_data[c(1:10L, 91:100L), ])
#' # Create fake matching SH analytes
#' new_seqs <- c(
#'   "seq.4124.24",   # cell lysis
#'   "seq.2811.27",   # platelet activation
#'   "seq.2242.51"    # complement activation
#' )
#' names(data)[31:33L] <- new_seqs   # assign into 'noise' apts
#' pca <- center_scale(data, center = TRUE, scale = FALSE) |>
#'   strip_meta() |>
#'   prcomp2()
#'
#' plotSampleHandling(pca, samples = data$class_response, matrix.type = "p")
#' plotSampleHandling(pca, samples = data$class_response, matrix.type = "p",
#'                    add.apts = "seq.2802.68")
#'
#' # Save to file
#' f_out <- tempfile("SHplot-", fileext = ".pdf")
#' plotSampleHandling(pca, samples = data$class_response, matrix.type = "p",
#'                    legend.pos = c(0, 0), filename = f_out)
#' @importFrom lifecycle deprecated is_present deprecate_soft
#' @importFrom dplyr case_when left_join
#' @importFrom ggplot2 geom_point scale_fill_manual scale_shape_manual
#' @importFrom ggplot2 theme guides element_blank element_rect
#' @importFrom ggplot2 margin ggsave
#' @importFrom gridExtra grid.arrange
#' @export
plotSampleHandling <- function(data.prcomp, add.apts = NULL, samples,
                               filename = NULL, scale = 1, matrix.type,
                               dims = 1:2L, add.apt.col = "firebrick3",
                               legend.pos = c(1, 1)) {

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

  if ( is.na(matrix.type) ) {
    stop(
      "Must provide either 'plasma' or 'serum' for ",
      "`matrix.type =` argument.", call. = FALSE
    )
  }

  matrix.type  <- match.arg(matrix.type, c("plasma", "serum"))
  pca_apts     <- rownames(data.prcomp$rotation)
  pca_apts     <- addClass(pca_apts, matrix.type)  # for the S3 method next
  apt_list     <- getHandlingList(apts = pca_apts, add.apts = add.apts)
  legend_names <- c("Cell Lysis", "Platelet Activation", "Complement Activation")

  col_vec <- c(SomaPlotr::soma_colors2$pink,
               SomaPlotr::soma_colors2$blue,
               SomaPlotr::soma_colors2$green) |>
    globalr::set_Names(names(apt_list)[1:3L])

  if ( !is.null(add.apts) ) {
    col_vec <- c(col_vec, add.apt.col)
    legend_names <- c(legend_names, "Suppl. Analytes")
  }

  apt_colors <- rep("gray75", length(pca_apts)) |>
    globalr::set_Names(pca_apts)

  for ( i in seq_along(col_vec) ) {
    apt_colors[ pca_apts %in% apt_list[[i]] ] <- col_vec[i]
  }

  # Select shapes for overlaid points & sync names with legend labels
  apt_pch_vec <- c(21, 24, 22, 23)[seq_along(apt_list)] |>
    globalr::set_Names(legend_names)

  rot_rnames <- rownames(data.prcomp$rotation)

  # Create rotation plot data, with annotations for colored points
  point_ann <- data.frame(seq = rot_rnames,
                          x = data.prcomp$rotation[, dims[1L]],
                          y = data.prcomp$rotation[, dims[2L]],
                          class = case_when(
                            rot_rnames %in% apt_list$cell.abuse ~ legend_names[1L], # nolint
                            rot_rnames %in% apt_list$platelet ~ legend_names[2L],   # nolint
                            rot_rnames %in% apt_list$complement ~ legend_names[3L], # nolint
                            TRUE ~ "none"))

  point_ann$class <- factor(point_ann$class,
                            levels = c(legend_names, "none"))

  if ( !is.null(add.apts) ) {
    point_ann$class <- case_when(
      rownames(data.prcomp$rotation) %in% apt_list$add.apts ~ legend_names[4L],
      TRUE ~ point_ann$class
    )
  }

  names(col_vec) <- legend_names

  # Create projection plot
  p <- plotProjection(data.prcomp, dims = dims, classes = samples) +
    theme(legend.title = element_blank(),
          legend.text = element_text(size = rel(0.8)),
          legend.margin = margin(t = -0.1, l = 0.07, b = 0.07, r = 0.2,
                                 unit = "cm"),
          legend.position.inside = legend.pos,
          legend.justification = legend.pos,
          legend.background = element_rect(color = "black",
                                           linewidth = 0.1))
  # Create rotation plot
  r <- plotRotation(data.prcomp, dims = dims, col = apt_colors,
                    auto.ident = TRUE) +
    geom_point(data = point_ann[point_ann$class != "none", ], # Additional apts
               aes(x = x, y = y, fill = class, shape = class),
               size = 3, color = "black") +
    scale_fill_manual(values = col_vec) + # Color for addl apts
    scale_shape_manual(values = apt_pch_vec) +
    theme(legend.title = element_blank(),
          legend.text = element_text(size = rel(0.8)),
          legend.margin = margin(t = -0.1, l = 0.07, b = 0.07, r = 0.2,
                                 unit = "cm"),
          legend.position.inside = legend.pos,
          legend.justification = legend.pos,
          legend.background = element_rect(color = "black",
                                           linewidth = 0.1)) +
    guides(color = "none")

  f <- grid.arrange(p, r, ncol = 2L)

  if ( !is.null(filename) ) {
    ht <- 4.5
    wd <- ht * 2
    ggsave(filename, plot = f, scale = scale,
           height = ht, width = wd,
           units = "in")
  }

  invisible(f)
}
