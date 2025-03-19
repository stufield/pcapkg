#' Principal Component Analysis
#'
#' Perform principal component analysis (PCA) on a proteomic data matrix,
#'   via the decomposition of the variance-covariance matrix (`SVD`).
#'   Some modification of the standard [prcomp()] is performed.
#'   Data transformations (e.g. centering and/or scaling) can be performed
#'   via its arguments.
#'
#' @param data A `data.frame` (or `tbl_df`) class object.
#' @param features `character(n)`. Which columns to consider the features.
#' @param center `logical(1)`. Should the features/variables be zero centered
#'   prior to decomposition? In general this should be performed so that
#'   variables are in the same space, thus defaults to `TRUE`.
#' @param scale `logical(1)`. Should the features be scaled to have
#'   unit variance prior to decomposition? If `TRUE`, this corresponds
#'   to PCA in correlation space as opposed to covariance space.
#'
#' @return A `pca` class object, similar to [prcomp()].
#'
#' @seealso [prcomp2()]
#' @author Stu Field
#'
#' @examples
#' pca <- pca(pcapkg:::log10_ft(simdata))
#'
#' @importFrom dplyr ungroup left_join select all_of rename
#' @importFrom tibble as_tibble tibble
#' @export
pca <- function(data, features = NULL, center = TRUE, scale = FALSE) {

  if ( inherits(data, "soma_adat") ) {
    if ( is.null(features) ) {
      features <- get_analytes(data)
    }
    tbl <- get_col_meta(data)
    tbl$Feature <- features
  } else {
    if ( is.null(features) ) {
      stop("`features` must be passed", call. = FALSE)
    }
    tbl <- tibble(Feature = features)
  }
  not_feat <- setdiff(names(data), features)
  meta     <- dplyr::select(data, all_of(not_feat)) |>
    structure(class = "data.frame") |>  # strip class
    as_tibble(rownames = ".id")         # (important for method dispatch below)

  # check for ".id" column; essential for join below
  stopifnot(".id" %in% names(meta))

  if ( inherits(data, "grouped_df") ) {
    data <- ungroup(data)
  }

  # Perform centering and scaling outside of `prcomp2()`
  scaled_data <- center_scale(data, center = center, scale = scale) |>
    feature_matrix(features)

  # Perform initial PCA
  orig_pca <- prcomp2(scaled_data)
  rotation <- cbind(data.frame(tbl),
                    data.frame(orig_pca$rotation)) |>
    as_tibble()

  projection <- as_tibble(orig_pca$x, rownames = ".id") |>
    dplyr::left_join(x = meta, by = ".id")

  list(projection = projection,
       rotation   = rotation) |>
    structure(class       = c("pca", "list"),
              sdev        = orig_pca$sdev,
              single.vals = orig_pca$single.vals,
              basis       = orig_pca$basis)
}


#' @describeIn pca
#'   The S3 print method for `pca` class objects.
#'
#' @param x A `pca` class object.
#' @param ... Arguments required by and passed to S3 [print()].
#'
#' @examples
#' # print method
#' pca
#'
#' @export
print.pca <- function(x, ...) {
  signal_rule("PCA Object", line_col = "blue")
  key   <- c("Rotation", "Projection", "Projection variables") |> pad(25)
  value <- c(paste0(dim(x$rotation), collapse = " x "),
             paste0(dim(x$projection), collapse = " x "),
             value(grep("^PC[0-9]", names(x$projection),
                        invert = TRUE, value = TRUE)))
  rpc <- grep("^PC", names(x$rotation), value = TRUE)
  ppc <- grep("^PC", names(x$projection), value = TRUE)
  value[1L] <- paste0(value[1L], " (", rpc[1L], ":", rpc[length(rpc)], ")")
  value[2L] <- paste0(value[2L], " (", ppc[1L], ":", ppc[length(ppc)], ")")
  writeLines(paste0(key, value))
  signal_rule(lty = "double", line_col = "green")
  invisible(x)
}


#' @describeIn pca
#'   The S3 plot method for `pca` class objects.
#'
#' @param type Either "projection" (default) or "rotation". If called from
#'   [plot_scree()], either "barplot" (default) or "lines". Will be matched.
#' @param dims `integer(2)`. Which dimensions to plot.
#' @param color An unquoted string indicating the variable in
#'   `x$projection` to color points by. Required for `projection` plots.
#' @param identify Logical. Identify points? For `rotation` space, the top
#'   `10` target names in each dimension are identified.
#' @param id.labels An unquoted string indicating the variable in
#'   `x$projection` containing point labels. Ignored for `rotation` plots.
#' @param ... Additional arguments passed to [geom_point()].
#'
#' @examples
#' # S3 plot methods
#' plot(pca, "r")                         # default rotation
#' plot(pca, "r", identify = TRUE)        # label targets
#'
#' plot(pca, "p", color = class_response) # default samples projection
#' plot(pca, "p", color = gender)         # use "gender" for colors
#' plot(pca, "p", color = class_response, id.labels = id) # label points
#' plot(pca, "p", color = class_response, alpha = 0.5)    # adjust point alpha
#' @importFrom dplyr arrange filter mutate desc pull
#' @importFrom ggplot2 ggplot aes labs geom_point geom_text discrete_scale theme
#' @export
plot.pca <- function(x, type = c("projection", "rotation"), dims = 1:2L,
                     color, identify = FALSE, id.labels, ...) {

  type    <- match.arg(type)
  vars    <- prop.table(attributes(x)$sdev^2)  # variance
  vars    <- vars[dims]                        # pull 2 `dims` integers
  labels  <- paste0(paste0("PC", dims), " (", round(vars * 100, 2L), "%)")
  dims    <- paste0("PC", dims)
  sym_dim <- rlang::syms(dims)

  # if rotation
  if ( type == "rotation" ) {
    x$rotation$Group <- "X"
  } else {
    color2 <- tryCatch(eval(color), error = function(e) NULL) %||% rlang::enquo(color)
    if ( is_chr(color2) ) {
      warning("The `color` param should be a naked string!", call. = FALSE)
      color2 <- str2lang(color2)
    } else if ( rlang::quo_is_missing(color2) ) {
      stop("The `color` param cannot be missing for projections.", call. = FALSE)
    }
    x$projection <- dplyr::mutate(x$projection, Group = !!color2)
    if ( !missing(id.labels) ) {
      identify  <- TRUE
      id.labels <- rlang::enquo(id.labels)
    }
  }

  p <- ggplot(x[[type]], aes(x = !!sym_dim[[1L]], y = !!sym_dim[[2L]],
                             color = Group)) +
    geom_point(size = 2.5, alpha = 0.6, ...) +
    labs(x = labels[1L], y = labels[2L]) +
    discrete_scale("color",
                   palette = function(n) {
                     rep_len(unlist(col_palette, use.names = FALSE),
                             length.out = n)
                   }) +
    theme(legend.position = ifelse(type == "rotation", "none", "right"))

  if ( identify ) {
    if ( type == "rotation" ) {
      top1 <- x$rotation |>
        arrange(desc(abs(!!sym_dim[[1L]]))) |>
        pull("Feature") |> utils::head(10L)
      top2 <- x$rotation |>
        arrange(desc(abs(!!sym_dim[[2L]]))) |>
        pull("Feature") |> head(10L)
      id_df <- x$rotation |>
        filter(Feature %in% union(top1, top2))
      p <- p + geom_text(data  = id_df,
                         aes(label = Feature),
                         color = 1, hjust = 0,
                         size = 2.5, check_overlap = TRUE)
    } else {
      p <- p + geom_text(
        aes(label = !!id.labels),
        color = 1,  # rm this to color text with pts
        hjust = 0, size = 3, check_overlap = TRUE
      )
    }
  }
  p
}


#' Plot % variance by principal component
#'
#' The [plot_scree()] function generates a `"scree plot"`
#'   of a `pca` class object.
#'
#' @rdname pca
#'
#' @param n `integer(1)`. The number of components to plot.
#'
#' @examples
#' # Scree plots
#' plot_scree(pca)               # barplot
#' plot_scree(pca, type = "l")   # lines
#' plot_scree(pca, type = "l")
#' @importFrom tibble enframe
#' @importFrom ggplot2 geom_bar aes ggplot geom_line geom_point scale_x_continuous
#' @export
plot_scree <- function(x, n = min(15L, length(attributes(x)$sdev)),
                       type = c("barplot", "lines")) {
  type <- match.arg(type)
  data <- attributes(x)$sdev^2 |>
    enframe(name = "Component") |>
    dplyr::mutate(y = value / sum(value),
                  cum_perc = sprintf("%0.f%%", cumsum(y) * 100)) |>
    utils::head(n)

  p <- data |>
    ggplot(aes(x = Component, y = y * 100)) +
    labs(y = "% Total Variance",
         x = sprintf("Cumulative Percentage (n = %i)", n)) +
    scale_x_continuous(breaks = 1:n, labels = data$cum_perc)

  if ( type == "barplot" ) {
    p <- p + geom_bar(stat = "identity", alpha = 0.75)
  } else {
    p <- p +
      geom_point(size = 4, alpha = 0.5) +
      geom_line(size = 1, alpha = 0.75)
  }
  p
}
