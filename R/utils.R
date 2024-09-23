
getSeqId <- getFromNamespace("getSeqId", "SomaDataIO")
matchSeqIds <- getFromNamespace("matchSeqIds", "SomaDataIO")
getMeta <- getFromNamespace("getMeta", "SomaDataIO")
getAnalytes <- getFromNamespace("getAnalytes", "SomaDataIO")
getAnalyteInfo <- getFromNamespace("getAnalyteInfo", "SomaDataIO")

addClass <- function (x, class) {
  if (is.null(class)) {
    warning("Passing `class = NULL` leaves class(x) unchanged.", call. = FALSE)
  }
  if (any(is.na(class))) {
    stop("The `class` param cannot contain `NA`: ", class, call. = FALSE)
  }
  new_class <- union(class, class(x))
  structure(x, class = new_class)
}

col_string <- c("dodgerblue", "red", "darkgreen", "darkorchid4", "cyan",
                "orange", "black", "grey", "#990066", "green", "#24135F")

par_def <- list(mgp = c(2.00, 0.75, 0.00), mar = c(3, 4, 3, 1))

#' @importFrom grDevices topo.colors
#' @noRd
topo_colors <- function(n, alpha) {
  grDevices::topo.colors(n = n, alpha = alpha, rev = TRUE)
}
