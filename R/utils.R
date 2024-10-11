
getSeqId <- getFromNamespace("getSeqId", "globalr")
matchSeqIds <- getFromNamespace("matchSeqIds", "globalr")
getMeta <- getFromNamespace("getMeta", "globalr")
getAnalytes <- getFromNamespace("getAnalytes", "globalr")
getAnalyteInfo <- getFromNamespace("getAnalyteInfo", "globalr")

col_string <- c("dodgerblue", "red", "darkgreen", "darkorchid4", "cyan",
                "orange", "black", "grey", "#990066", "green", "#24135F")

par_def <- list(mgp = c(2.00, 0.75, 0.00), mar = c(3, 4, 3, 1))

#' @importFrom grDevices topo.colors
#' @noRd
topo_colors <- function(n, alpha) {
  grDevices::topo.colors(n = n, alpha = alpha, rev = TRUE)
}
