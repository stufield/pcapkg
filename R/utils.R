
get_seq  <- function(x) sub("\\.", "-", sub("^seq\\.", "", x))
add_seq <- getFromNamespace("add_seq", "helpr")
get_analytes <- getFromNamespace("get_analytes", "helpr")
get_meta <- getFromNamespace("get_meta", "helpr")

get_col_meta <- function(x) {
  attr(x, "Col.Meta") %||% tibble::tibble(feature = get_analytes(x))
}

log_rfu <- function(x) {
  cls  <- class(x)
  cols <- get_analytes(x)
  for ( i in cols ) x[[i]] <- log10(x[[i]])
  structure(x, class = cls)
}

#' @importFrom tibble tibble
#' @noRd
match_seq <- function(x, y) {
  xtbl <- tibble(x = x, seq = get_seq(x))
  ytbl <- tibble(y = as.character(y), seq = get_seq(y))  # rm class
  df <- dplyr::left_join(xtbl, ytbl, by = "seq")
  df$y[!is.na(df$y)]
}

col_palette <- list(
  purple     = "#24135F",
  lightgreen = "#00A499",
  lightgrey  = "#707372",
  magenta    = "#840B55",
  lightblue  = "#006BA6",
  yellow     = "#D69A2D",
  darkgreen  = "#007A53",
  darkblue   = "#1B365D",
  darkgrey   = "#54585A",
  blue       = "#004C97"
)

par_def <- list(mgp = c(2.00, 0.75, 0.00), mar = c(3, 4, 3, 1))

#' @importFrom grDevices topo.colors
#' @noRd
topo_colors <- function(n, alpha) {
  grDevices::topo.colors(n = n, alpha = alpha, rev = TRUE)
}

