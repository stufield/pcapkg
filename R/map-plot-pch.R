#' Map Point Characters
#'
#' Map a series of vectors containing feature lists such that each
#' list is mapped to a specific point character (`pch`) value for
#' downstream plotting (e.g. [plotRotation()].
#' Additionally, character expansion (`cex`) is also mapped if `> 2` aptamer
#' lists are passed. A masking of `TRUE/FALSE` is also returned that
#' is used in the weighted PCA.
#'
#' @param apts Character. A vector containing the _superset_ of ALL
#'   analyte/feature names to be mapped.
#' @param aptamers Character. Required. A _subset_ of `apts`.
#'   Sequence ID matched ... hollow triangle.
#' @param aptamers2 Character. Optional. A *different * subset.
#'   Sequence ID matched ... hollow diamond.
#' @param aptamers3 Character. Optional. A *different* subset.
#'   Marked with a hollow square (see [pch()]).
#'   Sequence ID matched ... hollow square. 
#' @param aptamers4 Character. Optional. A *different* subset.
#'   Sequence ID matched ... hollow circle. 
#' @param aptamers5 Character. Optional. A *different* subset.
#'   Sequence ID matched ... upside-down triangle.
#' @return A tibble containing:
#'   \item{mask}{Logical. A binary depending on whether the entry in
#'     `apts` is present in any of the optionally added set(s).}
#'   \item{pch}{The point character symbol for each entry.}
#'   \item{cex}{The character expansion for the symbol for each entry.}
#' @author Michael Mehan
#' @examples
#' # get the mmps
#' ad <- pcapkg:::get_col_meta(sample.adat)
#' mmps <- filter(ad, grepl("^MMP", EntrezGeneSymbol))$SeqId
#' sps  <- filter(ad, grepl("^SP", EntrezGeneSymbol))$SeqId
#'
#' # Do cex and pch mapping
#' foo <- map_plot_pch(pcapkg:::get_analytes(sample.adat), sps, mmps)
#'
#' # Look at summary mappings
#' sum(ret$mask)
#' length(mmps)
#' length(sps)
#' table(ret$pch)
#' @noRd
map_plot_pch <- function(apts,
                         aptamers  = NULL,
                         aptamers2 = NULL,
                         aptamers3 = NULL,
                         aptamers4 = NULL,
                         aptamers5 = NULL,
                         default.cex = 2.5) {

  if ( is.null(aptamers) ) {
    stop(
      "Must pass, at minimum, an `aptamers =` argument.\n",
      "Please pass a character string of feature names.",
      call. = FALSE
    )
  }

  aptamers <- c(aptamers, aptamers2, aptamers3, aptamers4, aptamers5)
  dplyr::left_join(
    tibble(apts,     seq = get_seq(apts)),
    tibble(aptamers, seq = get_seq(aptamers)),
    by= "seq"
    ) |>
    dplyr::mutate(
      mask = !is.na(aptamers),
      cex  = ifelse(mask, 4, default.cex),
      pch  = dplyr::case_when(
        seq %in% get_seq(aptamers)  ~ 23,
        seq %in% get_seq(aptamers2) ~ 24,
        seq %in% get_seq(aptamers3) ~ 22,
        seq %in% get_seq(aptamers4) ~ 21,
        seq %in% get_seq(aptamers5) ~ 25,
        .default = 19
      )
  )
}
