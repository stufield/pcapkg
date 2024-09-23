#' Map Aptamer Point Characters
#'
#' Map a series of vectors containing aptamer lists so that each
#' list is mapped to a specific point character value for plotting,
#' particularly in [plotRotation()].
#' Additionally, character expansion (`cex`) is also mapped if `> 2` aptamer
#' lists are passed. A masking of `0/1` is also returned that
#' is used in the weighted PCA.
#'
#' @param apts Character. A vector containing the _superset_ of ALL
#'   SOMAmer/feature names to be mapped.
#' @param aptamers Character. Required. A _subset_ of `apts`. Marked with a
#'    hollow triangle (see [pch()]).
#' @param aptamers2 Character. An optional different *subset* of `apts`. Marked
#'    with a hollow diamond (see [pch()]).
#' @param aptamers3 Character. An optional different *subset* of `apts`. Marked
#'    with a hollow square (see [pch()]).
#' @param aptamers4 Character. An optional different *subset* of `apts`. Marked
#'    with a hollow circle (see [pch()]).
#' @param aptamers5 Character. An optional different *subset* of `apts`. Marked
#'    with a hollow upside-down triangle (see [pch()]).
#' @return A list containing:
#'   \item{mask:}{A binary vector of `1/0` depending on whether the
#'     entry in `apts` is present in any of the optionally added list(s) of aptamers.}
#'   \item{pch:}{The point character symbol for each of the entries in `apts`.}
#'   \item{cex:}{The character expansion for the symbol for each
#'     of the entries in `apts`. Only used if > 2 lists are passed.}
#' @author Michael Mehan
#' @examples
#' # get the mmps
#' ad <- SomaPCA:::getAnalyteInfo(sample.adat)
#' mmps <- filter(ad, grepl("^MMP", EntrezGeneSymbol))$SeqId |>
#'   seqid2apt()
#' # get the sps
#' sps <- filter(ad, grepl("^SP", EntrezGeneSymbol))$SeqId |>
#'   seqid2apt()
#'
#' # Do cex and pch mapping
#' ret <- mapAptCharacters(SomaPCA:::getAnalytes(sample.adat), sps, mmps)
#'
#' # Look at summary mappings
#' sum(ret$mask)
#' length(mmps)
#' length(sps)
#' table(ret$pch)
#' @noRd
mapAptCharacters <- function(apts, aptamers, aptamers2 = NULL,
                             aptamers3 = NULL, aptamers4 = NULL,
                             aptamers5 = NULL, default.cex = 2.5) {

  if ( missing(aptamers) || is.null(aptamers) ) {
    stop(
      "Must pass, at minimum, an 'aptamers =' argument.\n",
      "Please pass a character string of aptamer names.\n",
      "Perhaps a sample handling list?", call. = FALSE
    )
  }

  if ( !is.null(aptamers2) ) {
    aptamers <- c(aptamers, aptamers2)
  }
  if ( !is.null(aptamers3) ) {
    aptamers <- c(aptamers, aptamers3)
  }
  if ( !is.null(aptamers4) ) {
    aptamers <- c(aptamers, aptamers4)
  }
  if ( !is.null(aptamers5) ) {
    aptamers <- c(aptamers, aptamers5)
  }

  mask <- ifelse(apts %in% matchSeqIds(aptamers, apts), 1, 0)
  pch  <- ifelse(mask == 0, 19, 23)
  cex  <- ifelse(mask == 1, 4, default.cex)

  if ( !is.null(aptamers2) ) {
    pch[apts %in% matchSeqIds(aptamers2, apts)] <- 24
    cex[apts %in% matchSeqIds(aptamers2, apts)] <- 4
  }

  if ( !is.null(aptamers3) ) {
    pch[apts %in% matchSeqIds(aptamers3, apts)] <- 22
    cex[apts %in% matchSeqIds(aptamers3, apts)] <- 4
  }

  if ( !is.null(aptamers4) ) {
    pch[apts %in% matchSeqIds(aptamers4, apts)] <- 21
    cex[apts %in% matchSeqIds(aptamers4, apts)] <- 4
  }

  if ( !is.null(aptamers5) ) {
    pch[apts %in% matchSeqIds(aptamers5, apts)] <- 25
    cex[apts %in% matchSeqIds(aptamers5, apts)] <- 4
  }

  list(mask = mask, pch = pch, cex = cex)
}
