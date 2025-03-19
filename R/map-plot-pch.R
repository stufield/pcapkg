#' Map Point Characters
#'
#' Map a series of vectors containing feature lists such that each
#'   list is mapped to a specific point character (`pch`) value for
#'   downstream plotting (e.g. [plot_rotation()].
#'   Additionally, character expansion (`cex`) is also mapped if `> 2`
#'   lists are passed. A masking of `TRUE/FALSE` is also returned that
#'   is used in the weighted PCA.
#'
#' @param features `character(n)`. A vector containing the _superset_ of **ALL**
#'   analyte/feature names to be mapped.
#' @param set1 `character(n)`. Required. A *subset* of `features` ... hollow triangle.
#' @param set2 `character(n)`. Optional. A *different* subset ... hollow diamond.
#' @param set3 `character(n)`. Optional. A *different* subset ... hollow square.
#' @param set4 `character(n)`. Optional. A *different* subset ... hollow circle.
#' @param set5 `character(n)`. Optional. A *different* subset ... upside-down triangle.
#' @return A tibble containing:
#'   \item{mask}{Boolean. Whether the entry in `features` is present
#'     in any of the optionally added set(s).}
#'   \item{pch}{The point character symbol for each entry.}
#'   \item{cex}{The character expansion for the symbol for each entry.}
#'
#' @author Michael Mehan
#'
#' @examples
#' # get the mmps & sps
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
map_plot_pch <- function(features,
                         set1 = NULL,
                         set2 = NULL,
                         set3 = NULL,
                         set4 = NULL,
                         set5 = NULL,
                         default_cex = 2.5) {

  if ( is.null(set1) ) {
    stop(
      "Must pass, at minimum, an `set1=` argument.\n",
      "Please pass a character string of feature names.",
      call. = FALSE
    )
  }

  all <- c(set1, set2, set3, set4, set5) |> unique()
  dplyr::left_join(
    tibble(features,  ft_id = get_seq(features)),
    tibble(all = all, ft_id = get_seq(all)),
    by = "ft_id"
  ) |>
    dplyr::mutate(
      mask = !is.na(all),
      cex  = ifelse(mask, 4, default_cex),
      pch  = dplyr::case_when(
        ft_id %in% get_seq(set1) ~ 23,
        ft_id %in% get_seq(set2) ~ 24,
        ft_id %in% get_seq(set3) ~ 22,
        ft_id %in% get_seq(set4) ~ 21,
        ft_id %in% get_seq(set5) ~ 25,
        .default = 19
      )
    )
}
