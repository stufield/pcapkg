
#' Package Objects
#'
#' @name objects
"_PACKAGE"


#' Sample Mapping Vectors
#'
#' The sample mapping vectors related to sample handling.
#' @format A list containing 4 vectors:
#' \itemize{
#'   \item{SerumCellAbuse}
#'   \item{PlasmaCellAbuse}
#'   \item{PlasmaPlatelet}
#'   \item{Complement}
#' }
#' @rdname objects
#' @source `smvs`: Mike Mehan
#' @examples
#' # ------ SomaPCA -------
#' lapply(smvs, head)
"smvs"


#' V3 Sample Handling Lists
#'
#' The "official" `SeqIds` of the V3 sample handling lists.
#' @format A list containing 4 vectors:
#' \itemize{
#'   \item{Complement}
#'   \item{Platelet}
#'   \item{plasmaLysis}
#'   \item{serumLysis}
#' }
#' @rdname objects
#' @source `sh_list`: Mike Mehan
#' @examples
#'
#' lapply(sh_list, head)
"sh_list"
