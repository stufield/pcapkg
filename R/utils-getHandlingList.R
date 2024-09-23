
#' Internal S3 methods for pulling
#'   V3 sample handling lists from the object `smvs`.
#' Separate methods for serum and plasma
#' @noRd
getHandlingList <- function(apts, ...) UseMethod("getHandlingList", apts)

# S3 plasma method
#' @noRd
getHandlingList.plasma <- function(apts, add.apts, ...) {
  ret <- list()
  ret$cell.abuse <- matchSeqIds(names(smvs$PlasmaCellAbuse), apts)
  ret$platelet   <- matchSeqIds(names(smvs$PlasmaPlatelet), apts)
  ret$complement <- matchSeqIds(names(smvs$Complement), apts)
  if ( !is.null(add.apts) ) {
    ret$add.apts <- matchSeqIds(add.apts, apts)
  }
  ret
}

# S3 serum method
#' @noRd
getHandlingList.serum <- function(apts, add.apts, ...) {
  ret <- list()
  ret$cell.abuse <- matchSeqIds(names(smvs$SerumCellAbuse), apts)
  ret$platelet   <- matchSeqIds(names(smvs$PlasmaPlatelet), apts)
  ret$complement <- matchSeqIds(names(smvs$Complement), apts)
  if ( !is.null(add.apts) ) {
    ret$add.apts <- matchSeqIds(add.apts, apts)
  }
  ret
}
