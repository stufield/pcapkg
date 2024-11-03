#' ---------------------------------
#' Generate and save "PCA" package
#' objects to `data/pca.rda`
#' Generate PCA SMVs
#' @author Stu Field
#' ---------------------------------
#' Makefile$: make objects
#' ---------------------------------
loadSmvFromFile <- function(file) {
  smv_csv    <- utils::read.csv(file, header = TRUE, row.names = 1)
  smv        <- smv_csv[, 1L]
  names(smv) <- rownames(smv_csv)
  smv
}

smvs <- list()
smvs$SerumCellAbuse  <- loadSmvFromFile("inst/data-raw/csv/smv_cell_abuse.csv")
smvs$PlasmaCellAbuse <- loadSmvFromFile("inst/data-raw/csv/smv_plasma_cell_abuse.csv")
smvs$PlasmaPlatelet  <- loadSmvFromFile("inst/data-raw/csv/smv_plasma_plat.csv")
smvs$Complement      <- loadSmvFromFile("inst/data-raw/csv/smv_comp.csv")
smvs <- lapply(smvs, function(.x) helpr::set_Names(.x, nms = SomaDataIO::getSeqId, trim = TRUE))

dat <- utils::read.csv("inst/data-raw/csv/V3_sample_handling.csv",
                       stringsAsFactors = FALSE)
list_names <- dplyr::select(dat, Serum_SH, Plasma_SH) |>
  unlist() |> unique() |> sort()
list_names <- list_names[-1L]   # drop 1st element

sh_list <- lapply(list_names, function(.x) dat$SeqId[dat$Plasma_SH == .x])
names(sh_list) <- gsub("Lysis", "plasmaLysis",
                       strsplit(list_names, " ") |> vapply(`[[`, i = 1L, ""))
sh_list$serumLysis <- dat$SeqId[dat$Serum_SH == grep("Lysis", list_names, value = TRUE)]
sh_list <- lapply(sh_list, function(.x) strsplit(.x, "_") |> vapply(`[[`, i = 1L, ""))
save(smvs, sh_list, file = "R/sysdata.rda", compress = "xz")
