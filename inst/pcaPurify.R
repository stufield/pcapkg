#' Purify Principal Components Vectors
#'
#' Iteratively "purify" (enrich) a feature set according to their
#'   "pure" contribution to the _first_ principal component (PC1).
#'   This enrichment algorithm is seeded with an initial string
#'   of implicated features. At each successive round features
#'   are either kicked out or added depending an objective
#'   function that considers the relative coefficients of
#'   PC1/PC2 (high = good) _and_ the euclidean distance from the origin
#'   in PCs 2 and 3 (low = good).
#'
#' This procedure was used to purify the original sample mapping vectors
#' (SMVs) for the sample handling vectors. Please keep in mind that
#' generating plots for >5 iterations can be slow.
#'
#' @param tr.data A `tr_data` object with a "Response" column containing
#'   the `true` class names. See `SomaClassify::convert2TrainingData()`.
#' @param features Character. A string of possible features
#'   to initiate the search.
#' @param max.iter Integer. The maximum number of iterations to run.
#' @param start.size Integer. The maximum feature set size to seed the search.
#' @param verbose Logical. Should information about the features that
#'   were added/removed from each iteration be printed?
#' @return A list of character strings for the final iteration:
#'   \item{apts}{the features in the final iteration corresponding
#'     to features that explain the largest proportion
#'     of the total variance via principal components}
#'   \item{mu_scores}{the mean values of the objective function at each
#'     iteration}
#' Also plots the progress of each iteration with a rotation
#'   space (i.e. loadings) plot.
#' @author Mike Mehan, Amanda Hiser
#' @seealso [supervised_peel()]
#' @examples
#' \dontrun{
#'   tr <- SomaClassify::convert2TrainingData(sample.adat, "SampleGroup")
#'   sh <- getSeqId(unique(unlist(sapply(smvs, names))), trim.version = TRUE)
#'   sh <- unique(c(sh, getSeqId(unique(unlist(sh_list)), trim.version = TRUE)))
#'   sh <- matchSeqIds(sh, names(tr))
#'
#'   # Small iter, for example
#'   purify <- dplyr::select(tr, all_of(sh), Response) |>
#'     pcaPurify(sh_list$plasmaLysis, max.iter = 5)
#' }
#' @importFrom graphics points
#' @importFrom utils tail head
#' @importFrom ggplot2 ggplot aes geom_point geom_line theme labs unit rel
#' @importFrom ggplot2 scale_y_continuous element_text element_blank
#' @importFrom gridExtra grid.arrange
#' @export
pcaPurify <- function(tr.data, features, max.iter = 10L, start.size = 30L,
                      verbose = TRUE) {

  start.size <- min(start.size, length(features))
  apts       <- getAnalytes(tr.data)
  pca_data   <- dplyr::select(tr.data, getAnalytes(tr.data), "Response")
  cur_apts   <- matchSeqIds(head(features, start.size), apts)
  mean_score <- numeric(0)
  thresh     <- 1e-02
  count      <- 1L
  plot_list  <- list()

  repeat {
    if ( count > max.iter ) {
      break
    }
    curPCA <- supervised_peel(pca_data, aptamers = cur_apts,
                              scale = TRUE)$unweighted # scale = TRUE (correlation space)
    tab     <- .calcScore(curPCA)
    cur_tab <- tab[cur_apts, ]
    add_tab <- tab[setdiff(rownames(tab), cur_apts), ]
    cur_tab <- cur_tab[order(cur_tab[, "Score"], decreasing = TRUE), ]  # the IN apts
    add_tab <- add_tab[order(add_tab[, "Score"], decreasing = TRUE), ]  # the OUT apts
    # logical: which better score than worst current
    better  <- add_tab[, "Score"] > min(cur_tab[, "Score"])
    mu      <- mean(cur_tab[, "Score"])
    if ( (length(mean_score) > 0L &&
            abs(mu - tail(mean_score, 1L)) < thresh) || sum(better) == 0 ) {
      break
    }
    mean_score <- c(mean_score, mu)
    delta      <- min(floor(length(features) / 10L), sum(better))
    add_apts   <- head(add_tab[ better, ], delta) |> rownames()
    rem_apts   <- tail(cur_tab, length(add_apts)) |> rownames()
    # plotting vars & objects
    pch        <- ifelse(seq_len(length(mean_score)) %in% count, 19, 21)
    plot_data  <- data.frame(x = seq_len(length(mean_score)),
                             y = mean_score)
    iter       <- ggplot(plot_data, aes(x = x, y = y))
    col        <- ifelse(apts %in% cur_apts,
                         SomaPlotr::soma_colors2$blue,
                         "chocolate1")

    # geom_line produces error when only 1 data point present
    if ( count > 1 ) {
      iter <- iter + geom_line(aes(group = 1),
                               color = SomaPlotr::soma_colors2$blue)
    }

    # Create iteration dot/line plot
    iter <- iter +
      geom_point(color = SomaPlotr::soma_colors2$blue,
                 size = 2, shape = pch, fill = "white") +
      labs(title = sprintf("Iteration %i", count),
           x = "Iteration",
           y = "Mean Score") +
      SomaPlotr::theme_soma() +
      scale_y_continuous(labels = function(x) format(x, digits = 2)) +
      theme(plot.title   = element_text(hjust = 0.5, size = rel(0.7),
                                        face = "bold"),
            panel.grid   = element_blank(),
            panel.border = element_rect(fill = NA),
            axis.text    = element_text(size = rel(0.5)),
            axis.text.y  = element_text(angle = 90, hjust = 0.5),
            axis.title   = element_text(size = rel(0.7)),
            axis.ticks.length = unit(0.1, "cm"))

    # Create 1st rotation plot (dims 1 & 2)
    rot1 <- plotRotation(curPCA, col = col, pt.cex = 1, lab.cex = rel(2)) +
      theme(plot.title   = element_text(hjust = 0.5, size = rel(0.7)),
            panel.grid   = element_blank(),
            panel.border = element_rect(fill = NA),
            axis.text    = element_text(size = rel(0.5)),
            axis.text.y  = element_text(angle = 90, hjust = 0.5),
            axis.title   = element_text(size = rel(0.7)),
            axis.ticks.length = unit(0.1, "cm"))

    # Annotate apts on 1st plot
    for ( apt in add_apts ) {
      rot1 <- rot1 +
        geom_point(data = as.data.frame(curPCA$rotation)[apt, ],
                   aes(x = PC1, y = PC2), shape = 13,
                   size = 3, col = "green")
    }
    for ( apt in rem_apts ) {
      rot1 <- rot1 +
        geom_point(data = as.data.frame(curPCA$rotation)[apt, ],
                   aes(x = PC1, y = PC2), shape = 13,
                   size = 3, col = "red")
    }

    # Create 2nd rotation plot (dims 2 & 3)
    rot2 <- plotRotation(curPCA, col = col, dims = c(2, 3),
                         pt.cex = 1, lab.cex = rel(2)) +
      scale_y_continuous(labels = function(x) format(x, digits = 1)) +
      theme(plot.title   = element_text(hjust = 0.5, size = rel(0.7)),
            panel.grid   = element_blank(),
            panel.border = element_rect(fill = NA),
            axis.text    = element_text(size = rel(0.5)),
            axis.text.y  = element_text(angle = 90, hjust = 0.5),
            axis.title   = element_text(size = rel(0.7)),
            axis.ticks.length = unit(0.1, "cm"))

    # Annotate apts on 2nd plot
    for ( apt in add_apts ) {
      rot2 <- rot2 +
        geom_point(data = as.data.frame(curPCA$rotation)[apt, ],
                   aes(x = PC2, y = PC3), shape = 13,
                   size = 3, col = "green")
    }
    for ( apt in rem_apts ) {
      rot2 <- rot2 +
        geom_point(data = as.data.frame(curPCA$rotation)[apt, ],
                   aes(x = PC2, y = PC3), shape = 13,
                   size = 3, col = "red")
    }

    if ( verbose ) {
      signal_todo("Iter", count)
      signal_info("Removed ...", value(rem_apts))
      signal_info("Added .....", value(add_apts))
    }

    cur_apts <- c(add_apts, setdiff(cur_apts, rem_apts))
    count    <- count + 1L
    cur_iter <- c(list(iter), list(rot1), list(rot2)) |>
      globalr::set_Names(c(paste0(c("iter_", "rot1_", "rot2_"), count)))
    # Add this iteration's plots to final list
    plot_list <- c(plot_list, cur_iter)
  }

  # Reorder plots by iteration so they're rendered in order
  plot_list <- plot_list[order(names(plot_list))]

  grid.arrange(grobs   = plot_list,
               ncol    = max.iter,
               nrow    = 3,
               padding = 0,
               widths  = unit(rep(2, max.iter), "in"),
               heights = unit(rep(2, 3), "in")) |>
    invisible()

  list(apts = cur_apts, mu_scores = mean_score)
}



#' Calculate Objective Function Scores
#' .internal to pcaPurify()
#' @param x A prcomp object.
#' @noRd
.calcScore <- function(x) {
  load1 <- x$rotation[, "PC1"]
  load2 <- x$rotation[, "PC2"]
  # rel magnitude of PC1/PC2 ratio; log -> scale -ve numbers; hi = good
  PC1purity <- log(abs(load1 / load2))
  dist <- apply(x$rotation[, 2:3L], 1, function(.x) { # dist from origin (lo = good)
    dist(rbind(c(0, 0), .x), method = "euclidean")
  })
  Score <- PC1purity + 1 / dist           # simple score; hi = good
  cbind(PC1         = load1,
        PC2         = load2,
        PC1purity   = PC1purity,
        PC23_origin = dist,
        Score       = Score)
}
