
ad   <- get_col_meta(simdata)
set1 <- ad$SeqId[c(1, 3, 5)]
set2 <- ad$SeqId[c(17, 19, 21)]
map  <- map_plot_pch(get_analytes(simdata), set1, set2)

test_that("mapping works as expected", {
  true <- tibble::tibble(
    features = c("seq.2802.68", "seq.9251.29", "seq.1942.70", "seq.5751.80",
                 "seq.9608.12", "seq.3459.49", "seq.3865.56", "seq.3363.21",
                 "seq.4487.88", "seq.5994.84", "seq.9011.72", "seq.2902.23",
                 "seq.2260.48", "seq.4936.96", "seq.2277.95", "seq.2953.31",
                 "seq.3032.11", "seq.4330.4", "seq.4914.10", "seq.3896.5",
                 "seq.5002.7", "seq.3476.4", "seq.1130.49", "seq.6356.60",
                 "seq.4579.40", "seq.8344.24", "seq.8441.53", "seq.9360.55",
                 "seq.7841.8", "seq.8142.63", "seq.4461.56", "seq.9297.97",
                 "seq.9396.38", "seq.3300.26", "seq.2772.14", "seq.6615.18",
                 "seq.8797.98", "seq.9879.88", "seq.8993.16", "seq.9373.82"),
    ft_id = c("2802-68", "9251-29", "1942-70", "5751-80", "9608-12", "3459-49",
              "3865-56", "3363-21", "4487-88", "5994-84", "9011-72", "2902-23",
              "2260-48", "4936-96", "2277-95", "2953-31", "3032-11", "4330-4",
              "4914-10", "3896-5", "5002-7", "3476-4", "1130-49", "6356-60",
              "4579-40", "8344-24", "8441-53", "9360-55", "7841-8", "8142-63",
              "4461-56", "9297-97", "9396-38", "3300-26", "2772-14", "6615-18",
              "8797-98", "9879-88", "8993-16", "9373-82"),
    all = c("2802-68", NA, "1942-70", NA, "9608-12", NA, NA, NA, NA, NA, NA, NA,
            NA, NA, NA, NA, "3032-11", NA, "4914-10", NA, "5002-7", NA, NA, NA, NA,
            NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
    mask = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE,
             FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE,
             FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
             FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
    cex = c(4, 2.5, 4, 2.5, 4, 2.5, 2.5, 2.5, 2.5, 2.5, 2.5, 2.5, 2.5, 2.5, 2.5,
            2.5, 4, 2.5, 4, 2.5, 4, 2.5, 2.5, 2.5, 2.5, 2.5, 2.5, 2.5, 2.5, 2.5, 2.5,
            2.5, 2.5, 2.5, 2.5, 2.5, 2.5, 2.5, 2.5, 2.5),
    pch = c(23, 19, 23, 19, 23, 19, 19, 19, 19, 19, 19, 19,
            19, 19, 19, 19, 24, 19, 24, 19, 24, 19, 19, 19,
            19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19,
            19, 19, 19, 19)
  )
  expect_equal(map, true)
})
