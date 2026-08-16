de_table <- function() {
  tibble::tibble(
    Feature = c("TP53", "EGFR", "BRCA1", "MYC"),
    logFC = c(2, -3, 1, 0.5),
    adj.P.Val = c(0.001, 0.01, 0.2, 0.5)
  )
}

# rank_features ---------------------------------------------------------------

test_that("rank_features() ranks by logFC in decreasing order", {
  ranked <- rank_features(de_table(), "logFC")

  expect_equal(names(ranked), c("TP53", "BRCA1", "MYC", "EGFR"))
  expect_equal(unname(ranked), c(2, 1, 0.5, -3))
})

test_that("rank_features() combines fold change and significance for 'both'", {
  ranked <- rank_features(de_table(), "both")
  expected <- de_table()$logFC * -log(de_table()$adj.P.Val)

  expect_equal(sort(unname(ranked), decreasing = TRUE), sort(expected, decreasing = TRUE))
  # the strongly up-regulated and highly significant gene must come first
  expect_equal(names(ranked)[1], "TP53")
  # and the strongly down-regulated one last
  expect_equal(names(ranked)[length(ranked)], "EGFR")
})

test_that("rank_features() for 'both' does not fall back to the p-value", {
  ranked <- rank_features(de_table(), "both")
  p_values <- sort(de_table()$adj.P.Val, decreasing = TRUE)

  expect_false(isTRUE(all.equal(unname(ranked), p_values)))
})

test_that("rank_features() accepts any other numeric column", {
  dat <- de_table() |> dplyr::mutate(custom = c(4, 3, 2, 1))
  expect_message(ranked <- rank_features(dat, "custom"), "based on the custom variable")

  expect_equal(names(ranked), c("TP53", "EGFR", "BRCA1", "MYC"))
  expect_equal(unname(ranked), c(4, 3, 2, 1))
})

test_that("rank_features() rejects an unknown ranking column", {
  expect_error(rank_features(de_table(), "nope"), "not valid")
})


# rename_ranking_to_entrezid --------------------------------------------------

test_that("rename_ranking_to_entrezid() keeps each score with its own gene", {
  skip_if_not_installed("clusterProfiler")
  skip_if_not_installed("org.Hs.eg.db")

  ranked <- c(TP53 = 5, EGFR = 4, NOTAREALGENE = 3, MYC = 2)
  mapped <- quietly(rename_ranking_to_entrezid(ranked))

  # the unmappable symbol is dropped rather than shifting every other score
  expect_false(anyNA(names(mapped)))
  expect_lte(length(mapped), 3)

  symbols <- quietly(clusterProfiler::bitr(
    c("TP53", "EGFR", "MYC"),
    fromType = "SYMBOL", toType = "ENTREZID",
    OrgDb = org.Hs.eg.db::org.Hs.eg.db
  ))
  tp53_id <- symbols$ENTREZID[symbols$SYMBOL == "TP53"]
  myc_id <- symbols$ENTREZID[symbols$SYMBOL == "MYC"]

  expect_equal(unname(mapped[tp53_id]), 5)
  expect_equal(unname(mapped[myc_id]), 2)
})

test_that("rename_ranking_to_entrezid() returns a decreasing vector", {
  skip_if_not_installed("clusterProfiler")
  skip_if_not_installed("org.Hs.eg.db")

  mapped <- quietly(rename_ranking_to_entrezid(c(TP53 = 1, EGFR = 5, MYC = 3)))
  expect_false(is.unsorted(rev(unname(mapped))))
})


# hd_show_backgrounds / select_background -------------------------------------

test_that("hd_show_backgrounds() lists the available backgrounds", {
  expect_message(hd_show_backgrounds(), "olink")
})

test_that("select_background() returns a gene vector for a known list", {
  backgrounds <- background_lists()
  name <- names(backgrounds)[1]

  expect_type(select_background(name), "character")
  expect_gt(length(select_background(name)), 0)
})

test_that("select_background() warns and returns NULL for an unknown list", {
  expect_warning(res <- select_background("nope"), "not found in available backgrounds")
  expect_null(res)
})

test_that("select_background() passes a custom vector straight through", {
  genes <- c("TP53", "EGFR")
  expect_equal(select_background(genes), genes)
})


# hd_ora ----------------------------------------------------------------------

test_that("hd_ora() runs an over-representation analysis", {
  skip_on_cran()
  skip_if_not_installed("clusterProfiler")
  skip_if_not_installed("org.Hs.eg.db")

  # a coherent set of cell-cycle genes so that something is enriched
  genes <- c(
    "CDK1", "CDK2", "CCNB1", "CCNA2", "CDC20", "PLK1", "AURKA", "AURKB",
    "BUB1", "MAD2L1", "CCNE1", "CDC25A", "CHEK1", "MCM2", "MCM3"
  )
  enrichment <- quietly(hd_ora(genes, database = "GO", ontology = "BP"))

  expect_s3_class(enrichment, "hd_enrichment")
  expect_true("enrichment" %in% names(enrichment))
  expect_gt(nrow(enrichment$enrichment@result), 0)
})

test_that("hd_ora() errors clearly when nothing is enriched", {
  skip_on_cran()
  skip_if_not_installed("clusterProfiler")
  skip_if_not_installed("org.Hs.eg.db")

  expect_error(
    quietly(hd_ora(c("TP53", "EGFR"), database = "GO", ontology = "BP", pval_lim = 1e-12)),
    "No significant terms found"
  )
})

test_that("hd_ora() validates the database and ontology arguments", {
  expect_error(hd_ora(c("TP53"), database = "nope"), "should be one of")
  expect_error(hd_ora(c("TP53"), database = "GO", ontology = "nope"), "should be one of")
})
