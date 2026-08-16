# Shared fixtures ------------------------------------------------------------
#
# Small, fully deterministic datasets. Tests assert on exact values wherever the
# maths allows it, so the fixtures deliberately avoid randomness unless a test
# needs it (in which case it seeds locally).

#' A tiny wide dataset with a known structure.
#'
#' 6 samples, 4 numeric features, one NA in `f2`.
tiny_wide <- function() {
  tibble::tibble(
    DAid = paste0("S", 1:6),
    f1 = c(1, 2, 3, 4, 5, 6),
    f2 = c(10, 20, NA, 40, 50, 60),
    f3 = c(6, 5, 4, 3, 2, 1),
    f4 = c(2, 2, 2, 2, 2, 3)
  )
}

#' Metadata matching `tiny_wide()`.
tiny_meta <- function() {
  tibble::tibble(
    DAid = paste0("S", 1:6),
    Disease = c("A", "A", "A", "B", "B", "B"),
    Sex = c("F", "M", "F", "M", "F", "M"),
    Age = c(30, 40, 50, 60, 70, 80)
  )
}

#' The long-format equivalent of `tiny_wide()`.
tiny_long <- function() {
  tidyr::pivot_longer(
    tiny_wide(),
    cols = -"DAid",
    names_to = "Assay",
    values_to = "NPX"
  )
}

#' A moderately sized dataset with a genuine group difference, for tests that
#' need a differential-expression or modelling signal.
#'
#' `n_per_group` samples in each of two groups; `up` is shifted upwards in group
#' B, `down` downwards, and `flat` carries no signal at all.
signal_data <- function(n_per_group = 25, seed = 1) {
  withr::local_seed(seed)
  n <- n_per_group * 2
  group <- rep(c("ctrl", "case"), each = n_per_group)
  shift <- ifelse(group == "case", 1, 0)
  dat <- tibble::tibble(
    DAid = sprintf("S%03d", seq_len(n)),
    up = stats::rnorm(n) + 3 * shift,
    down = stats::rnorm(n) - 3 * shift,
    flat = stats::rnorm(n),
    flat2 = stats::rnorm(n),
    flat3 = stats::rnorm(n)
  )
  meta <- tibble::tibble(
    DAid = dat$DAid,
    Disease = group,
    Sex = rep(c("F", "M"), length.out = n),
    Age = seq(20, 80, length.out = n),
    Batch = rep(c("b1", "b2"), length.out = n)
  )
  list(data = dat, metadata = meta)
}

#' An `HDAnalyzeR` object built from `signal_data()`.
signal_object <- function(...) {
  sd <- signal_data(...)
  hd_initialize(sd$data, sd$metadata, is_wide = TRUE)
}

#' A subset of the shipped example data.
#'
#' The end-to-end tests exercise the real pipelines on the real example data, but
#' fitting a twelve-class penalised model over 100 assays takes minutes. Five
#' diseases and 40 assays keep every code path intact while staying fast enough to
#' run on every push.
#'
#' @param diseases Which disease groups to keep.
#' @param n_assays How many assays to keep.
example_subset <- function(diseases = c("AML", "CLL", "MYEL", "LUNGC", "GLIOM"),
                           n_assays = 40) {
  assays <- utils::head(sort(unique(example_data$Assay)), n_assays)
  metadata <- example_metadata |>
    dplyr::filter(.data$Disease %in% diseases)
  dat <- example_data |>
    dplyr::filter(
      .data$Assay %in% assays,
      .data$DAid %in% metadata$DAid
    )

  hd_initialize(dat, metadata)
}

#' Run an expression while suppressing the package's progress chatter.
quietly <- function(expr) {
  suppressMessages(suppressWarnings(expr))
}

#' TRUE when the object is a ggplot that can actually be rendered.
expect_renderable_ggplot <- function(p) {
  testthat::expect_s3_class(p, "ggplot")
  testthat::expect_no_error(ggplot2::ggplot_build(p))
}
