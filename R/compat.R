# Small internal replacements for single-function dependencies.
#
# Each helper below reproduces the one function the package used from an
# external package, so that package no longer has to be an import. They are
# deliberately minimal: they cover the arguments HDAnalyzeR actually passes,
# not the full surface of the original.

#' Reorder factor levels by a summary of another variable
#'
#' Internal stand-in for `forcats::fct_reorder()`.
#'
#' @param f A factor (or a vector coercible to one).
#' @param x A numeric vector used to order the levels of `f`.
#' @param .fun The summary applied to `x` within each level. Default is
#'   [stats::median()], matching `forcats::fct_reorder()`.
#'
#' @return `f` as a factor with reordered levels.
#' @noRd
fct_reorder <- function(f, x, .fun = stats::median) {
  f <- as.factor(f)
  scores <- tapply(x, f, .fun)
  # `order()` sorts NA scores last, which keeps empty levels rather than
  # dropping them the way `sort()` would.
  factor(f, levels = levels(f)[order(scores)])
}


#' Reorder a variable within a faceting group
#'
#' Internal stand-in for `tidytext::reorder_within()`. Appends the grouping
#' value to each label so levels can be ordered independently per facet; the
#' suffix is stripped again by `scale_x_reordered()` / `scale_y_reordered()`.
#'
#' @param x The vector to reorder.
#' @param by The numeric vector to order by.
#' @param within The grouping vector (or list of vectors).
#' @param fun The summary applied to `by` within each level. Default is `mean`.
#' @param sep The separator between the label and the grouping value.
#'
#' @return A factor with levels ordered within each group.
#' @noRd
reorder_within <- function(x, by, within, fun = mean, sep = "___") {
  if (!is.list(within)) {
    within <- list(within)
  }
  new_x <- do.call(paste, c(list(x, sep = sep), within))
  stats::reorder(new_x, by, FUN = fun)
}


#' Strip the grouping suffix added by `reorder_within()`
#'
#' @param x A character vector of axis labels.
#' @param sep The separator used by `reorder_within()`.
#'
#' @return `x` with the separator and everything after it removed.
#' @noRd
reorder_func <- function(x, sep = "___") {
  gsub(paste0(sep, ".+$"), "", x)
}


#' Discrete x scale that cleans labels made by `reorder_within()`
#'
#' Internal stand-in for `tidytext::scale_x_reordered()`.
#'
#' @param ... Passed on to [ggplot2::scale_x_discrete()].
#'
#' @return A ggplot2 scale.
#' @noRd
scale_x_reordered <- function(..., labels = reorder_func) {
  ggplot2::scale_x_discrete(labels = labels, ...)
}


#' Discrete y scale that cleans labels made by `reorder_within()`
#'
#' Internal stand-in for `tidytext::scale_y_reordered()`.
#'
#' @param ... Passed on to [ggplot2::scale_y_discrete()].
#'
#' @return A ggplot2 scale.
#' @noRd
scale_y_reordered <- function(..., labels = reorder_func) {
  ggplot2::scale_y_discrete(labels = labels, ...)
}


#' Evenly spaced hues around the colour wheel
#'
#' Internal stand-in for `scales::hue_pal()()`. Reproduces the ggplot2 default
#' discrete palette, which is `n` equally spaced hues at fixed chroma and
#' luminance.
#'
#' `scales` converts HCL to hex with `farver`, which rounds a channel
#' differently than [grDevices::hcl()] does for a handful of hues. The two
#' agree exactly up to `n = 15`; beyond that a single channel of a single
#' colour can differ by 1/255, which is not visible.
#'
#' @param n The number of colours to generate.
#'
#' @return A character vector of `n` hex colours.
#' @noRd
hue_pal <- function(n) {
  if (n < 1) {
    stop("Must request at least one colour from a hue palette.")
  }
  h <- c(0, 360) + 15
  # Drop the last hue when the range wraps the full circle, so the first and
  # last colours are not the same.
  if ((diff(h) %% 360) < 1) {
    h[2] <- h[2] - 360 / n
  }
  hues <- seq(h[1], h[2], length.out = n) %% 360
  grDevices::hcl(hues, c = 100, l = 65)
}


#' Read a delimited text file
#'
#' Internal stand-in for `readr::read_csv()` / `readr::read_tsv()`, built on
#' [utils::read.table()]. The non-default arguments all exist to match what
#' readr did:
#'
#' * `check.names = FALSE` keeps feature names such as `IL-6` intact, rather
#'   than mangling them to `IL.6`.
#' * `na.strings = c("", "NA")` treats blank fields as missing.
#' * `comment.char = ""` stops `#` in a value from truncating the line.
#' * integer columns are widened to double, because readr's type guessing
#'   never returned integer — so a whole number saved and re-imported keeps
#'   the double type it started with.
#'
#' @param path_name The path to the file to read.
#' @param sep The field separator.
#'
#' @return A data frame.
#' @noRd
read_delimited <- function(path_name, sep) {
  dat <- utils::read.table(
    path_name,
    header = TRUE,
    sep = sep,
    quote = "\"",
    na.strings = c("", "NA"),
    check.names = FALSE,
    comment.char = "",
    stringsAsFactors = FALSE,
    encoding = "UTF-8"
  )

  int_cols <- vapply(dat, is.integer, logical(1))
  dat[int_cols] <- lapply(dat[int_cols], as.double)

  dat
}


#' Wrap strings onto lines of a target width
#'
#' Internal stand-in for `stringr::str_wrap()`. Unlike [base::strwrap()] this
#' is vectorised over `string` and returns one element per input, with lines
#' joined by newlines.
#'
#' @param string A character vector.
#' @param width The target line width in characters.
#'
#' @return A character vector the same length as `string`.
#' @noRd
str_wrap <- function(string, width = 80) {
  vapply(
    string,
    function(s) {
      if (is.na(s)) {
        return(NA_character_)
      }
      paste(strwrap(s, width = width + 1), collapse = "\n")
    },
    character(1),
    USE.NAMES = FALSE
  )
}
