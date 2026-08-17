# hd_initialize --------------------------------------------------------------

test_that("hd_initialize() pivots long data to wide", {
  hd_obj <- hd_initialize(tiny_long(), tiny_meta())

  expect_s3_class(hd_obj, "HDAnalyzeR")
  expect_equal(hd_obj$data, tiny_wide())
  expect_equal(hd_obj$metadata, tiny_meta())
  expect_equal(hd_obj$sample_id, "DAid")
  expect_equal(hd_obj$var_name, "Assay")
  expect_equal(hd_obj$value_name, "NPX")
})

test_that("hd_initialize() keeps wide data untouched", {
  expect_equal(hd_initialize(tiny_wide(), is_wide = TRUE)$data, tiny_wide())
})

test_that("hd_initialize() honours non-default column names", {
  long <- tiny_long() |>
    dplyr::rename(sample = "DAid", feature = "Assay", intensity = "NPX")

  hd_obj <- hd_initialize(
    long,
    sample_id = "sample",
    var_name = "feature",
    value_name = "intensity"
  )

  expect_equal(colnames(hd_obj$data), c("sample", "f1", "f2", "f3", "f4"))
  expect_equal(hd_obj$sample_id, "sample")
})

test_that("hd_initialize() rejects malformed input", {
  expect_error(hd_initialize("not a data frame"), "dat must be a data frame")
  expect_error(
    hd_initialize(tiny_long(), metadata = "nope"),
    "metadata must be a data frame"
  )
  expect_error(
    hd_initialize(tiny_long(), sample_id = "missing"),
    "Sample ID column must exist in dat"
  )
  expect_error(
    hd_initialize(tiny_long(), metadata = tiny_meta() |> dplyr::select(-"DAid")),
    "Sample ID column must exist in metadata"
  )
  expect_error(
    hd_initialize(tiny_long(), var_name = "missing"),
    "Variable name column must exist in dat"
  )
  expect_error(
    hd_initialize(tiny_long(), value_name = "missing"),
    "Value column must exist in dat"
  )
})

test_that("hd_initialize() warns about non-numeric feature columns", {
  bad <- tiny_wide() |> dplyr::mutate(f1 = as.character(c("a", "b", "c", "d", "e", "f")))
  expect_warning(
    hd_initialize(bad, is_wide = TRUE),
    "The following columns are not numeric: f1"
  )
})


# hd_widen_data / hd_long_data ------------------------------------------------

test_that("widening and lengthening round-trips", {
  wide <- hd_widen_data(tiny_long())
  expect_equal(wide, tiny_wide())

  back <- hd_long_data(wide)
  expect_equal(
    back |> dplyr::arrange(.data$DAid, .data$Assay),
    tiny_long() |> dplyr::arrange(.data$DAid, .data$Assay)
  )
})

test_that("hd_long_data() can exclude several columns", {
  wide <- tiny_wide() |> dplyr::mutate(batch = "b1")
  long <- hd_long_data(wide, exclude = c("DAid", "batch"))

  expect_setequal(colnames(long), c("DAid", "batch", "Assay", "NPX"))
  expect_equal(nrow(long), nrow(wide) * 4)
})


# hd_detect_vartype -----------------------------------------------------------

test_that("hd_detect_vartype() classifies by type and cardinality", {
  expect_equal(hd_detect_vartype(c("a", "b", "a")), "categorical")
  expect_equal(hd_detect_vartype(factor(c("a", "b"))), "categorical")
  expect_equal(hd_detect_vartype(seq_len(6)), "continuous")
  expect_equal(hd_detect_vartype(c(1, 1, 2, 2)), "categorical")
  expect_equal(hd_detect_vartype(as.Date("2020-01-01")), "unknown")
})

test_that("hd_detect_vartype() respects unique_threshold", {
  x <- seq_len(6)
  expect_equal(hd_detect_vartype(x, unique_threshold = 5), "continuous")
  expect_equal(hd_detect_vartype(x, unique_threshold = 6), "categorical")
})


# hd_bin_columns --------------------------------------------------------------

test_that("hd_bin_columns() bins only the continuous columns", {
  dat <- data.frame(age = c(0, 25, 50, 75, 100), sex = c("M", "F", "M", "F", "M"))
  binned <- hd_bin_columns(dat, c("continuous", "categorical"), bins = 4)

  expect_s3_class(binned$age, "factor")
  expect_equal(levels(binned$age), c("0-25", "25-50", "50-75", "75-100"))
  expect_equal(as.character(binned$age), c("0-25", "0-25", "25-50", "50-75", "75-100"))
  expect_equal(binned$sex, dat$sex)
})

test_that("hd_bin_columns() validates its arguments", {
  dat <- data.frame(a = 1:5, b = 1:5)
  expect_error(hd_bin_columns("nope", c("continuous")), "data must be a dataframe")
  expect_error(hd_bin_columns(dat, "continuous"), "column_types length must match")
  expect_error(
    hd_bin_columns(dat, c("continuous", "nonsense")),
    "column_types must contain only"
  )
})


# hd_filter -------------------------------------------------------------------

test_that("hd_filter() subsets data and metadata together", {
  hd_obj <- hd_initialize(tiny_wide(), tiny_meta(), is_wide = TRUE)

  kept <- quietly(hd_filter(hd_obj, "Disease", "A", "k"))
  expect_equal(kept$metadata$DAid, c("S1", "S2", "S3"))
  expect_equal(kept$data$DAid, c("S1", "S2", "S3"))

  removed <- quietly(hd_filter(hd_obj, "Disease", "A", "r"))
  expect_equal(removed$metadata$DAid, c("S4", "S5", "S6"))
  expect_equal(removed$data$DAid, c("S4", "S5", "S6"))
})

test_that("hd_filter() supports every continuous operator", {
  hd_obj <- hd_initialize(tiny_wide(), tiny_meta(), is_wide = TRUE)
  ages <- function(flag, value) {
    quietly(hd_filter(hd_obj, "Age", value, flag))$metadata$Age
  }

  expect_equal(ages("=", 50), 50)
  expect_equal(ages("<", 50), c(30, 40))
  expect_equal(ages("<=", 50), c(30, 40, 50))
  expect_equal(ages(">", 50), c(60, 70, 80))
  expect_equal(ages(">=", 50), c(50, 60, 70, 80))
  expect_equal(ages("!=", 50), c(30, 40, 60, 70, 80))
})

test_that("hd_filter() can filter on a data column", {
  hd_obj <- hd_initialize(tiny_wide(), tiny_meta(), is_wide = TRUE)
  res <- quietly(hd_filter(hd_obj, "f1", 3, ">"))

  expect_equal(res$data$DAid, c("S4", "S5", "S6"))
  expect_equal(res$metadata$DAid, c("S4", "S5", "S6"))
})

test_that("hd_filter() drops NA rows rather than keeping them", {
  hd_obj <- hd_initialize(tiny_wide(), tiny_meta(), is_wide = TRUE)
  res <- quietly(hd_filter(hd_obj, "f2", 15, ">"))

  # S3 has NA in f2 and must not survive a numeric comparison
  expect_equal(res$data$DAid, c("S2", "S4", "S5", "S6"))
})

test_that("hd_filter() validates its arguments", {
  hd_obj <- hd_initialize(tiny_wide(), tiny_meta(), is_wide = TRUE)

  expect_error(hd_filter(list(), "Disease", "A", "k"), "Invalid HD object")
  expect_error(hd_filter(hd_obj, "nope", "A", "k"), "Variable not found")
  expect_error(quietly(hd_filter(hd_obj, "Disease", "A", ">")), "Invalid flag for categorical")
  expect_error(quietly(hd_filter(hd_obj, "Age", 50, "k")), "Invalid flag for continuous")
})

test_that("hd_filter() is silent when verbose = FALSE", {
  hd_obj <- hd_initialize(tiny_wide(), tiny_meta(), is_wide = TRUE)
  expect_no_message(hd_filter(hd_obj, "Disease", "A", "k", verbose = FALSE))
})

test_that("hd_filter() messages are readable", {
  hd_obj <- hd_initialize(tiny_wide(), tiny_meta(), is_wide = TRUE)
  expect_message(
    hd_filter(hd_obj, "Disease", "A", "k"),
    "Rows remaining: 3"
  )
})


# hd_log_transform ------------------------------------------------------------

test_that("hd_log_transform() applies log2 to the feature columns only", {
  dat <- tibble::tibble(DAid = c("S1", "S2"), a = c(1, 4), b = c(8, 16))
  res <- hd_log_transform(dat)

  expect_equal(res$DAid, c("S1", "S2"))
  expect_equal(res$a, c(0, 2))
  expect_equal(res$b, c(3, 4))
})

test_that("hd_log_transform() turns non-positive values into NA and warns", {
  dat <- tibble::tibble(DAid = c("S1", "S2", "S3"), a = c(4, 0, -2))
  expect_warning(res <- hd_log_transform(dat), "non-positive values")
  expect_equal(res$a, c(2, NA, NA))
})

test_that("hd_log_transform() round-trips through an HDAnalyzeR object", {
  hd_obj <- hd_initialize(
    tibble::tibble(DAid = c("S1", "S2"), a = c(1, 4)),
    is_wide = TRUE
  )
  res <- hd_log_transform(hd_obj)

  expect_s3_class(res, "HDAnalyzeR")
  expect_equal(res$data$a, c(0, 2))
})

test_that("hd_log_transform() rejects an empty HDAnalyzeR object", {
  hd_obj <- hd_initialize(tiny_wide(), is_wide = TRUE)
  hd_obj$data <- NULL
  expect_error(hd_log_transform(hd_obj), "'data' slot .* is empty")
})


# hd_save_path ----------------------------------------------------------------

test_that("hd_save_path() creates nested directories and returns the path", {
  withr::local_dir(withr::local_tempdir())

  path <- hd_save_path("outer/inner", date = FALSE)
  expect_equal(path, "outer/inner")
  expect_true(dir.exists("outer/inner"))
})

test_that("hd_save_path() appends the current date when asked", {
  withr::local_dir(withr::local_tempdir())

  path <- hd_save_path("results", date = TRUE)
  expect_equal(path, file.path("results", format(Sys.Date(), "%Y_%m_%d")))
  expect_true(dir.exists(path))
})

test_that("hd_save_path() reports an existing directory readably", {
  withr::local_dir(withr::local_tempdir())
  dir.create("existing")

  expect_message(hd_save_path("existing"), "Directory existing already exists")
})


# hd_save_data / hd_import_data ----------------------------------------------

test_that("csv, tsv and rds round-trip through save and import", {
  withr::local_dir(withr::local_tempdir())
  dat <- tibble::tibble(x = c(1, 2, 3), y = c(4, 5, 6))

  for (ext in c("csv", "tsv", "rds")) {
    path <- paste0("out/data.", ext)
    expect_match(hd_save_data(dat, path), "File saved as")
    expect_true(file.exists(path))
    expect_equal(quietly(hd_import_data(path)), dat)
  }
})

test_that("hd_save_data() rejects unsupported extensions", {
  withr::local_dir(withr::local_tempdir())
  expect_error(
    hd_save_data(tibble::tibble(x = 1), "out/data.json"),
    "Unsupported file type: json"
  )
})

test_that("hd_import_data() reads parquet", {
  skip_if_not_installed("arrow")
  expect_s3_class(
    hd_import_data(test_path("..", "testdata", "test_parquet.parquet")),
    "tbl_df"
  )
})

test_that("hd_import_data() reads an rda file back as its own object", {
  withr::local_dir(withr::local_tempdir())
  stored_object <- tibble::tibble(value = c(10, 20, 30))
  save(stored_object, file = "obj.rda")

  expect_equal(hd_import_data("obj.rda"), stored_object)
})

test_that("hd_import_data() rejects unsupported extensions", {
  expect_error(hd_import_data("nope.json"), "Unsupported file type: json")
})


# check_installed -------------------------------------------------------------

test_that("check_installed() passes for an installed package", {
  expect_true(check_installed("stats"))
})

test_that("check_installed() gives a repository-appropriate install hint", {
  expect_error(
    check_installed("definitelyNotAPackage"),
    'install.packages\\("definitelyNotAPackage"\\)'
  )
})

test_that("check_installed() points Bioconductor packages at BiocManager", {
  skip_if(requireNamespace("ReactomePA", quietly = TRUE))
  expect_error(check_installed("ReactomePA"), "BiocManager::install")
})

test_that("check_installed() explains what the package was needed for", {
  expect_error(
    check_installed("definitelyNotAPackage", "do something useful"),
    "required to do something useful"
  )
})

test_that("hd_filter() keeps and removes complementary sets of samples", {
  hd_obj <- hd_initialize(tiny_wide(), tiny_meta(), is_wide = TRUE)

  kept <- quietly(hd_filter(hd_obj, "Disease", "A", "k"))
  removed <- quietly(hd_filter(hd_obj, "Disease", "A", "r"))

  # "k" and "r" once returned the same rows because the companion component
  # was filtered with an inverted condition
  expect_false(identical(kept$data, removed$data))
  expect_false(identical(kept$metadata, removed$metadata))
  expect_equal(
    sort(c(kept$data$DAid, removed$data$DAid)),
    sort(hd_obj$data$DAid)
  )
  expect_length(intersect(kept$data$DAid, removed$data$DAid), 0)

  # the two components must stay aligned on the same samples
  expect_equal(kept$data$DAid, kept$metadata$DAid)
  expect_equal(removed$data$DAid, removed$metadata$DAid)
  expect_true(all(kept$metadata$Disease == "A"))
  expect_false(any(removed$metadata$Disease == "A"))
})
