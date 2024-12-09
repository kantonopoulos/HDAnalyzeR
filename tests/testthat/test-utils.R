# Test hd_initialize -----------------------------------------------------------
test_that("hd_initialize works correctly", {
  # Create a mock dataset for testing
  mock_data <- tibble::tibble(
    DAid = c("Sample1", "Sample2", "Sample3"),
    Assay = c("A", "B", "C"),
    NPX = c(1.2, 3.4, 5.6)
  )

  # Create a mock metadata for testing
  mock_metadata <- tibble::tibble(
    DAid = c("Sample1", "Sample2", "Sample3"),
    Group = c("Control", "Treatment", "Control")
  )

  # Test case 1: Initialize with basic arguments
  hd_object <- hd_initialize(mock_data)
  expect_s3_class(hd_object, "HDAnalyzeR")
  expect_true("data" %in% names(hd_object))
  expect_true("metadata" %in% names(hd_object))
  expect_true("sample_id" %in% names(hd_object))
  expect_true("var_name" %in% names(hd_object))
  expect_true("value_name" %in% names(hd_object))

  # Test case 2: Initialize with metadata and is_wide = FALSE
  hd_object_with_metadata <- hd_initialize(mock_data, metadata = mock_metadata)
  expect_s3_class(hd_object_with_metadata, "HDAnalyzeR")
  expect_equal(hd_object_with_metadata$metadata, mock_metadata)

  # Test case 3: Initialize with wide data format (is_wide = TRUE)
  mock_data_wide <- tibble::tibble(
    DAid = c("Sample1", "Sample2", "Sample3"),
    A = c(1.2, 3.4, 5.6),
    B = c(2.1, 4.3, 6.5),
    C = c(3.3, 4.4, 5.5)
  )
  hd_object_wide <- hd_initialize(mock_data_wide, is_wide = TRUE)
  expect_s3_class(hd_object_wide, "HDAnalyzeR")
  expect_equal(hd_object_wide$data, mock_data_wide)

  # Test case 4: Check for missing sample_id in data
  expect_error(hd_initialize(mock_data, sample_id = "NonExistentSampleId"),
               "Sample ID column must exist in dat.")

  # Test case 5: Check for missing sample_id in metadata
  expect_error(hd_initialize(mock_data, metadata = mock_metadata, sample_id = "NonExistentSampleId"),
               "Sample ID column must exist in dat.")

  # Test case 6: Check for missing variable name in data (when is_wide = FALSE)
  mock_data_long <- tibble::tibble(
    DAid = c("Sample1", "Sample2", "Sample3"),
    Assay = c("A", "B", "C"),
    NPX = c(1.2, 3.4, 5.6)
  )
  expect_error(hd_initialize(mock_data_long, is_wide = FALSE, var_name = "NonExistentVar"),
               "Variable name column must exist in dat.")

  # Test case 7: Check for missing value name in data (when is_wide = FALSE)
  expect_error(hd_initialize(mock_data_long, is_wide = FALSE, value_name = "NonExistentValue"),
               "Value column must exist in dat.")


  # Test case 8: Check for non numeric data (when is_wide = FALSE)
  dat <- data.frame(DAid = c("A", "B", "C"), Assay = c("M", "F", "M"), NPX = c(5, "Y", -2))
  expect_warning(hd_initialize(dat, is_wide = FALSE), "The following columns are not numeric: F")
})


# Test hd_save_path ------------------------------------------------------------
test_that("Directory creation without date", {
  dir_name <- "test_directory_without_date"
  hd_save_path(dir_name)
  expect_true(dir.exists(dir_name), "Directory should be created")
  unlink(dir_name, recursive = TRUE)
})

test_that("Directory creation with date", {
  dir_prefix <- "test_directory_with_date"
  result <- hd_save_path(dir_prefix, date = T)

  # List all directories that match the prefix
  expected <- file.path(dir_prefix, format(Sys.Date(), "%Y_%m_%d"))

  # Check that at least one directory with the expected prefix and date format exists
  expect_true(dir.exists(expected), "Directory with date should be created")

  unlink(dir_prefix, recursive = TRUE)
})

test_that("Handling existing directory", {
  dir_name <- "existing_directory"
  hd_save_path(dir_name)
  hd_save_path(dir_name)  # Attempt to create again
  expect_true(dir.exists(dir_name), "Existing directory should still exist")
  unlink(dir_name, recursive = TRUE)
})


# Test hd_save_data ------------------------------------------------------------
test_that("hd_save_data creates directory and saves CSV", {
  df <- data.frame(x = 1:10, y = rnorm(10))
  dir_name <- "test_csv_dir"
  file_name <- "test_csv_dir/test_csv_file.csv"

  if (dir.exists(dir_name)) {
    unlink(dir_name, recursive = TRUE)
  }

  hd_save_data(df, file_name)
  expect_true(file.exists(file_name))
  unlink(dir_name, recursive = TRUE)
})

test_that("hd_save_data saves TSV", {
  df <- data.frame(x = 1:10, y = rnorm(10))
  dir_name <- "test_tsv_dir"
  file_name <- "test_tsv_dir/test_tsv_file.tsv"

  if (dir.exists(dir_name)) {
    unlink(dir_name, recursive = TRUE)
  }

  hd_save_data(df, file_name)
  expect_true(file.exists(file_name))
  unlink(dir_name, recursive = TRUE)
})

test_that("hd_save_data saves RDS", {
  df <- data.frame(x = 1:10, y = rnorm(10))
  dir_name <- "test_rda_dir"
  file_name <- "test_rda_dir/test_rda_file.rds"

  if (dir.exists(dir_name)) {
    unlink(dir_name, recursive = TRUE)
  }

  hd_save_data(df, file_name)
  expect_true(file.exists(file_name))
  unlink(dir_name, recursive = TRUE)
})

test_that("hd_save_data saves XLSX", {
  df <- data.frame(x = 1:10, y = rnorm(10))
  dir_name <- "test_xlsx_dir"
  file_name <- "test_xlsx_dir/test_xlsx_file.xlsx"

  if (dir.exists(dir_name)) {
    unlink(dir_name, recursive = TRUE)
  }

  hd_save_data(df, file_name)
  expect_true(file.exists(file_name))
  unlink(dir_name, recursive = TRUE)
})

test_that("hd_save_data works with current directory", {
  df <- data.frame(x = 1:10, y = rnorm(10))
  file_name <- "test_current_dir.csv"

  hd_save_data(df, file_name)
  expect_true(file.exists(file_name))
  unlink(file_name)
})

test_that("hd_save_data works with existing directory", {
  df <- data.frame(x = 1:10, y = rnorm(10))
  dir_name <- "existing_dir"
  file_name <- "existing_dir/test_existing_file.csv"

  if (!dir.exists(dir_name)) {
    dir.create(dir_name)
  }
  hd_save_path(dir_name)
  hd_save_data(df, file_name)
  expect_true(file.exists(file_name))
  unlink(dir_name, recursive = TRUE)
})

test_that("hd_save_data handles invalid file type", {
  df <- data.frame(x = 1:10, y = rnorm(10))
  dir_name <- "test_invalid_dir"
  file_name <- "test_invalid_dir/test_invalid_file.invalid"

  if (dir.exists(dir_name)) {
    unlink(dir_name, recursive = TRUE)
  }

  expected_error_message <- paste("Unsupported file type: invalid")
  expect_error(hd_save_data(df, file_name), expected_error_message)
  expect_false(dir.exists(dir_name))
})

test_that("hd_save_data saves R objects", {
  obj <- list(x = 1:10, y = rnorm(10))
  dir_name <- "test_rds_dir"
  file_name <- "test_rds_dir/test_rds_file.rds"

  if (dir.exists(dir_name)) {
    unlink(dir_name, recursive = TRUE)
  }

  hd_save_data(obj, file_name)
  expect_true(file.exists(file_name))
  unlink(dir_name, recursive = TRUE)
})


# Test hd_import_data ----------------------------------------------------------
test_that("hd_import_data handles CSV files", {
  df_out <- data.frame(x = 1:10, y = rnorm(10))
  file_name <- "test_file.csv"
  utils::write.csv(df_out, file_name, row.names = FALSE)
  expect_true(file.exists(file_name))

  if (file.exists(file_name)) {
    df_in <- hd_import_data(file_name)
    expect_true(tibble::is_tibble(df_in))
  } else {
    skip("Test skipped: File not found.")
  }

  unlink(file_name, recursive = TRUE)
})


test_that("hd_import_data handles TSV files", {
  df_out <- data.frame(x = 1:10, y = rnorm(10))
  file_name <- "test_file.tsv"
  utils::write.table(df_out, file_name, sep='\t', row.names = FALSE, col.names = TRUE)
  expect_true(file.exists(file_name))

  if (file.exists(file_name)) {
    df_in <- hd_import_data(file_name)
    expect_true(tibble::is_tibble(df_in))
  } else {
    skip("Test skipped: File not found.")
  }

  unlink(file_name, recursive = TRUE)
})


test_that("hd_import_data handles TXT files", {
  df_out <- data.frame(x = 1:10, y = rnorm(10))
  file_name <- "test_file.txt"
  utils::write.table(df_out, file_name, row.names = FALSE, col.names = TRUE)
  expect_true(file.exists(file_name))

  if (file.exists(file_name)) {
    df_in <- hd_import_data(file_name)
    expect_true(tibble::is_tibble(df_in))
  } else {
    skip("Test skipped: File not found.")
  }

  unlink(file_name, recursive = TRUE)
})


test_that("hd_import_data handles RDS files", {
  df_out <- data.frame(x = 1:10, y = rnorm(10))
  file_name <- "test_file.rds"
  saveRDS(df_out, file = file_name)
  expect_true(file.exists(file_name))

  if (file.exists(file_name)) {
    df_in <- hd_import_data(file_name)
    expect_true(tibble::is_tibble(df_in))
  } else {
    skip("Test skipped: File not found.")
  }

  unlink(file_name, recursive = TRUE)
})


test_that("hd_import_data handles RDA files", {
  df_out <- data.frame(x = 1:10, y = rnorm(10))
  file_name <- "test_file.rda"
  save(df_out, file = file_name)
  expect_true(file.exists(file_name))

  if (file.exists(file_name)) {
    df_in <- hd_import_data(file_name)
    expect_true(tibble::is_tibble(df_in))
  } else {
    skip("Test skipped: File not found.")
  }

  unlink(file_name, recursive = TRUE)
})


test_that("hd_import_data handles XLSX files", {
  df_out <- data.frame(x = 1:10, y = rnorm(10))
  file_name <- "test_file.xlsx"
  writexl::write_xlsx(df_out, file_name)
  expect_true(file.exists(file_name))

  if (file.exists(file_name)) {
    df_in <- hd_import_data(file_name)
    expect_true(tibble::is_tibble(df_in))
  } else {
    skip("Test skipped: File not found.")
  }

  unlink(file_name, recursive = TRUE)
})


test_that("hd_import_data handles Parquet files", {
  file_name <- "../testdata/test_parquet.parquet"
  expect_true(file.exists(file_name))

  if (file.exists(file_name)) {
    df_in <- hd_import_data(file_name)
    expect_true(tibble::is_tibble(df_in))
  } else {
    skip("Test skipped: File not found.")
  }
})


# Test hd_widen_data -----------------------------------------------------------
test_that("hd_widen_data widens data properly", {
  result <- hd_widen_data(example_data)
  expected <- example_data |>
    dplyr::select(DAid, Assay, NPX) |>
    tidyr::pivot_wider(names_from = Assay, values_from = NPX)
  expect_equal(result, expected)
})

test_that("hd_widen_data works with non default values", {
  result <- hd_widen_data(example_data, exclude = c("DAid", "Sample"), names_from = "OlinkID")
  expected <- example_data |>
    dplyr::select(DAid, Sample, OlinkID, NPX) |>
    tidyr::pivot_wider(names_from = OlinkID, values_from = NPX)
  expect_equal(result, expected)
})


# Test hd_long_data ------------------------------------------------------------
test_that("hd_long_data lengthens data properly", {
  example_data_wide <- hd_widen_data(example_data, exclude = c("DAid", "Sample"), names_from = "OlinkID")
  result <- hd_long_data(example_data_wide, exclude = c("DAid", "Sample"), names_to = "OlinkID")
  expected <- example_data_wide |>
    tidyr::pivot_longer(cols = -c("DAid", "Sample"), names_to = "OlinkID", values_to = "NPX")
  expect_equal(result, expected)
})

test_that("hd_long_data works with non default values", {
  example_data_wide <- hd_widen_data(example_data)
  result <- hd_long_data(example_data_wide)
  expected <- example_data_wide |>
    tidyr::pivot_longer(cols = -DAid, names_to = "Assay", values_to = "NPX")
  expect_equal(result, expected)
})

# Test hd_detect_vartype -------------------------------------------------------
test_that("hd_detect_vartype detects categorical data", {
  category <- c("A", "B", "A", "C")
  result <- hd_detect_vartype(category)
  expected <- "categorical"
  expect_equal(result, expected)
})

test_that("hd_detect_vartype detects continuous data", {
  continuous <- c(1, 2, 3, 4, 5, 6)
  result <- hd_detect_vartype(continuous)
  expected <- "continuous"
  expect_equal(result, expected)
})

test_that("hd_detect_vartype detects continuous data with less categories than the threshold", {
  continuous <- c(1, 2, 3, 4)
  result <- hd_detect_vartype(continuous)
  expected <- "categorical"
  expect_equal(result, expected)
})

test_that("hd_detect_vartype detects continuous data with equal categories with the threshold", {
  continuous <- c(1, 2, 3, 4, 5)
  result <- hd_detect_vartype(continuous)
  expected <- "categorical"
  expect_equal(result, expected)
})

test_that("hd_detect_vartype detects mixed data", {
  mixed <- c(1, "1", 2, 2, "3", 3)
  result <- hd_detect_vartype(mixed)
  expected <- "categorical"
  expect_equal(result, expected)
})

test_that("hd_detect_vartype works with dataframes", {
  example <- data.frame(Category = c("A", "B", "A", "C", "B", "A"),
                        Continuous = c(1.1, 2.5, 3.8, 4.0, 5.8, 9),
                        Mixed = c(1, "1", 2, 2, "3", 3))
  result <- sapply(example, hd_detect_vartype)
  expected <- c(Category = "categorical", Continuous = "continuous", Mixed = "categorical")
  expect_equal(result, expected)
})

test_that("hd_detect_vartype handles unknown data", {
  unknown <- as.Date(c("2021-01-01", "2021-01-02", "2021-01-03"))
  result <- hd_detect_vartype(unknown)
  expected <- "unknown"
  expect_equal(result, expected)
})


# Test check_numeric_columns ---------------------------------------------------
test_that("All numeric columns", {
  dat <- data.frame(ID = c("A", "B", "C"), Age = c(25, 30, 35), Weight = c(60, 75, 80))
  expect_warning(non_numeric <- check_numeric_columns(dat), NA)
  expect_equal(non_numeric, character(0)) # No non-numeric columns
})

test_that("Some non-numeric columns", {
  dat <- data.frame(ID = c("A", "B", "C"), Age = c(25, NA, 35), Height = c("5.6", "D", "6.1"))
  expect_warning(non_numeric <- check_numeric_columns(dat), "The following columns are not numeric: Height")
  expect_equal(non_numeric, "Height")
})

test_that("All non-numeric columns", {
  dat <- data.frame(ID = c("A", "B", "C"), Gender = c("M", "F", "M"), Group = c("X", "Y", "Z"))
  expect_warning(non_numeric <- check_numeric_columns(dat), "The following columns are not numeric: Gender, Group")
  expect_equal(non_numeric, c("Gender", "Group"))
})

test_that("Input is not a data frame", {
  dat <- matrix(c(1, 2, 3, 4), ncol = 2)
  expect_error(check_numeric_columns(dat), "Input must be a data frame or tibble.")
})

test_that("Empty data frame", {
  dat <- data.frame()
  expect_warning(non_numeric <- check_numeric_columns(dat), NA) # No warning
  expect_equal(non_numeric, character(0)) # No non-numeric columns
})

test_that("Numeric-like character columns are handled correctly", {
  dat <- data.frame(
    "DAid" = c("A", "B", "C"),
    "M" = c("5", NA, "-2"),  # Can be coerced to numeric
    "F" = c(NA, "Y", NA)     # Cannot be coerced to numeric
  )
  expect_warning(non_numeric <- check_numeric_columns(dat),
                 "The following columns are not numeric: F")
  expect_equal(non_numeric, "F") # Only F should be flagged
})


# Test hd_filter_by_sex --------------------------------------------------------
test_that("hd_filter_by_sex filters data by sex correctly", {
  test_data <- data.frame(
    sample_id = c("S1", "S2", "S3", "S4", "S5"),
    Protein1 = c(1.5, 2.3, 0.7, 4.1, 2.5),
    Protein2 = c(0.8, 2.5, 3.1, 0.3, 4.0)
  )

  test_metadata <- data.frame(
    sample_id = c("S1", "S2", "S3", "S4", "S5"),
    Sex = c("M", "F", "M", "F", "M")
  )

  hd_object <- hd_initialize(test_data, test_metadata, sample_id = "sample_id", is_wide = TRUE)

  # Test filtering by male
  filtered_data_male <- hd_filter_by_sex(hd_object, variable = "Sex", sex = "M")
  expect_equal(nrow(filtered_data_male$data), 3)  # Should only include male samples
  expect_true(all(filtered_data_male$metadata$Sex == "M"))

  # Test filtering by female
  filtered_data_female <- hd_filter_by_sex(hd_object, variable = "Sex", sex = "F")
  expect_equal(nrow(filtered_data_female$data), 2)  # Should only include female samples
  expect_true(all(filtered_data_female$metadata$Sex == "F"))

  # Test invalid sex input
  expect_error(hd_filter_by_sex(hd_object, variable = "Sex", sex = "InvalidSex"),
               "InvalidSex is not a valid value for the Sex variable.")

  # Test missing metadata
  no_metadata_object <- hd_object
  no_metadata_object$metadata <- NULL
  expect_error(hd_filter_by_sex(no_metadata_object, metadata = NULL, variable = "Sex", sex = "M"),
               "The 'metadata' argument or slot of the HDAnalyzeR object is empty. Please provide the metadata.")

  # Test non-existent variable in metadata
  expect_error(hd_filter_by_sex(hd_object, variable = "NonExistentVariable", sex = "M"),
               "The variable NonExistentVariable does not exist in the metadata.")
})


# Test hd_log_transform --------------------------------------------------------
test_that("hd_log_transform log transforms data correctly", {

  test_data <- data.frame(
    sample_id = c("S1", "S2", "S3", "S4", "S5"),
    Protein1 = c(1.5, 2.3, 0.7, 4.1, 2.5),
    Protein2 = c(0.8, 2.5, 3.1, 0.3, 4.0)
  )

  test_metadata <- data.frame(
    sample_id = c("S1", "S2", "S3", "S4", "S5"),
    Sex = c("M", "F", "M", "F", "M")
  )

  hd_object <- hd_initialize(test_data, test_metadata, sample_id = "sample_id", is_wide = TRUE)

  # Test log transformation with positive values
  transformed_data <- hd_log_transform(hd_object)
  expect_true(all(!is.na(transformed_data$data[,-1])))  # Should not have NA values except for zero/negative entries

  # Test handling of non-positive values (<= 0) replaced by NA
  data_with_zeros <- test_data
  data_with_zeros$Protein1[3] <- 0  # Add a zero value
  data_with_zeros$Protein2[4] <- -1  # Add a negative value
  transformed_data_with_zeros <- hd_log_transform(hd_initialize(data_with_zeros, test_metadata, sample_id = "sample_id", is_wide = TRUE))
  expect_true(all(is.na(transformed_data_with_zeros$data$Protein1[3])))  # Zero should be NA after log transform
  expect_true(all(is.na(transformed_data_with_zeros$data$Protein2[4])))  # Negative should be NA after log transform
})
