#' Initialize HDAnalyzeR object
#'
#' `hd_initialize()` initializes an HDAnalyzeR object with the data, metadata, and other parameters. This object can be used to run various analyses in the package.
#'
#' @param dat A tibble containing the omics data.
#' @param metadata A tibble containing the metadata. Default is NULL.
#' @param is_wide A logical indicating if the data is in wide format. Default is FALSE.
#' @param sample_id The name of the column containing the sample IDs. Default is "DAid".
#' @param var_name The name of the column containing the variable names. Default is "Assay".
#' @param value_name The name of the column containing the values. Default is "NPX".
#'
#' @return An HDAnalyzeR object.
#' @details
#' It is strongly recommended to use this function to initialize an HDAnalyzeR
#' object before starting any analysis. This initialization step ensures that
#' your input data and metadata are correctly formatted and compatible with the
#' package. Specifically, the function validates your data and automatically
#' converts it to wide format if provided in long format (`is_wide`).
#'
#' While it is possible to use HDAnalyzeR functions on data outside of an HDAnalyzeR object,
#' users must ensure that the data adheres to the required structure: the first column
#' must contain sample IDs, followed by numeric columns (protein expression) in wide format.
#' Additionally, the metadata must include the same set of sample IDs.
#' Proper initialization or careful adherence to these requirements is crucial
#' for accurate and efficient analysis. Defaults are provided according to the
#' Human Disease Blood Atlas Olink data format.
#'
#' @export
#'
#' @examples
#' # Initialize an HDAnalyzeR object
#' hd_initialize(example_data, example_metadata)
hd_initialize <- function(dat, metadata = NULL, is_wide = FALSE, sample_id = "DAid", var_name = "Assay", value_name = "NPX") {

  if (!is.data.frame(dat)) stop("dat must be a data frame.")
  if (!is.null(metadata) && !is.data.frame(metadata)) stop("metadata must be a data frame.")
  if (!sample_id %in% colnames(dat)) {
    stop("Sample ID column must exist in dat.")
  }
  if (!is.null(metadata) && !sample_id %in% colnames(metadata)) {
    stop("Sample ID column must exist in metadata.")
  }

  if (isFALSE(is_wide)) {
    if (!var_name %in% colnames(dat)) {
      stop("Variable name column must exist in dat.")
    }
    if (!value_name %in% colnames(dat)) {
      stop("Value column must exist in dat.")
    }

    wide_data <- hd_widen_data(dat, exclude = sample_id, names_from = var_name, values_from = value_name)
  } else {
    wide_data <- dat
  }

  check_numeric <- check_numeric_columns(wide_data)

  data_object <- list(data = wide_data,
                      metadata = metadata,
                      sample_id = sample_id,
                      var_name = var_name,
                      value_name = value_name)

  class(data_object) <- "HDAnalyzeR"

  return(data_object)
}

#' Filter an HD object based on a variable
#'
#' @description
#' Filter an HD object based on a variable, either in the data or metadata component.
#'
#' @param hd_obj An HD object to be filtered.
#' @param variable The name of the variable to filter on.
#' @param values The values to filter on (for categorical variables) or the value to compare to (for continuous variables).
#' @param flag The type of filter to apply (see details).
#' @param verbose A logical indicating whether to print messages during filtering. Default is TRUE.
#'
#' @details
#' The `flag` argument can take the following values:
#' \itemize{
#'   \item "k" : Keep only rows where the variable matches the values (categorical)
#'   \item "r" : Remove rows where the variable matches the values (categorical)
#'   \item "=" : Keep only rows where the variable equals the value (continuous)
#'   \item "<" : Keep only rows where the variable is less than the value (continuous)
#'   \item "<=" : Keep only rows where the variable is less than or equal to the value (continuous)
#'   \item ">" : Keep only rows where the variable is greater than the value (continuous)
#'   \item ">=" : Keep only rows where the variable is greater than or equal to the value (continuous)
#'   \item "!=" : Keep only rows where the variable is not equal to the value (continuous)
#' }
#'
#' @return The filtered HD object
#' @export
#' 
#' @examples
#' # Create the HDAnalyzeR object providing the data and metadata
#' hd_obj <- hd_initialize(example_data, example_metadata)
#' hd_obj
#' 
#' # Filter by categorical variable
#' hd_filter(hd_obj, variable = "Sex", values = "F", flag = "k")
#' 
#' # Filter by continuous variable
#' hd_filter(hd_obj, variable = "Age", values = 80, flag = ">")
hd_filter <- function(hd_obj, variable, values, flag, verbose = TRUE) {
  # Check if hd_obj is a valid HD object
  if (!inherits(hd_obj, "HDAnalyzeR")) {
    stop("Invalid HD object")
  }

  # Check if variable exists in data or metadata
  if (!(variable %in% names(hd_obj$data)) && !(variable %in% names(hd_obj$metadata))) {
    stop("Variable not found in data or metadata")
  }

  # Determine which component (data or metadata) the variable is in
  if (variable %in% names(hd_obj$data)) {
    var_component <- hd_obj$data
    other_component <- hd_obj$metadata
  } else {
    var_component <- hd_obj$metadata
    other_component <- hd_obj$data
  }

  # Check if variable is categorical or continuous
  var_type <- hd_detect_vartype(var_component[[variable]], unique_threshold = 5)
  if (verbose) {
    message(paste("Variable", variable, "is ", var_type))
  }
  
  # Filter data and metadata based on variable type and flag
  if (var_type == "categorical") {
    if (flag == "k") {
      # Keep only rows where variable matches values
      var_component <- var_component[var_component[[variable]] %in% values, ]
      other_component <- other_component[other_component[[hd_obj$sample_id]] %in% var_component[[hd_obj$sample_id]], ]
    } else if (flag == "r") {
      # Remove rows where variable matches values
      var_component <- var_component[!(var_component[[variable]] %in% values), ]
      other_component <- other_component[!(other_component[[hd_obj$sample_id]] %in% var_component[[hd_obj$sample_id]]), ]
    } else {
      stop("Invalid flag for categorical variable")
    }
  } else if (var_type == "continuous") {
    if (flag == "=") {
      # Keep only rows where variable equals values
      var_component <- var_component[var_component[[variable]] == values, ]
      other_component <- other_component[other_component[[hd_obj$sample_id]] %in% var_component[[hd_obj$sample_id]], ]
    } else if (flag == "<") {
      # Keep only rows where variable is less than values
      var_component <- var_component[var_component[[variable]] < values, ]
      other_component <- other_component[other_component[[hd_obj$sample_id]] %in% var_component[[hd_obj$sample_id]], ]
    } else if (flag == "<=") {
      # Keep only rows where variable is less than or equal to values
      var_component <- var_component[var_component[[variable]] <= values, ]
      other_component <- other_component[other_component[[hd_obj$sample_id]] %in% var_component[[hd_obj$sample_id]], ]
    } else if (flag == ">") {
      # Keep only rows where variable is greater than values
      var_component <- var_component[var_component[[variable]] > values, ]
      other_component <- other_component[other_component[[hd_obj$sample_id]] %in% var_component[[hd_obj$sample_id]], ]
    } else if (flag == ">=") {
      # Keep only rows where variable is greater than or equal to values
      var_component <- var_component[var_component[[variable]] >= values, ]
      other_component <- other_component[other_component[[hd_obj$sample_id]] %in% var_component[[hd_obj$sample_id]], ]
    } else if (flag == "!=") {
      # Keep only rows where variable is not equal to values
      var_component <- var_component[var_component[[variable]] != values, ]
      other_component <- other_component[!(other_component[[hd_obj$sample_id]] %in% var_component[[hd_obj$sample_id]]), ]
    } else {
      stop("Invalid flag for continuous variable")
    }
  } else {
    stop("Unsupported variable type")
  }

  # Update the original hd_obj
  if (variable %in% names(hd_obj$data)) {
    hd_obj$data <- var_component
    hd_obj$metadata <- other_component
  } else {
    hd_obj$metadata <- var_component
    hd_obj$data <- other_component
  }

  if (verbose) {
    message(paste("Filtering complete. Rows remaining:", nrow(hd_obj$data)))
  }

  return(hd_obj)
}


#' Create directory to save results
#'
#' `hd_save_path()` creates a directory with in a specified or the current path to save results.
#' The user can optionally choose to create another inner directory named with the current date.
#' If the directory already exists, a message is printed.
#'
#' @param path_name The name of the directory to create.
#' @param date If TRUE, a directory with the current date as name will be created inside the directory with `path_name`.
#'
#' @return The relative directory path as a string.
#' @export
#'
#' @examples
#' # Create a directory
#' hd_save_path("my_directory", date = FALSE)
#' unlink("my_directory", recursive = TRUE)  # Clean up the created directory
#'
#' # Create a directory and an inner directory with the current date as name
#' hd_save_path("my_directory", date = TRUE)
#' unlink("my_directory", recursive = TRUE)
#'
#' # Create a directory inside another directory
#' hd_save_path("outer_directory/inner_directory", date = FALSE)
#' unlink("outer_directory", recursive = TRUE)
#'
#' # Create a directory inside a pre existing one
#' hd_save_path("outer_directory", date = FALSE)
#' hd_save_path("outer_directory/inner_directory", date = FALSE)
#' unlink("outer_directory", recursive = TRUE)
#'
#' # Create a directory with the current date as name inside a pre existing one
#' hd_save_path("outer_directory", date = FALSE)
#' hd_save_path("outer_directory", date = TRUE)
#' unlink("outer_directory", recursive = TRUE)
hd_save_path <- function(path_name, date = FALSE) {

  if (isTRUE(date) && !is.null(path_name)) {
    current_date <- format(Sys.Date(), "%Y_%m_%d")  # Get the current date
    path_name <- paste0(path_name, "/", current_date)
  }

  # Check if the directory already exists
  if (!dir.exists(path_name)) {
    dir.create(path_name, recursive = TRUE)

    # Check if the directory was created successfully
    if (dir.exists(path_name)) {
    } else {
      warning(paste("Failed to create directory", path_name))
    }

  } else {
    message(paste("Directory", path_name, "already exists."))
  }

  return(path_name)
}


#' Save tibble or R object
#'
#' `hd_save_data()` saves either a tibble as CSV, TSV, RDS, or XLSX, or an R object
#' (for example a list) as RDS in a specified directory. If the directory does not exist,
#' it will be created automatically before saving the file. The recommended file type
#' for files that are going to be used in an R environment is RDS.
#'
#' @param dat The data to save.
#' @param path_name The name of the file to be saved. Extension options are "csv", "tsv", "rds", or "xlsx".
#' If the data is anything else than a dataframe or tibble, the file extension should be "rds".
#'
#' @return A message indicating if the file was saved successfully.
#' @export
#'
#' @examples
#' # Save a metadata dataframe as an RDS file
#' hd_save_data(example_metadata, "my_data/metadata.rds")
#'
#' unlink("my_data", recursive = TRUE)  # Clean up the created directory
hd_save_data <- function(dat, path_name) {

  dir_path <- dirname(path_name)
  file_name <- basename(path_name)
  file_ext <- sub(".*\\.", "", file_name)

  valid_file_types <- c("csv", "tsv", "rds", "xlsx")

  if (!file_ext %in% valid_file_types) {
    stop("Unsupported file type: ", file_ext, ". Supported file types are: ", paste(valid_file_types, collapse = ", "))
  }

  # Create directory if it does not exist
  dir_path <- hd_save_path(dir_path, date = FALSE)

  if (file_ext == "csv") {
    utils::write.csv(dat, path_name, row.names = FALSE)
  } else if (file_ext == "tsv") {
    utils::write.table(dat, path_name, sep = "\t", row.names = FALSE, col.names = TRUE)
  } else if (file_ext == "rds") {
    saveRDS(dat, path_name)
  } else if (file_ext == "xlsx") {
    writexl::write_xlsx(dat, path_name)
  }

  if (file.exists(path_name)) {
    return(paste("File saved as", path_name))
  } else {
    stop(paste("Failed to save file as", path_name))
  }

}


#' Import data from file
#'
#' `hd_import_data()` imports data from a file. The file format can be CSV,
#' TSV, TXT, RDA, RDS, XLSX, or Parquet format. It recognizes the file format,
#' reads it and returns it as a tibble or an R object.
#'
#' @param path_name The path to the file to import.
#'
#' @return The imported data as a tibble or an R object.
#' @export
#'
#' @examples
#' # Save a dataframe as an RDS file
#' hd_save_data(example_metadata, "my_data/metadata.rds")
#'
#' # Import the saved RDS file again as a tibble
#' hd_import_data("my_data/metadata.rds")
#'
#' unlink("my_data", recursive = TRUE)  # Clean up the created directory
hd_import_data <- function(path_name) {

  # Determine file extension from file path
  file_extension <- tools::file_ext(path_name)

  dat <- switch(tolower(file_extension),
                csv = readr::read_csv(path_name),
                tsv = readr::read_tsv(path_name),
                txt = utils::read.table(path_name, header = TRUE, stringsAsFactors = FALSE),
                rda = { load(path_name); get(ls()[1]) },
                rds = readRDS(path_name),
                xlsx = readxl::read_excel(path_name, guess_max=10000000),
                parquet = arrow::read_parquet(path_name),
                stop("Unsupported file type: ", file_extension))

  dat <- tibble::as_tibble(dat)

  return(dat)
}


#' Convert omics data to wide format
#'
#' `hd_widen_data()` transforms omics data from long to wide format with variables
#' like different "Assays" as column names and expression values like "NPX" as values.
#'
#' @param dat A tibble containing data in long format.
#' @param exclude The name of the columns to exclude from the transformation. Default is "DAid".
#' @param names_from The name of the column containing the variable names. Default is "Assay".
#' @param values_from The name of the column containing the values. Default is "NPX".
#'
#' @return A tibble containing the data in wide format.
#' @export
#'
#' @examples
#' # Olink data in long format
#' example_data
#'
#' # Transform Olink data in wide format
#' hd_widen_data(example_data)
#'
#' # Use Sample name instead of Sample ID and Olink IDs instead of Assay names
#' hd_widen_data(example_data, exclude = "Sample", names_from = "OlinkID")
hd_widen_data <- function(dat, exclude = "DAid", names_from = "Assay", values_from = "NPX") {
  suppressWarnings({
    wide_data <- dat |>
      dplyr::select(dplyr::all_of(c(exclude, names_from, values_from))) |>
      tidyr::pivot_wider(names_from = names_from, values_from = values_from)
  })
  return(wide_data)
}


#' Convert omics data to long format
#'
#' `hd_long_data()` transforms omics data from wide to long format.
#'
#' @param dat A tibble containing data in wide format.
#' @param exclude The name of the columns to exclude from the transformation. Default is "DAid".
#' @param names_to The name of the column to create for the variable names. Default is "Assay".
#' @param values_to The name of the column to create for the values. Default is "NPX".
#'
#' @return A tibble containing the data in long format.
#' @export
#'
#' @examples
#' # Olink data in wide format
#' example_data_wide <- hd_widen_data(example_data)
#' example_data_wide
#'
#' # Transform Olink data in long format
#' hd_long_data(example_data_wide)
#'
#' # Use Sample name instead of Sample ID and Olink IDs instead of Assay names
#' example_data_wide <- hd_widen_data(example_data,
#'                                    exclude = "Sample",
#'                                    names_from = "OlinkID")
#' hd_long_data(example_data_wide, exclude = "Sample", names_to = "OlinkID")
hd_long_data <- function(dat, exclude = "DAid", names_to = "Assay", values_to = "NPX") {
  suppressWarnings({
    long_data <- dat |>
      tidyr::pivot_longer(cols = -dplyr::all_of(exclude), names_to = names_to, values_to = values_to)
    })
  return(long_data)
}


#' Detect variable type
#'
#' `hd_detect_vartype()` detects the type of a variable based on its content.
#' If a variable is a factor or character, it is considered categorical.
#' If a variable is numeric and has less than or equal to `unique_threshold` unique values,
#' it is considered categorical. Otherwise, it is considered continuous.
#'
#' @param var The variable (vector or dataframe column)to detect the type of.
#' @param unique_threshold The threshold to consider a numeric variable as categorical. Default is 5.
#'
#' @return The type of the variable as a string: "categorical", "continuous", or "unknown".
#' @details
#' If you want to apply this function to each column of a dataframe, you can use the
#' `sapply()` function. See examples. For more information check `sapply` documentation.
#'
#' @export
#'
#' @examples
#' # Check categorical data
#' category <- c("A", "B", "A", "C")
#' hd_detect_vartype(category)
#'
#' # Check continuous data
#' continuous <- c(1, 2, 3, 4, 5, 6)
#' hd_detect_vartype(continuous)
#'
#' # Apply the function to each column of a dataframe
#' example <- data.frame(Category = c("A", "B", "A", "C", "B", "A"),
#'                       Continuous = c(1.1, 2.5, 3.8, 4.0, 5.8, 9),
#'                       Mixed = c(1, "1", 2, 2, "3", 3))
#'
#' sapply(example, hd_detect_vartype)
hd_detect_vartype <- function(var, unique_threshold = 5) {

  if (is.factor(var) || is.character(var)) {
    return("categorical")
  } else if (is.numeric(var)) {
    # Check number of unique values
    if (length(unique(var)) <= unique_threshold) {
      return("categorical")
    } else {
      return("continuous")
    }
  } else {
    return("unknown")  # For unsupported types
  }

}


#' Bin variables
#'
#' `hd_bin_columns()` bins continuous variables and labels them with ranges.
#'
#' @param dat The data to bin.
#' @param column_types A vector containing the type of each variable (column) in the dataframe.
#' @param bins The number of bins to create. Default is 5.
#' @param round_digits The number of digits to round the bin ranges to. Default is 0.
#'
#' @return The data with continuous variables binned.
#' @details
#' In case of a dataset with many variables, it is recommended to use the function
#' `hd_detect_vartype()` to automatically detect the type of each variable. See examples.
#'
#' @export
#'
#' @examples
#' # Example dataframe
#' test_data <- data.frame(
#'   age = c(25, 35, 45, 55, 65),
#'   BMI = c(25, 35, 30, 32, 28),
#'   sex = c("M", "F", "M", "F", "M")
#' )
#'
#' column_types <- c("continuous", "continuous", "categorical")
#' hd_bin_columns(test_data, column_types, bins = 3)
#'
#' # Automatically detect variable types
#' test_data <- data.frame(Category = c("A", "B", "A", "C", "B", "A"),
#'                         Continuous = c(1.1, 2.5, 3.8, 4.0, 5.8, 9))
#' column_types <- sapply(test_data, hd_detect_vartype)
#'
#' # The variable to be binned has one significant digit
#' # So we will also round the bins to one digit
#' hd_bin_columns(test_data, column_types, bins = 3, round_digits = 1)
hd_bin_columns <- function(dat, column_types, bins = 5, round_digits = 0) {
  # Ensure inputs are valid
  if (!is.data.frame(dat)) stop("data must be a dataframe.")
  if (length(column_types) != ncol(dat)) stop("column_types length must match the number of columns in data.")
  if (!all(column_types %in% c("categorical", "continuous"))) {
    stop("column_types must contain only 'categorical' or 'continuous'.")
  }

  # Create a copy of the data to modify
  binned_data <- dat

  for (i in seq_along(column_types)) {
    if (column_types[i] == "continuous") {
      # Bin continuous columns and label with ranges
      breaks <- seq(
        from = min(binned_data[[i]], na.rm = TRUE),
        to = max(binned_data[[i]], na.rm = TRUE),
        length.out = bins + 1
      )
      binned_data[[i]] <- cut(
        binned_data[[i]],
        breaks = breaks,
        include.lowest = TRUE,
        labels = paste0(utils::head(round(breaks, round_digits), -1),
                        "-",
                        utils::tail(round(breaks, round_digits), -1))
      )
    }
  }

  return(binned_data)
}


#' Check for non-numeric columns
#'
#' `check_numeric_columns()` checks if all columns except the first (Sample ID)
#' in a dataframe are numeric.
#'
#' @param dat The dataframe to check.
#'
#' @return A warning with the names of non-numeric columns if any.
#' @keywords internal
check_numeric_columns <- function(dat) {

  if (!is.data.frame(dat)) {
    stop("Input must be a data frame or tibble.")
  }

  # Exclude the first column as it should be the sample ID
  cols_to_check <- dat[-1]

  non_numeric <- NULL
  non_numeric <- names(cols_to_check)[!sapply(cols_to_check, function(col) {
    suppressWarnings({ # Suppress warnings during coercion
      coerced <- as.numeric(col)
      return(!any(is.na(coerced) & !is.na(col))) # TRUE if valid numeric after coercion
    })
  })]

  if (length(non_numeric) > 0) {
    warning("The following columns are not numeric: ", paste(non_numeric, collapse = ", "))
  }

  invisible(non_numeric)
}


#' Log transform data with base 2
#'
#' `hd_log_transform()` log transforms the data in a dataset.
#' It replaces non-positive values (<= 0) with NA values and informs the user.
#'
#' @param dat An HDAnalyzeR object or a dataset in wide format.
#'
#' @return The log-transformed data.
#' @export
#'
#' @examples
#' # Create the HDAnalyzeR object providing the data and metadata
#' hd_object <- hd_initialize(example_data, example_metadata)
#' hd_object$data
#'
#' # Log transform the data
#' hd_object_transformed <- hd_log_transform(hd_object)
#' # Normally you should not transform Olink data as they are already log-transformed
#' hd_object_transformed$data
hd_log_transform <- function(dat) {

  if (inherits(dat, "HDAnalyzeR")) {
    if (is.null(dat$data)) {
      stop("The 'data' slot of the HDAnalyzeR object is empty. Please provide the data to run the DE analysis.")
    }
    wide_data <- dat[["data"]]
    sample_id <- dat[["sample_id"]]
  } else {
    wide_data <- dat
    sample_id <- colnames(dat)[1]
  }

  check_numeric <- check_numeric_columns(wide_data)

  # Check for negative or zero values
  if (any(wide_data[, -1] <= 0, na.rm = TRUE)) {
    warning("Data contains non-positive values (<= 0). These will be replaced with NA during log transformation.")
  }

  suppressWarnings({
    wide_data[, -1] <- lapply(wide_data[, -1], function(col) {
      transformed <- log2(ifelse(col > 0, col, NA))
      transformed[is.nan(transformed)] <- NA  # Replace NaN with NA
      return(transformed)
    })
  })

  if (inherits(dat, "HDAnalyzeR")) {
    dat[["data"]] <- wide_data
    return(dat)
  } else {
    return(wide_data)
  }
}
