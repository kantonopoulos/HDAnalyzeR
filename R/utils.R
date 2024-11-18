#' Create directory to save results
#'
#' `hd_save_path()` creates a directory with in a specified or the current path to save results.
#' The user can optionally choose to create another inner directory named with the current date.
#' If the directory already exists, a message is printed.
#'
#' @param path_name The name of the directory to create.
#' @param date If TRUE, a directory with the current date as name will be created inside the directory with `dir_name`.
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


#' Save tibbles or R objects
#'
#' `hd_save_data()` saves either a tibble as CSV, TSV, RDS, or XLSX, or an R object
#' (for example a list) as RDS in a specified directory. If the directory does not exist,
#' it will be created. The recommended file type for files that are going to be used mainly
#' in R is RDS.
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
#' like different `Assays` as column names and expression values like `NPX` as values.
#'
#' @param dat A tibble containing data in long format.
#' @param exclude The name of the columns to exclude from the transformation.
#' @param names_from The name of the column containing the variable names.
#' @param values_from The name of the column containing the values.
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

  wide_data <- dat |>
    dplyr::select(all_of(c(exclude, names_from, values_from))) |>
    tidyr::pivot_wider(names_from = names_from, values_from = values_from)

  return(wide_data)
}


#' Convert omics data to long format
#'
#' `hd_long_data()` transforms omics data from wide to long format.
#'
#' @param dat A tibble containing data in wide format.
#' @param exclude The name of the columns to exclude from the transformation.
#' @param names_to The name of the column to create for the variable names.
#' @param values_to The name of the column to create for the values.
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
hd_long_data <- function(dat, exclude = "DAid", names_to = "Assay", values_to = "NPX") {

  long_data <- dat |>
    tidyr::pivot_longer(cols = -all_of(exclude), names_to = names_to, values_to = values_to)

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
#'                            Continuous = c(1.1, 2.5, 3.8, 4.0, 5.8, 9),
#'                            Mixed = c(1, "1", 2, 2, "3", 3))
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
    return("unknown")  # For unsupported or mixed types
  }

}
