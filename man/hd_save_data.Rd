% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{hd_save_data}
\alias{hd_save_data}
\title{Save tibble or R object}
\usage{
hd_save_data(dat, path_name)
}
\arguments{
\item{dat}{The data to save.}

\item{path_name}{The name of the file to be saved. Extension options are "csv", "tsv", "rds", or "xlsx".
If the data is anything else than a dataframe or tibble, the file extension should be "rds".}
}
\value{
A message indicating if the file was saved successfully.
}
\description{
\code{hd_save_data()} saves either a tibble as CSV, TSV, RDS, or XLSX, or an R object
(for example a list) as RDS in a specified directory. If the directory does not exist,
it will be created automatically before saving the file. The recommended file type
for files that are going to be used in an R environment is RDS.
}
\examples{
# Save a metadata dataframe as an RDS file
hd_save_data(example_metadata, "my_data/metadata.rds")

unlink("my_data", recursive = TRUE)  # Clean up the created directory
}
