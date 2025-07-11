% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/impute.R
\name{hd_omit_na}
\alias{hd_omit_na}
\title{Omit missing values}
\usage{
hd_omit_na(dat, columns = NULL)
}
\arguments{
\item{dat}{An HDAnalyzeR object or a dataset in wide format and sample ID as its first column.}

\item{columns}{The columns to consider for the removal of missing values.}
}
\value{
The dataset without the rows containing missing values.
}
\description{
\code{hd_omit_na()} removes rows with missing values from a dataset. It allows the user to
specify the columns to consider for the removal of missing values. If no columns are
provided, the function removes rows with missing values in any column.
}
\examples{
# Create the HDAnalyzeR object providing the data and metadata
hd_object <- hd_initialize(example_data, example_metadata)
hd_object$data

# Data after removing missing values
res <- hd_omit_na(hd_object)
res$data

# Data after removing missing values in specific columns
res <- hd_omit_na(hd_object, columns = "AARSD1")
res$data
}
