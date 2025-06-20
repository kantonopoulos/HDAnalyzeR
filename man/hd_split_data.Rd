% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classification_models.R
\name{hd_split_data}
\alias{hd_split_data}
\title{Split data}
\usage{
hd_split_data(
  dat,
  metadata = NULL,
  variable = "Disease",
  metadata_cols = NULL,
  ratio = 0.75,
  seed = 123
)
}
\arguments{
\item{dat}{An HDAnalyzeR object or a dataset in wide format and sample ID as its first column.}

\item{metadata}{A dataset containing the metadata information with the sample ID as the first column. If a HDAnalyzeR object is provided, this parameter is not needed.}

\item{variable}{The name of the metadata variable containing the case and control groups. Default is "Disease".}

\item{metadata_cols}{The metadata variables to be selected from the metadata as predictors. Default is NULL.}

\item{ratio}{The ratio of training data to test data. Default is 0.75.}

\item{seed}{Seed for reproducibility. Default is 123.}
}
\value{
A split object containing train and test data splits.
}
\description{
\code{hd_split_data()} splits the data into training and test sets based on the ratio
provided. It also stratifies the data based on the variable of interest if any.
At this stage the user can select metadata variable predictors to be included in
the training and test sets.
}
\details{
It is always recommended to split the data into training and test sets to avoid
overfitting. This function also initializes the model object to be used in the
downstream machine learning pipeline. The user can create their own model object
with the train and test data splits, but it must be a list with the train set as
the first and the test set as the second element. The function utilizes the
\code{initial_split()} function from the \code{rsample} package to split the data. For more
information on the \code{rsample} package, please check their documentation.
}
\examples{
# Initialize an HDAnalyzeR object
hd_object <- hd_initialize(example_data, example_metadata)

# Split the data into training and test sets
hd_split_data(hd_object, variable = "Disease")
}
