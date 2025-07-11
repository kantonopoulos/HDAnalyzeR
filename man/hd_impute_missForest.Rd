% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/impute.R
\name{hd_impute_missForest}
\alias{hd_impute_missForest}
\title{Impute via missForest}
\usage{
hd_impute_missForest(
  dat,
  maxiter = 10,
  ntree = 100,
  parallelize = "no",
  seed = 123,
  verbose = TRUE
)
}
\arguments{
\item{dat}{An HDAnalyzeR object or a dataset in wide format and sample ID as its first column.}

\item{maxiter}{The maximum number of iterations.}

\item{ntree}{The number of trees to grow.}

\item{parallelize}{If "no", the imputation is done in a single core. If "variables", the imputation is done in parallel for each variable. If "forest", the imputation is done in parallel for each tree. For more information, check the \code{missForest} documentation.}

\item{seed}{The seed to be used in the imputation. Default is 123.}

\item{verbose}{If TRUE, the percentage of missing values in each column is displayed.}
}
\value{
The imputed dataset.
}
\description{
\code{impute_missForest()} imputes missing values in a dataset using the \code{missForest} method.
It can also display the percentage of missing values in each column before imputation.
}
\details{
This is the slowest and more complex imputation method. If KNN works fine, it is
recommended to use it instead of \code{missForest}. In case of large datasets, it is recommended
to parallelize the imputation. However, the user must have the \code{doParallel} package installed
and register a cluster before running the function. An example of how to parallelize the
imputation is provided in the examples section.
}
\examples{
# Create the HDAnalyzeR object providing the data and metadata
hd_object <- hd_initialize(example_data, example_metadata)
hd_object$data

# Data after imputation
res <- hd_impute_missForest(hd_object, maxiter = 1, ntree = 50)
res$data

\dontrun{
# Parallelize the imputation
library(doParallel)  # Load the doParallel package
cl <- makeCluster(4)  # Create a cluster with 4 cores
registerDoParallel(cl)  # Register the cluster
res <- hd_impute_missForest(hd_object, maxiter = 1, ntree = 50, parallelize = "forests")
}
}
