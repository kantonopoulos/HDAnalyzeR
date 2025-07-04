% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dim_reduction.R
\name{hd_auto_pca}
\alias{hd_auto_pca}
\title{Run PCA analysis and plot the results}
\usage{
hd_auto_pca(
  dat,
  metadata = NULL,
  components = 10,
  by_sample = TRUE,
  impute = TRUE,
  plot_x = "PC1",
  plot_y = "PC2",
  plot_color = NULL,
  plot_palette = NULL
)
}
\arguments{
\item{dat}{An HDAnalyzeR object or a dataset in wide format and sample ID as its first column.}

\item{metadata}{A dataset containing the metadata information with the sample ID as the first column. If a HDAnalyzeR object is provided, this parameter is not needed.}

\item{components}{The number of PCs to be calculated. Default is 10.}

\item{by_sample}{If TRUE, points represent samples. If FALSE, points represent features. Default is TRUE.}

\item{impute}{If TRUE, missing values will be imputed using the k-nearest neighbors algorithm (k = 5). Default is TRUE.}

\item{plot_x}{The name of the column in \code{dim_res} that contains the x-axis values. Default is "PC1".}

\item{plot_y}{The name of the column in \code{dim_res} that contains the y-axis values. Default is "PC2".}

\item{plot_color}{The name of the column in \code{dim_res} that contains the variable to be used to plot the points color. Default is NULL.}

\item{plot_palette}{The color palette for the plot. If it is a character, it should be one of the palettes from \code{hd_palettes()}. Default is NULL.}
}
\value{
A list with the PCA results and PCA plots.
}
\description{
\code{hd_auto_pca()} runs a PCA analysis on the provided data and plots the PCA results.
}
\examples{
# Create the HDAnalyzeR object providing the data and metadata
hd_object <- hd_initialize(example_data, example_metadata)

# Run the PCA analysis
hd_auto_pca(hd_object, components = 20, plot_color = "Disease", plot_palette = "cancers12")
}
