% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/impute.R
\name{hd_na_search}
\alias{hd_na_search}
\title{Heatmap summary of missing values}
\usage{
hd_na_search(
  dat,
  metadata = NULL,
  annotation_vars = NULL,
  palette = NULL,
  x_labels = FALSE,
  y_labels = FALSE
)
}
\arguments{
\item{dat}{An HDAnalyzeR object or a dataset in wide format and sample ID as its first column.}

\item{metadata}{A dataset containing the metadata information with the sample ID as the first column. If a HDAnalyzeR object is provided, this parameter is not needed.}

\item{annotation_vars}{The metadata columns to include in the summary.}

\item{palette}{A list of color palettes for the heatmap annotations. The names of the list should match the column names in \code{annotation_vars}. Default is NULL.}

\item{x_labels}{If TRUE, show x-axis labels. Default is FALSE.}

\item{y_labels}{If TRUE, show y-axis labels. Default is FALSE.}
}
\value{
A list containing the summary of missing values and the heatmap visualization.
}
\description{
\code{hd_na_search()} provides a visual summary of missing values in a dataset as an annotated heatmap.
It allows the user to specify the metadata columns to include in the summary and the color palette
to use for the heatmap annotations.
}
\details{
When using continuous metadata variables, they are automatically binned
into categories of 5 bins to make the heatmap more informative and easier to interpret.
If the user wants to use a different number of bins, they can bin the data before
using the \code{hd_bin_columns()} function and its \code{bins} argument.

Also, when coloring annotations, the user can use custom palettes or the
Human Protein Atlas (HPA) palettes. It is not required to provide a palette
for all annotations, but when a palette is provided, it must be in correct
format (check examples bellow).
}
\examples{
# Create the HDAnalyzeR object providing the data and metadata
hd_object <- hd_initialize(example_data, example_metadata)

# Use custom palettes for coloring annotations
palette = list(Sex = c(M = "blue", F = "pink"))
na_res <- hd_na_search(hd_object,
                       annotation_vars = c("Age", "Sex"),
                       palette = palette)
na_res$na_heatmap

# Use a mix of custom and HPA palettes for coloring annotations
palette = list(Disease = "cancers12", Sex = c(M = "blue", F = "pink"))
na_res <- hd_na_search(hd_object,
                       annotation_vars = c("Disease", "Sex"),
                       palette = palette)
na_res$na_heatmap
}
