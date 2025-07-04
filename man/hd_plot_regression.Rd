% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/visualize.R
\name{hd_plot_regression}
\alias{hd_plot_regression}
\title{Regression plot}
\usage{
hd_plot_regression(
  dat,
  metadata = NULL,
  metadata_cols = NULL,
  x,
  y,
  se = FALSE,
  line_color = "#883268",
  r_2 = TRUE
)
}
\arguments{
\item{dat}{An HDAnalyzeR object or a dataset in wide format and sample ID as its first column.}

\item{metadata}{A dataset containing the metadata information with the sample ID as the first column. If a HDAnalyzeR object is provided, this parameter is not needed.}

\item{metadata_cols}{The metadata variables to plot. Default is NULL.}

\item{x}{The column name of the x-axis variable. It should be a variable of the data or metadata.}

\item{y}{The column name of the y-axis variable. It should be a variable of the data or metadata.}

\item{se}{Whether to add the standard error of the regression line. Default is FALSE.}

\item{line_color}{The color of the regression line.}

\item{r_2}{Whether to add the R-squared and p-value to the plot. Default is TRUE.}
}
\value{
The scatter plot with the regression line.
}
\description{
\code{hd_plot_regression} plots a scatter plot with a linear regression line.
It is possible to add the standard error of the regression line, as well as the
R-squared and p-value.
}
\examples{
# Initialize an HDAnalyzeR object
hd_object <- hd_initialize(example_data, example_metadata)

# Scatter plot for AARSD1 and ABL1
hd_plot_regression(hd_object,
                   x = "AARSD1",
                   y = "ABL1",
                   se = TRUE)

# Scatter plot for AARSD1 and Age (metadata variable)
hd_plot_regression(hd_object,
                   metadata_cols = "Age",
                   x = "AARSD1",
                   y = "Age",
                   r_2 = FALSE)
}
