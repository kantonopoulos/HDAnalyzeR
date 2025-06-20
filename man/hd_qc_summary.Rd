% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qc_summary.R
\name{hd_qc_summary}
\alias{hd_qc_summary}
\title{Summarize quality control information}
\usage{
hd_qc_summary(
  dat,
  metadata = NULL,
  variable,
  palette = NULL,
  unique_threshold = 5,
  cor_threshold = 0.8,
  cor_method = "pearson",
  verbose = TRUE
)
}
\arguments{
\item{dat}{An HDAnalyzeR object or a dataset in wide format and sample ID as its first column.}

\item{metadata}{A dataset containing the metadata information with the sample ID as the first column. If a HDAnalyzeR object is provided, this parameter is not needed.}

\item{variable}{The name of the metadata variable (column) containing the different classes (for example the column that contains your case and control groups).}

\item{palette}{A list of color palettes for the plots. The names of the list should match the column names in the metadata. Default is NULL.}

\item{unique_threshold}{The threshold to consider a numeric variable as categorical. Default is 5.}

\item{cor_threshold}{The threshold to consider a protein-protein correlation as high. Default is 0.8.}

\item{cor_method}{The method to calculate the correlation. Default is "pearson". Other options are "spearman" and "kendall".}

\item{verbose}{Whether to print the summary. Default is TRUE.}
}
\value{
A list containing the qc summary of data and metadata.
}
\description{
\code{hd_qc_summary()} summarizes the quality control results of the input data and metadata.
It returns general information about the datasets, missing value information,
protein-protein correlations, and metadata summary visualizations.
}
\examples{
# Create the HDAnalyzeR object providing the data and metadata
hd_object <- hd_initialize(example_data,
                           example_metadata |> dplyr::select(-Sample))

# Run the quality control summary
qc_res <- hd_qc_summary(hd_object,
                        variable = "Disease",
                        palette = list(Disease = "cancers12", Sex = "sex"),
                        cor_threshold = 0.7,
                        verbose = TRUE)

# Data summary -------------------------------------------------------------
qc_res$data_summary$na_col_hist
qc_res$data_summary$na_row_hist
qc_res$data_summary$cor_results
qc_res$data_summary$cor_heatmap

# Metadata summary ---------------------------------------------------------
qc_res$metadata_summary$na_col_hist
qc_res$metadata_summary$na_row_hist
qc_res$metadata_summary$Age
qc_res$metadata_summary$Sex
qc_res$metadata_summary$BMI
qc_res$metadata_summary$Stage
qc_res$metadata_summary$Grade
qc_res$metadata_summary$Cohort
}
