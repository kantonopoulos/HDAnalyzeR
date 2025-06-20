% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/visualize.R
\name{hd_plot_feature_network}
\alias{hd_plot_feature_network}
\title{Feature network}
\usage{
hd_plot_feature_network(
  feature_panel,
  plot_color = "Scaled_Importance",
  class_palette = NULL,
  importance_palette = NULL,
  seed = 123
)
}
\arguments{
\item{feature_panel}{A dataset containing the features and the classes. The dataframe must contain at least 3 columns: Feature, Class and the color variable. See examples.}

\item{plot_color}{The color variable to plot. Default is "Scaled_Importance".}

\item{class_palette}{The color palette for the classes. If it is a character, it should be one of the palettes from \code{hd_palettes()}. Default is NULL.}

\item{importance_palette}{A named list or vector that contains the high and low colors (for example c("high" = "grey30", "low" = "grey80")). If NULL the default colors are used. Default is NULL.}

\item{seed}{seed Seed for reproducibility. Default is 123.}
}
\value{
The feature network plot.
}
\description{
\code{hd_plot_feature_network} plots the network of the specified features. The bigger nodes represent the
classes and the smaller nodes represent the features. The color of the nodes is based on the
color variable which can be either the importance, the logFC or any value that can rank the features.
}
\examples{
\dontrun{
# Initialize an HDAnalyzeR object
hd_object <- hd_initialize(example_data, example_metadata)

# Create a feature panel from differential expression results
de_results_aml <- hd_de_limma(hd_object, case = "AML")
de_results_lungc <- hd_de_limma(hd_object, case = "LUNGC")
de_results_cll <- hd_de_limma(hd_object, case = "CLL")
de_results_myel <- hd_de_limma(hd_object, case = "MYEL")
de_results_gliom <- hd_de_limma(hd_object, case = "GLIOM")

feature_panel <- de_results_aml[["de_res"]] |>
  dplyr::filter(adj.P.Val < 0.05 & abs(logFC) > 1) |>
  dplyr::mutate(Class = "AML") |>
  dplyr::bind_rows(de_results_cll[["de_res"]] |>
                     dplyr::filter(adj.P.Val < 0.05 & abs(logFC) > 1) |>
                     dplyr::mutate(Class = "CLL"),
                   de_results_myel[["de_res"]] |>
                     dplyr::filter(adj.P.Val < 0.05 & abs(logFC) > 1) |>
                     dplyr::mutate(Class = "MYEL"),
                   de_results_lungc[["de_res"]] |>
                     dplyr::filter(adj.P.Val < 0.05 & abs(logFC) > 1) |>
                     dplyr::mutate(Class = "LUNGC"),
                   de_results_gliom[["de_res"]] |>
                     dplyr::filter(adj.P.Val < 0.05 & abs(logFC) > 1) |>
                     dplyr::mutate(Class = "GLIOM"))

print(head(feature_panel, 5))

hd_plot_feature_network(feature_panel,
                        plot_color = "logFC",
                        class_palette = "cancers12",
                        importance_palette = c("high" = "red4", "low" = "grey90"))
}
}
