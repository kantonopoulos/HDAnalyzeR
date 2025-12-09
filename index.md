# HDAnalyzeR

[![R-CMD-check](https://github.com/kantonopoulos/HDAnalyzeR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/kantonopoulos/HDAnalyzeR/actions/workflows/R-CMD-check.yaml)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.13786124.svg)](https://doi.org/10.5281/zenodo.13786124)
[![Version](https://img.shields.io/badge/Version-1.0.0-purple)](https://github.com/kantonopoulos/HDAnalyzeR)
[![License](https://img.shields.io/badge/license-Apache2.0-yellow)](https://github.com/kantonopoulos/HDAnalyzeR/blob/main/LICENSE.md)

HDAnalyzeR is an R package developed by the Human Disease Blood Atlas to
simplify data analysis for biomarker discovery in disease research. It
provides a set of user-friendly functions to efficiently process omics
data and gain insights into disease mechanisms. With HDAnalyzeR, you can
easily perform tasks like protein differential expression analysis,
classification modeling, dimensionality reduction, imputation, and data
visualization.

The package is designed to be intuitive and accessible, even for those
with limited programming experience, making it ideal for
bioinformaticians, clinicians, and molecular biologists. It offers
ready-to-use functions that reduce analysis time while ensuring
high-quality results. Its powerful visualization tools also help present
findings in clear, publication-ready plots, improving both the analysis
and communication of results. Whether you’re analyzing omics data for
research or clinical applications, HDAnalyzeR simplifies the process,
enhances reproducibility, and supports the discovery of potential
biomarkers in disease research.

## Key Features

- **Differential Expression Analysis:** Perform protein differential
  expression analysis with built-in support for multiple methods.
- **Machine Learning Models:** Train and evaluate classification models
  with integrated variable importance visualization.
- **Pathway Enrichment Analysis:** Identify enriched pathways and
  visualize results in multiple formats.
- **Weighted Correlation Network Analysis:** Construct co-expression
  networks and identify modules of interest.
- **Dimensionality Reduction:** Easily apply PCA or other dimensionality
  reduction techniques to explore complex data.
- **Imputation and NA Handling:** Handle missing data with imputation
  techniques and filtering options.
- **Publication-Ready Visualizations:** Generate high-quality plots,
  including volcano plots, heatmaps, and feature importance plots.

## Installation

You can install the **latest** (recommended) or development version of
HDAnalyzeR from GitHub:

``` r
# Install devtools if you haven't already
install.packages("devtools")

# Install HDAnalyzeR latest version
options(timeout = 1200)  # Set timeout to 20 minutes to avoid timeout errors
devtools::install_github("kantonopoulos/HDAnalyzeR")

# Install HDAnalyzeR development version
options(timeout = 1200) 
devtools::install_github("kantonopoulos/HDAnalyzeR")
```

## App Interface

HDAnalyzeR combines a [graphical user interface
(GUI)](https://hdanalyzer.serve.scilifelab.se) to make it accessible to
researchers with no coding experience. With this app, users can run
complex analyses even on their phone or tablet. The app includes
features for data and metadata uploads, exploratory data analysis
visualizations, dimensionality reduction, differential expression
analysis, and simple classification models. All outputs can be
downloaded as .CSV or .PNG files.

Check our [short video tutorial](https://youtu.be/Fxk4wfVUezM) to learn
more about it!

## Usage

The following example showcases how to perform differential expression
analysis and plot the results. It is one of the many features of
HDAnalyzeR. A complete guide is available through [package’s
documentation](https://kantonopoulos.github.io/HDAnalyzeR/).

``` r
library(HDAnalyzeR)

# Prepare data
hd_object <- hd_initialize(example_data, example_metadata)

# Run differential expression analysis
de_results <- hd_de_limma(hd_object, case = "AML")

# Plot volcano plot
de_results <- hd_plot_volcano(de_results)

# DE results and volcano plot for AML
de_results$de_res
de_results$volcano_plot
```

## Issues and Support

If you encounter any bugs or you want to recommend new features and
changes to existing ones, please open a new issue on our GitHub
repository.

## Contact

For any questions or further information, please contact us at
<k.antono@outlook.com>.
