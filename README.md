# HDAnalyzeR <a href="https://hda1472.github.io/HDAnalyzeR"><img src="man/figures/logo.png" align="right" height="200" alt="HDAnalyzeR website" /></a>

[![R-CMD-check](https://github.com/HDA1472/HDAnalyzeR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/HDA1472/HDAnalyzeR/actions/workflows/R-CMD-check.yaml)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.13786124.svg)](https://doi.org/10.5281/zenodo.13786124)
[![Version](https://img.shields.io/badge/Version-1.1.0-purple)](https://github.com/HDA1472/HDAnalyzeR) 
[![License](https://img.shields.io/badge/license-Apache2.0-yellow)](https://github.com/HDA1472/HDAnalyzeR/blob/main/LICENSE.md)

HDAnalyzeR is an R package developed by the Human Disease Blood Atlas project, designed to facilitate proteomics analysis for biomarker selection from blood plasma samples. HDAnalyzeR offers ready-to-use functions for common proteomics tasks such as protein differential expression analysis, classification models, imputation methods, dimensionality reduction, and data visualization, aiming to streamline workflows and enhance the standardization and efficiency of biomarker discovery in disease research.

## Installation

You can install the **latest** (recommended) or development version of HDAnalyzeR from GitHub:

``` r
# Install devtools if you haven't already
install.packages("devtools")

# Install HDAnalyzeR latest version
options(timeout = 1200)  # Set timeout to 20 minutes to avoid timeout errors
devtools::install_github("HDA1472/HDAnalyzeR@v1.1.0")

# Install HDAnalyzeR development version
options(timeout = 1200)
devtools::install_github("HDA1472/HDAnalyzeR")
```

## Cheat Sheet

<a href="https://github.com/HDA1472/HDAnalyzeR/blob/main/cheatsheet/hdanalyzer_cheat-sheet.pdf"><img src="cheatsheet/hdanalyzer_cheat-sheet.png" width="630" height="252"/></a>

## Usage

The following example showcases how to perform a differential expression analysis. It is one of the many features of HDAnalyzeR. A complete guide is available through [package's documentation](https://hda1472.github.io/HDAnalyzeR/).

``` r
library(HDAnalyzeR)

# Prepare data
hd_object <- hd_initialize(example_data, example_metadata)

# Run differential expression analysis
de_results <- hd_run_de_limma(hd_object, case = "AML")

# Plot volcano plot
de_results <- hd_plot_volcano(de_results)

# DE results and volcano plot for AML
de_results$de_res
de_results$volcano_plot
```

## Citation

Antonopoulos, K., Bueno Alvez, M., Johansson, E., & Edfors Arfwidsson, F. (2024). HDAnalyzeR: HDA Internal Package for Streamed-line Proteomics Analysis (1.1.0). Zenodo. https://doi.org/10.5281/zenodo.13786124

## Issues and Support

If you encounter any bugs or you want to recommend new features and changes to existing ones, please open a new issue on our GitHub repository.

## Contact

For any questions or further information, please contact us at [konstantinos.antonopoulos@scilifelab.se](mailto:konstantinos.antonopoulos@scilifelab.se).
