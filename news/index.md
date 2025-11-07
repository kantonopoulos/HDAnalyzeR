# Changelog

## HDAnalyzeR 1.1.0

### General Updates

- **Improved Function Naming:**  
  All functions now start with `hd_` for consistency and easier
  searching in RStudio, especially when using multiple packages
  simultaneously.

- **Heatmap Updates:**  
  Heatmaps are now converted to `ggplot` objects using `ggplotify`,
  enhancing flexibility and uniformity. Now all plots are ggplot
  objects.

- **Enhanced Vignettes:**  
  Vignettes have been streamlined by suppressing unnecessary outputs and
  messages.

- **Bug Fixes & Warnings:**  
  Addressed several bugs and added informative warnings and error
  handling to guide users when potential issues arise.

- **Preprocess Module Removal:**  
  The preprocess module was removed due to redundancy and limited
  functionality.

### Utility Functions

- **New Features:**
  - [`hd_initialize()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_initialize.md):
    Initializes a base object used across all functions for simplified
    and consistent workflows.  
  - [`hd_detect_vartype()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_detect_vartype.md):
    Identifies if a variable is categorical or continuous.
  - [`hd_bin_columns()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_bin_columns.md):
    Bins continuous variables into categories.
  - [`hd_filter()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_filter.md):
    Filters data based on specified variables and updates the data and
    metadata automatically
  - [`hd_log_transform()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_log_transform.md):
    Log-transforms data.
  - [`hd_long_data()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_long_data.md):
    Converts wide data to long format.
- **Simplified Usage:**
  - [`hd_save_data()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_save_data.md)
    (formerly `save_df()`) now has more user-friendly arguments,
    aligning with
    [`hd_import_data()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_import_data.md)
    (formerly `import_df()`).
  - [`hd_widen_data()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_widen_data.md)
    supports custom column selection for wide data creation with
    arguments like `exclude`, `names_from`, and `values_from`.
- **Deprecated Functions:**  
  `generate_df()` was deprecated as it is now redundant.

### Dimensionality Reduction

- **Bug Fixes:**  
  Fixed an issue in
  [`hd_auto_pca()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_auto_pca.md)
  (formerly `do_pca()`) when handling more than 9 principal components.

### Palettes & Themes

- **New Functions:**
  - [`hd_show_palettes()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_show_palettes.md):
    Displays all available palettes in the package.
  - Added selected palettes from the `ggsci` package.

### Imputation

- **New Features:**
  - [`hd_na_search()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_na_search.md):
    Summarizes NA distributions in heatmaps and supports user-defined
    metadata annotations.  
  - [`hd_omit_na()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_omit_na.md):
    Removes rows with NA values based on specified columns.
- **Removed Features:**  
  `impute_mice()` was removed for simplicity. Users requiring advanced
  imputation can directly use the `mice` package.

### Quality Control (QC) Summary

- **New & Enhanced Features:**
  - Added a `cor_method` argument to
    [`qc_summary_data()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/qc_summary_data.md)
    for customizable correlation methods.  
  - Merged
    [`qc_summary_data()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/qc_summary_data.md)
    and
    [`qc_summary_metadata()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/qc_summary_metadata.md)
    into a single function,
    [`hd_qc_summary()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_qc_summary.md),
    which now visualizes all metadata columns.
- **Removed Normality Checks:**  
  Removed automatic normality checks for large datasets due to
  sensitivity issues. Instead, users are encouraged to use histograms or
  QQ plots for this purpose.

### Differential Expression Analysis

- **Streamlined Functions:**
  - Merged `do_limma()` and `do_limma_continuous()` into
    [`hd_de_limma()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_de_limma.md),
    which auto-detects variable types.  
  - [`hd_plot_volcano()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_plot_volcano.md)
    is now a standalone function to reduce the number of arguments but
    still keep the flexibility.
- **Improved Customization:**  
  Added a `user_defined_proteins` argument to
  [`hd_plot_volcano()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_plot_volcano.md),
  allowing users to label specific proteins on volcano plots.

### Classification Models

- **Model Updates:**  
  Merged multiclass and binary models.

- **Enhanced Visualizations:**

  - Multiclassification models now include a variable importance plot.  
  - AUC bar plots were removed to make the output more consistent with
    the binary classification models.
  - Probability plots were added to visualize the distribution of
    probabilities for each class.

### Visualization Functions

- **New Functions:**
  - Added `plot_feature_summary_heatmap()`: Summarizes differential
    expression and classification model results in a single heatmap.
  - Added `plot_feature_summary_network()`: Summarizes differential
    expression or classification model features results in a single
    network.
- **Bug Fixes & Improvements:**  
  Fixed color-matching issues in bar plots for
  [`hd_plot_de_summary()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_plot_de_summary.md)
  and
  [`hd_plot_model_summary()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_plot_model_summary.md)
  caused by frequency ties. Bars are now colored correctly.

### Pathway Enrichment Analysis

- **New Features**
  - Added ready-to-use ORA background datasets for
    [`hd_ora()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_ora.md).
  - Added
    [`hd_show_backgrounds()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_show_backgrounds.md):
    See available backgrounds for ORA.
- **Streamlined Outputs:**  
  Removed unnecessary returns from
  [`hd_plot_gsea()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_plot_gsea.md).

### Clustering

- **New Features:**
  - Added
    [`hd_cluster_samples()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_cluster_samples.md):
    Clusters samples based on selected features in k clusters. The
    number k is either user-defined or determined using the gap
    statistic.
  - Added
    [`hd_assess_clusters()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_assess_clusters.md):
    Assesses the quality of clustering using the cluster’s Jaccard index
    and sample size.

### New Analysis Module: WGCNA

Added the Weighted Gene Co-expression Network Analysis (WGCNA) module
for network analysis.

- **Module Functions:**
  - [`hd_wgcna()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_wgcna.md):
    Performs WGCNA analysis on the data.
  - [`hd_plot_wgcna()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_plot_wgcna.md):
    Visualizes the WGCNA results.

------------------------------------------------------------------------

## HDAnalyzeR 1.0.0 (2024-08-19)

Initial release of HDAnalyzeR.
