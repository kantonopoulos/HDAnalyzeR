# HDAnalyzeR 1.1.0

## General Updates
- **Improved Function Naming:**  
  All functions now start with `hd_` for consistency and easier searching in RStudio, especially when using multiple packages simultaneously.

- **Heatmap Updates:**  
  Heatmaps are now converted to `ggplot` objects using `ggplotify`, enhancing flexibility and uniformity. Now all plots are ggplot objects.

- **Enhanced Vignettes:**  
  Vignettes have been streamlined by suppressing unnecessary outputs and messages.

- **Bug Fixes & Warnings:**  
  Addressed several bugs and added informative warnings and error handling to guide users when potential issues arise.

- **Preprocess Module Removal:**  
  The preprocess module was removed due to redundancy and limited functionality.

## Utility Functions
- **New Features:**  
  - `hd_initialize()`: Initializes a base object used across all functions for simplified and consistent workflows.  
  - `hd_detect_vartype()`: Identifies if a variable is categorical or continuous.
  - `hd_bin_columns()`: Bins continuous variables into categories.
  - `hd_filter_by_sex()`: Filters data based on sex and updates the data and metadata automatically
  - `hd_log_transform()`: Log-transforms data.
  - `hd_long_data()`: Converts wide data to long format.
  
- **Simplified Usage:**  
  - `hd_save_data()` (formerly `save_df()`) now has more user-friendly arguments, aligning with `hd_import_data()` (formerly `import_df()`).
  - `hd_widen_data()` supports custom column selection for wide data creation with arguments like `exclude`, `names_from`, and `values_from`.

- **Deprecated Functions:**  
  `generate_df()` was deprecated as it is now redundant.

## Dimensionality Reduction
- **Bug Fixes:**  
  Fixed an issue in `hd_auto_pca()` (formerly `do_pca()`) when handling more than 9 principal components.

## Palettes & Themes
- **New Functions:**  
  - `hd_show_palettes()`: Displays all available palettes in the package.
  - Added selected palettes from the `ggsci` package.

## Imputation
- **New Features:**  
  - `hd_na_search()`: Summarizes NA distributions in heatmaps and supports user-defined metadata annotations.  
  - `hd_omit_na()`: Removes rows with NA values based on specified columns.

- **Removed Features:**  
  `impute_mice()` was removed for simplicity. Users requiring advanced imputation can directly use the `mice` package.

## Quality Control (QC) Summary
- **New & Enhanced Features:**  
  - Added a `cor_method` argument to `qc_summary_data()` for customizable correlation methods.  
  - Merged `qc_summary_data()` and `qc_summary_metadata()` into a single function, `hd_qc_summary()`, which now visualizes all metadata columns.

- **Removed Normality Checks:**  
  Removed automatic normality checks for large datasets due to sensitivity issues. Instead, users are encouraged to use histograms or QQ plots for this purpose.

## Differential Expression Analysis
- **Streamlined Functions:**  
  - Merged `do_limma()` and `do_limma_continuous()` into `hd_de_limma()`, which auto-detects variable types.  
  - `hd_plot_volcano()` is now a standalone function to reduce the number of arguments but still keep the flexibility.

- **Improved Customization:**  
  Added a `user_defined_proteins` argument to `hd_plot_volcano()`, allowing users to label specific proteins on volcano plots.

## Classification Models
- **Model Updates:**  
  Merged multiclass and binary models.
  
- **Enhanced Visualizations:**  
  - Multiclassification models now include a variable importance plot.  
  - AUC bar plots were removed to make the output more consistent with the binary classification models.
  - Probability plots were added to visualize the distribution of probabilities for each class.

## Visualization Functions
- **New Functions:**  
  - Added `plot_feature_summary_heatmap()`: Summarizes differential expression and classification model results in a single heatmap.
  - Added `plot_feature_summary_network()`: Summarizes differential expression or classification model features results in a single network.

- **Bug Fixes & Improvements:**  
  Fixed color-matching issues in bar plots for `hd_plot_de_summary()` and `hd_plot_model_summary()` caused by frequency ties. Bars are now colored correctly.  

## Pathway Enrichment Analysis
- **Streamlined Outputs:**  
  Removed unnecessary returns from `hd_plot_gsea()`.

## Clustering
- **New Features:**  
  - Added `hd_cluster_samples()`: Clusters samples based on selected features in k clusters. The number k is either user-defined or determined using the gap statistic.
  - Added `hd_assess_clusters()`: Assesses the quality of clustering using the cluster's Jaccard index and sample size.
  
## New Analysis Module: WGCNA
Added the Weighted Gene Co-expression Network Analysis (WGCNA) module for network analysis.
- **Module Functions:**  
  - `hd_wgcna()`: Performs WGCNA analysis on the data.
  - `hd_plot_wgcna()`: Visualizes the WGCNA results.
  
---

# HDAnalyzeR 1.0.0 (2024-08-19)

Initial release of HDAnalyzeR.
