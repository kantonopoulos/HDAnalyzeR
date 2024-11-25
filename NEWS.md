# HDAnalyzeR 2.0.0 

## General
- Changed function nomenclature to be more consistent, easier to understand and to search for. 
All functions now start with `hd_` which is convenient for searching in RStudio especially when 
working together with other packages.
- Convert all heatmaps from `tidyheatmaps` to ggplot objects with `ggplotify`.
- Make vignettes more readable by suppressing non necessary results and messages.
- Solve bugs and issues.

## Utils
- Added `hd_initialize()` to create an initial object that is used in all other functions. This is making the package usage much more straightforward and concise.
- Added `hd_detect_vartype()` to detect if a variable is categorical or continuous.
- Simplified the arguments in `hd_save_data()` (former `save_df()`) to make it more user friendly and similar to `hd_import_data()` (former `import_df()`).
- Made `hd_widen_data()` more flexible to allow for user defined columns to be used for wide data generation with `exclude`, `names_from` and `values_from` arguments.
- `generate_df()` was deprecated as it is considered redundant.

## Dimensionality Reduction
- Fix bug with `hd_auto_pca()` (former `do_pca()`) and PC component numbers above 9.

## Palettes & Themes
- Added `hd_show_palettes()` to display all available palettes in the package.
- Added selected palettes from ggsci package.

## Imputation
- Add `hd_na_search()` that summarizes the distribution of NA values in a heatmap. It also makes it able to annotate by user defined metadata variables.
- Removed `impute_mice()` function as it is considered too complicated for the package. If needed, the user can use the `mice` package directly.

## QC Summary
- Add `cor_method` argument to `qc_summary_data()` to allow the user to choose the correlation method.
- Remove normality check as it is invalid for large datasets. The tests are very sensitive and will always return significant results. If the user still wants to check normality we recommend histograms or QQ plots.
- Merge `qc_summary_data()` and `qc_summary_metadata()` into one function `hd_qc_summary()`.
- Add `hd_qc_summary()` visualizes all metadata columns instead of requiring manual selection.

## Classification Models
- Multiclassification models now return variable importance plot as well.
- AUC barplot in multiclassification models are now ordered.


- Add `plot_biomarkers_summary_heatmap()` to summarize the results of differential expression analysis and classification models in one summary heatmap.
- Fix issue with color matching with the bars of Upset plots in `plot_de_summary()` and `plot_features_summary()` because of frequency ties. Now the bars are colored correctly even in case of frequency ties. 
- Add verbose argument to `plot_de_summary()` and `plot_features_summary()` to suppress printing of proteins/features sets in different cases.

- Remove non necessary return from `plot_gsea()`.
- Add `user_defined_proteins` argument to `plot_volcano()` and all differential expression functions in order for the user to specify which proteins to label on top of the volcano plot.


# HDAnalyzeR 1.0.0 (2024-08-19)

Initial release of HDAnalyzeR.
