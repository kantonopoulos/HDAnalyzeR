# HDAnalyzeR 2.0.0 

## General
- Changed function nomenclature to be more consistent, easier to understand and to search for. 
All functions now start with `hd_` which is convenient for searching in RStudio especially when 
working together with other packages.

## Utils
- Added `hd_detect_vartype()` to detect if a variable is categorical or continuous.
- Simplified the arguments in `hd_save_data()` (`former save_df()`) to make it more user friendly and similar to `hd_import_data()` (former `import_df()`).
- Made `hd_widen_data()` more flexible to allow for user defined columns to be used for wide data generation with `exclude`, `names_from` and `values_from` arguments.

- Add XGBoost classification model (both binary and multi classification).
- Multiclassification models now return variable importance plot as well.
- AUC barplot in multiclassification models are now ordered.
- Add `na_search()` that summarizes the distribution of Olink dataset NA values in a heatmap. It also makes it able to group the NA values by user defined metadata columns.
- Add `cor_method` argument to `qc_summary_data()` to allow the user to choose the correlation method.
- Add `show_heatmap` argument to `create_corr_heatmap()` to not show heatmap when `report` argument in `qc_summary_data()` is FALSE.
- `generate_df()` was deprecated as it is considered redundant. None of the other functions work directly with joined data and wide data can be produced with `widen_data()`.
- Add `plot_biomarkers_summary_heatmap()` to summarize the results of differential expression analysis and classification models in one summary heatmap.
- Fix issue with color matching with the bars of Upset plots in `plot_de_summary()` and `plot_features_summary()` because of frequency ties. Now the bars are colored correctly even in case of frequency ties. 
- Add verbose argument to `plot_de_summary()` and `plot_features_summary()` to suppress printing of proteins/features sets in different cases.
- Make vignettes more readable by suppressing non necessary results and messages.
- Remove non necessary return from `plot_gsea()`.
- Add `user_defined_proteins` argument to `plot_volcano()` and all differential expression functions in order for the user to specify which proteins to label on top of the volcano plot.
- Fix problem with `plot_dim_reduction()` and PC numbers above 9.

# HDAnalyzeR 1.0.0 (2024-08-19)

Initial release of HDAnalyzeR.
