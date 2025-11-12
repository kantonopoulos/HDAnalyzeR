# Package index

## Generic Utilities

Functions that help you perform general tasks.

- [`hd_initialize()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_initialize.md)
  : Initialize HDAnalyzeR object
- [`hd_save_path()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_save_path.md)
  : Create directory to save results
- [`hd_save_data()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_save_data.md)
  : Save tibble or R object
- [`hd_import_data()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_import_data.md)
  : Import data from file
- [`hd_widen_data()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_widen_data.md)
  : Convert omics data to wide format
- [`hd_long_data()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_long_data.md)
  : Convert omics data to long format
- [`hd_bin_columns()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_bin_columns.md)
  : Bin variables
- [`hd_detect_vartype()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_detect_vartype.md)
  : Detect variable type
- [`hd_filter()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_filter.md)
  : Filter an HD object based on a variable
- [`hd_log_transform()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_log_transform.md)
  : Log transform data with base 2

## Data Preprocessing & Quality Control

### Data Normalization and Imputation

Functions that perform normalizaztion, remove batch effects and impute
missing values.

- [`hd_normalize()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_normalize.md)
  : Normalize data and remove batch effects
- [`hd_omit_na()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_omit_na.md)
  : Omit missing values
- [`hd_impute_median()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_impute_median.md)
  : Impute via Median
- [`hd_impute_knn()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_impute_knn.md)
  : Impute via k-nearest neighbors
- [`hd_impute_missForest()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_impute_missForest.md)
  : Impute via missForest

### Quality Control

Functions that perform automated data quality control check.

- [`hd_na_search()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_na_search.md)
  : Heatmap summary of missing values
- [`hd_qc_summary()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_qc_summary.md)
  : Summarize quality control information

### Correlation and Clustering

Functions that perform protein-protein correlation and can order data
based on selected clustering method.

- [`hd_correlate()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_correlate.md)
  : Correlate data
- [`hd_plot_cor_heatmap()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_plot_cor_heatmap.md)
  : Plot correlation heatmap
- [`hd_cluster()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_cluster.md)
  : Cluster data
- [`hd_cluster_samples()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_cluster_samples.md)
  : Cluster samples in k clusters
- [`hd_assess_clusters()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_assess_clusters.md)
  : Assess clusters

### Dimensionality Reduction

Functions that perform dimensionality reduction.

- [`hd_pca()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_pca.md)
  : Run PCA analysis
- [`hd_umap()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_umap.md)
  : Run UMAP analysis
- [`hd_plot_dim()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_plot_dim.md)
  : Plot PCA or UMAP results on a 2D plane
- [`hd_plot_pca_loadings()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_plot_pca_loadings.md)
  : Plot PCA loadings
- [`hd_plot_pca_variance()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_plot_pca_variance.md)
  : Plot PCA variance
- [`hd_auto_pca()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_auto_pca.md)
  : Run PCA analysis and plot the results
- [`hd_auto_umap()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_auto_umap.md)
  : Run UMAP analysis and plot the results

## Main Proteomics Analysis

### Protein Co-expression Network Analysis (WGCNA)

Functions that perform weighted gene co-expression network analysis.

- [`hd_wgcna()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_wgcna.md)
  : Weighted gene co-expression network analysis
- [`hd_plot_wgcna()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_plot_wgcna.md)
  : Plot WGCNA results

### Differential Expression Analysis

Functions that perform differential expression analysis.

- [`hd_de_limma()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_de_limma.md)
  : Differential expression analysis with limma
- [`hd_de_ttest()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_de_ttest.md)
  : Differential expression analysis with t-test
- [`hd_plot_volcano()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_plot_volcano.md)
  : Visualize differential expression results

### Classification Models

Functions that run machine learning classification model pipelines.

- [`hd_split_data()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_split_data.md)
  : Split data
- [`hd_model_lr()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_model_lr.md)
  : Logistic regression model pipeline
- [`hd_model_rreg()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_model_rreg.md)
  : Regularized regression model pipeline
- [`hd_model_rf()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_model_rf.md)
  : Random forest model pipeline
- [`hd_model_test()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_model_test.md)
  : Validate model on new data

### Summary Visualizations

Functions that visually summarize the results.

- [`hd_plot_de_summary()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_plot_de_summary.md)
  : Summarize differential expression results
- [`hd_plot_model_summary()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_plot_model_summary.md)
  : Summarize model features
- [`hd_plot_feature_heatmap()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_plot_feature_heatmap.md)
  : Summary the combined DE and model results

### Other Visualizations

Functions that visualize data.

- [`hd_plot_regression()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_plot_regression.md)
  : Regression plot
- [`hd_plot_feature_boxplot()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_plot_feature_boxplot.md)
  : Feature boxplots
- [`hd_plot_feature_network()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_plot_feature_network.md)
  : Feature network

## Post Analysis

Functions that analyze further the potential biormarkers and their
biological impact.

- [`hd_ora()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_ora.md)
  : Over-representation analysis
- [`hd_plot_ora()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_plot_ora.md)
  : Plot over-representation analysis results
- [`hd_show_backgrounds()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_show_backgrounds.md)
  : Show available background lists
- [`hd_gsea()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_gsea.md)
  : Gene set enrichment analysis
- [`hd_plot_gsea()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_plot_gsea.md)
  : Plot gene set enrichment analysis results
- [`hd_literature_search()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_literature_search.md)
  : Fetch PubMed articles for disease-protein biomarker associations

## Palettes and Themes

Functions that customize the appearance of your plots.

- [`hd_show_palettes()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_show_palettes.md)
  : Display available palettes
- [`hd_palettes()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_palettes.md)
  : HDAnalyzeR palettes
- [`scale_color_hd()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/scale_color_hd.md)
  : HDAnalyzeR color scales
- [`scale_fill_hd()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/scale_fill_hd.md)
  : HDAnalyzeR fill scales
- [`theme_hd()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/theme_hd.md)
  : HDAnalyzeR theme

## Built in datasets

- [`example_data`](https://kantonopoulos.github.io/HDAnalyzeR/reference/example_data.md)
  : Cancer cohort Olink data
- [`example_metadata`](https://kantonopoulos.github.io/HDAnalyzeR/reference/example_metadata.md)
  : Cancer cohort metadata
