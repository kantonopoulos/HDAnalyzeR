# HDAnalyzeR 1.1.0

A maintenance release focused on removing dependencies that no longer exist,
getting continuous integration green again, and fixing the bugs that a rewritten
test suite uncovered.

## Breaking changes

- **`multiROC` and `vip` have been removed.** Both were archived from CRAN
  (`multiROC` on 2026-05-04, `vip` on 2026-07-08), which made the package
  uninstallable from a clean library. Their functionality is now implemented
  in-package:
  - Multiclass ROC AUC is computed with `yardstick`. The per-class one-vs-rest
    AUCs and the `micro` average are numerically identical to what `multiROC`
    returned. The `macro` value now uses the standard definition, the unweighted
    mean of the per-class AUCs, rather than `multiROC`'s curve-averaged variant,
    so it may differ slightly (by up to ~0.02 on small datasets).
  - Variable importance for the random forest and logistic regression engines is
    read directly from the fitted model and matches `vip::vi()` exactly.

- **Many packages moved from `Imports` to `Suggests`.** The package now installs
  with 34 hard dependencies instead of 54. `arrow`, `cluster`, `clusterProfiler`,
  `easyPubMed`, `embed`, `enrichplot`, `fpc`, `missForest`, `ppsr`, `readxl`,
  `WGCNA` and `writexl` are only needed by the specific functions that use them,
  and those functions now raise a clear error naming the package and the exact
  install command when it is missing. Install everything with:

  ```r
  # CRAN
  install.packages(c("arrow", "cluster", "easyPubMed", "embed", "fpc",
                     "missForest", "ppsr", "readxl", "WGCNA", "writexl"))
  # Bioconductor
  BiocManager::install(c("clusterProfiler", "enrichplot", "org.Hs.eg.db", "ReactomePA"))
  ```

- **`purrr`, `tidyselect` and `umap` have been dropped entirely.** `umap` was
  never actually used: `hd_umap()` runs through `embed::step_umap()`, which calls
  `uwot`. The check for `umap` in `hd_umap()` has been replaced with a check for
  `embed`. `knitr`, `patchwork` and `viridis` moved from `Imports` to `Suggests`;
  they are only used by the vignettes.

- `glmnet` and `ranger` stay in `Imports`. They are the engines `hd_model_rreg()`
  and `hd_model_rf()` fit through, so the package cannot work without them even
  though it never calls them directly.

- `extract_protein_list()` (internal) no longer takes an `UpSetR` membership
  matrix; it derives the combinations from the feature lists directly.

## Bug fixes

### Differential expression

- `hd_de_limma()` no longer fails with `length of 'dimnames' [2] not equal to
  array extent` when `correct` contains a categorical variable with more than two
  levels. Only the two group columns of the design matrix are renamed now.
- `hd_de_ttest()` returned `CI.L`, `CI.R` and `t` as character strings, because
  each row was assembled with `c()` across mixed types. All statistics are
  numeric again.
- `hd_plot_de_summary()` labelled every feature `"up"` in both `proteins_df_up`
  and `proteins_df_down`. The direction is now passed explicitly.
- `hd_plot_de_summary()` and `hd_plot_model_summary()` no longer error when
  summarising a single analysis. `UpSetR::fromList()` degenerates to a named
  vector for one set; the UpSet plot is now skipped with a message and returned
  as `NULL` when fewer than two groups have features.

### Dimensionality reduction

- `hd_plot_dim(plot_loadings = ...)` drew every loading arrow on the diagonal,
  because the tip used the same loading value for both `xend` and `yend`. Arrows
  now run from the origin to the feature's `(x, y)` loadings, and carry an
  arrowhead.
- `hd_pca()` returned loadings for every component `prcomp()` produced rather
  than the `components` that were requested, so `pca_loadings` disagreed with
  `pca_res` and `pca_variance`.
- The point layer used an invalid `Color` label, which made ggplot2 emit
  `Ignoring unknown labels`. `hd_plot_model_summary()` had the same problem: its
  metrics bar plot labelled `color` while mapping `fill`, so the legend title was
  dropped.

### Clustering

- `hd_cluster(normalize = TRUE)` left the first feature completely unscaled and
  discarded the sample identifiers. It moved the sample ID into row names before
  calling `hd_normalize()`, which then treated the first *feature* as the ID
  column. Normalisation now happens before the row names are set.
- `hd_cluster()` returned the sample ID column as a factor; it is a character
  vector again.
- `hd_assess_clusters()` produced duplicated rows in `cluster_assessment`
  whenever two clusters had the same size and stability, because the old and new
  cluster labels were matched on `(n, Mean_ji)`. They are matched on the original
  cluster label now.

### Enrichment

- `hd_gsea(ranked_by = "both")` computed the combined logFC / significance score
  and then ranked by `adj.P.Val` anyway. It now ranks by the score it computed.
- `hd_gsea()` renamed the ranked gene vector to ENTREZ identifiers by position.
  Because symbol mapping drops unmappable genes, every score after the first
  failure was attached to the wrong gene. Scores are now matched by symbol.
- **`hd_gsea()` is reproducible.** GSEA p-values come from a permutation test, so
  every call returned a slightly different set of enriched terms. A new `seed`
  argument (123 by default, `NULL` to opt out) fixes the random number generator
  for the duration of the call.
- **`hd_ora()` and `hd_gsea()` no longer stop when nothing is enriched.** They
  warn and return the empty enrichment object instead. A null result is a
  legitimate outcome, and aborting on it broke the vignettes and the examples in
  CI whenever the permutation test or a Bioconductor annotation update happened
  to push everything above the threshold. `hd_plot_ora()` and `hd_plot_gsea()`
  warn and return their input unchanged in that case, rather than failing inside
  `clusterProfiler`.
- `hd_ora()` tested for missing adjusted p-values with `is.na(any(p.adjust))`,
  which coerces the numbers to logicals and never detects anything. An all-`NA`
  column now reports that nothing was enriched instead of failing with `missing
  value where TRUE/FALSE needed`.
- The returned `hd_enrichment` object carries the `pval_lim` it was built with,
  so the plotting functions know which terms were considered significant.

### Co-expression networks

- `hd_wgcna()` passed `NA` straight to `WGCNA::blockwiseModules()` when
  `pickSoftThreshold()` found no power meeting the scale-free topology cut-off.
  It now falls back to the WGCNA default power of 6 and says so.
- `hd_plot_wgcna()` errored when `clinical_vars` contained only continuous
  variables.
- `hd_plot_wgcna()` bound the module eigengenes and the metadata side by side,
  silently pairing modules with the wrong samples whenever the metadata rows were
  in a different order. They are joined on the sample ID now.

### Models

- `hd_model_rf()` failed with `The 'range' lower bound (2) must not exceed upper
  bound (1)` on datasets with fewer than about nine predictors, because `mtry`
  was tuned over `c(floor(sqrt(p)), floor(p / 3))`. The range is clamped now.

### Utilities and reporting

- `hd_import_data()` returned one of the function's own local variables instead
  of the stored object when reading `.rda` files, because it used `ls()[1]`. It
  uses the names returned by `load()` now.
- `hd_qc_summary()`, `hd_impute_median()`, `hd_impute_knn()` and
  `hd_impute_missForest()` printed tables as deparsed R code
  (`c("f2", "f1")c(1, 1)`). Tables are rendered the way they print at the console,
  truncated to the first ten rows.
- `hd_filter()` and `hd_save_path()` produced messages with no spaces between the
  words and the values (`Rows remaining:3`, `Directoryoutalready exists.`).
- `hd_normalize()` left `scaled:center` and `scaled:scale` attributes on every
  column of the returned tibble.

## Performance

`hd_qc_summary()` ran out of memory on large datasets (10,000 features,
5,000 samples). The results are unchanged on every input; only the way they are
computed differs.

- **The correlation heatmap is capped.** `hd_plot_cor_heatmap()` and
  `hd_qc_summary()` gained `max_heatmap_features` (default 1000). Above the
  limit the correlation matrix and the reported pairs are still returned, but
  `cor_heatmap` is `NULL` and a warning explains why. Clustering 10,000 features
  needs a 400 MB distance matrix and hours of `hclust`, for a plot with 100
  million unreadable cells.
- **The high-correlation pairs are read straight off the matrix.** The pairs used
  to be found by reshaping the whole correlation matrix to long format first,
  which is one row per entry: 100 million rows before any filtering, several GB.
  The new `cor_pairs_above()` scans blocks of columns and keeps only what exceeds
  the threshold, in the same order as before.
- **`calc_na_percentage_row()` no longer uses `rowwise()`**, which evaluated once
  per sample. Counting with `rowSums()` over blocks of columns is roughly 2000
  times faster on a 1,000 × 3,000 dataset (21 s to 0.01 s) and uses a bounded
  amount of memory.
- **`calc_na_percentage_col()`** counts a column at a time instead of building a
  one-row, 10,000-column summary and pivoting it.
- **`hd_correlate()`** substitutes `use = "everything"` for
  `"pairwise.complete.obs"` when the input has no missing values. The two are
  equivalent in that case, and the pairwise code path is about three times slower
  because it compares every pair of columns separately.
- **`check_numeric_columns()`** no longer coerces already-numeric columns with
  `as.numeric()`, which allocated a copy of every column of the dataset.
- `hd_qc_summary()` now says up front when a correlation is going to take a while.

## Continuous integration

- Fixed `.Rbuildignore`: the patterns for `inst/extdata`, `inst/cheatsheet` and
  `inst/hdanalyzer_app` used `\e`, `\c` and `\h` instead of `/`, so they never
  matched. The source tarball was **49 MB**; it is now **604 KB**.
- `R-CMD-check` also runs on `dev/**` and `ka/**` branches and on
  `workflow_dispatch`, not only on `main`. Pushes to working branches were never
  building.
- Added `bioc-version` to the R setup so the Bioconductor dependencies resolve
  reliably, and `oldrel-1` to the test matrix.
- The R-devel job is now `continue-on-error`. Bioconductor routinely lags R-devel
  for weeks after a release, which used to fail the whole matrix for reasons
  unrelated to this package.
- Added a job timeout and `concurrency` cancellation so superseded runs stop
  instead of queueing.
- Fixed a malformed `Suggests` field in `DESCRIPTION` (a trailing comma) and a
  redirecting URL.
- The `hd_literature_search()` example queried PubMed live during
  `R CMD check`, so a throttled request could stall the examples step on every
  platform. It is wrapped in `\donttest{}` now.
- The `hd_gsea()` examples and the `post_analysis` vignette failed the examples
  and pkgdown steps with `No significant terms found`. GSEA is a permutation
  test, so whether anything cleared the threshold varied between machines and
  between runs. `hd_gsea()` is seeded and no longer treats an empty result as an
  error (see *Enrichment* above).
- Every vignette declared a `\VignetteIndexEntry{}` that did not match its YAML
  title, so each build printed a warning about it. The index entries now carry
  the real titles.
- **Examples that need a `Suggests` package are now conditional.** Twelve help
  topics ran examples that call into `clusterProfiler`, `org.Hs.eg.db`,
  `enrichplot`, `WGCNA`, `ppsr`, `missForest`, `cluster`, `fpc`, `embed` or
  `easyPubMed` unconditionally. Suggested packages are best-effort on CI — the
  Windows runner had no usable `clusterProfiler`, and `R CMD check` then failed
  with `The 'clusterProfiler' package is required ... but is not installed`
  while macOS and Ubuntu passed. Each of those topics now carries an
  `@examplesIf requireNamespace(...)` guard, so the examples run where the
  package is available and are skipped where it is not.
- The pkgdown workflow reports the runner's memory and disk, and re-renders the
  articles in-process if the site build fails. pkgdown renders each article in a
  `callr` subprocess; when that subprocess dies without writing to stderr, its
  error formatter fails with `subscript out of bounds` in
  `wrap_rmarkdown_error()` and the real cause never reaches the log. The
  re-render step surfaces the actual error and only runs on failure.
- Added build and check artefacts (`*.Rcheck/`, `*.tar.gz`, `Rplots.pdf`) to
  `.gitignore`.

## Testing

- The test suite has been rewritten. It now checks behaviour and numeric results
  rather than only object shapes: differential expression is verified against
  `stats::t.test`, imputation and normalisation against hand-computed values,
  multiclass AUC against a one-vs-rest computation, and variable importance
  against the model coefficients.
- Added end-to-end tests covering the full workflows on the shipped example data:
  QC to imputation to normalisation to PCA to differential expression to
  classification, plus the multi-class and multi-model summary paths.
- Tests for optional packages are skipped rather than failed when the package is
  not installed.
- The PubMed tests are opt-in. NCBI throttles unauthenticated clients and a
  throttled request can stall for minutes, which is exactly the kind of hang that
  makes CI look broken. Run them with `HDANALYZER_TEST_PUBMED=true`.
- The end-to-end tests run on a five-disease, forty-assay subset of the example
  data. They cover the same code paths as the full dataset but the whole suite
  finishes in about a minute, so it is cheap enough to run on every push.

The suite is 15 files, 229 `test_that()` blocks and 608 assertions, passing with
no failures.

`R CMD check --as-cran` now completes with **0 errors and 0 warnings**. Two NOTEs
remain and neither is a defect: "New submission" (the package is not on CRAN) and
a handful of examples running longer than five seconds, which is inherent to
fitting real models on the example data.

## Internal

- Replaced superseded verbs (`summarise_all()`, `gather()`, `spread()`,
  `top_n()`, `sample_n()`, `rename_all()`) with their current tidyverse
  equivalents.
- Fixed `tidyselect` "external vector in selections" deprecation warnings in
  `hd_split_data()` and the model tuning helpers.
- Replaced the deprecated `size` aesthetic with `linewidth` in
  `hd_show_palettes()`.
- Removed 16 assigned-but-unused `check_numeric_columns()` results and other dead
  code.
- Extracted `multiclass_auc()`, `model_importance()`, `rank_features()`,
  `rename_ranking_to_entrezid()`, `build_upset_plot()`, `message_table()` and
  `check_installed()` as documented internal helpers, which makes the previously
  untestable logic directly testable.

# HDAnalyzeR 1.0.1 (development)

## Changes

- In multi-classification models the feature importance is now returned for each class separately.

## Bugs

- Fix bug in hd_filter().

# HDAnalyzeR 1.0.0 (2025-11-17)

Release of HDAnalyzeR.
