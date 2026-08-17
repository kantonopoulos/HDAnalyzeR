# Changelog

## HDAnalyzeR 1.1.0

A maintenance release focused on removing dependencies that no longer
exist, getting continuous integration green again, and fixing the bugs
that a rewritten test suite uncovered.

### Breaking changes

- **`multiROC` and `vip` have been removed.** Both were archived from
  CRAN (`multiROC` on 2026-05-04, `vip` on 2026-07-08), which made the
  package uninstallable from a clean library. Their functionality is now
  implemented in-package:

  - Multiclass ROC AUC is computed with `yardstick`. The per-class
    one-vs-rest AUCs and the `micro` average are numerically identical
    to what `multiROC` returned. The `macro` value now uses the standard
    definition, the unweighted mean of the per-class AUCs, rather than
    `multiROC`’s curve-averaged variant, so it may differ slightly (by
    up to ~0.02 on small datasets).
  - Variable importance for the random forest and logistic regression
    engines is read directly from the fitted model and matches
    `vip::vi()` exactly.

- **Many packages moved from `Imports` to `Suggests`.** Together with
  the removals below the package now installs with 28 hard dependencies
  instead of

  54. `arrow`, `cluster`, `clusterProfiler`, `easyPubMed`, `embed`,
      `enrichplot`, `fpc`, `missForest`, `ppsr`, `readxl`, `WGCNA` and
      `writexl` are only needed by the specific functions that use them,
      and those functions now raise a clear error naming the package and
      the exact install command when it is missing. Install everything
      with:

  ``` r

  # CRAN
  install.packages(c("arrow", "cluster", "easyPubMed", "embed", "fpc",
                     "missForest", "ppsr", "readxl", "WGCNA", "writexl"))
  # Bioconductor
  BiocManager::install(c("clusterProfiler", "enrichplot", "org.Hs.eg.db", "ReactomePA"))
  ```

- **`purrr`, `tidyselect` and `umap` have been dropped entirely.**
  `umap` was never actually used:
  [`hd_umap()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_umap.md)
  runs through
  [`embed::step_umap()`](https://embed.tidymodels.org/reference/step_umap.html),
  which calls `uwot`. The check for `umap` in
  [`hd_umap()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_umap.md)
  has been replaced with a check for `embed`. `knitr`, `patchwork` and
  `viridis` moved from `Imports` to `Suggests`; they are only used by
  the vignettes.

- **`broom`, `forcats`, `readr`, `scales`, `stringr` and `tidytext` have
  been dropped.** Each was pulled in for one or two small functions,
  which are now implemented in `R/compat.R` or taken from a package that
  was already a hard dependency. Together with their own dependencies
  this removes 15 packages from a clean install (`backports`, `bit`,
  `bit64`, `broom`, `clipr`, `crayon`, `forcats`, `hms`, `janeaustenr`,
  `progress`, `readr`, `SnowballC`, `tidytext`, `tokenizers`, `vroom`),
  taking the install closure from 147 to 132 packages. There is no
  user-visible change in behaviour:

  - [`broom::tidy()`](https://generics.r-lib.org/reference/tidy.html)
    was only ever dispatching to methods registered by `parsnip` and
    `recipes`, both already imported, so the calls now go through those.
  - [`readr::read_csv()`](https://readr.tidyverse.org/reference/read_delim.html)/`read_tsv()`
    in
    [`hd_import_data()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_import_data.md)
    are replaced with
    [`utils::read.table()`](https://rdrr.io/r/utils/read.table.html),
    configured to keep readr’s semantics: feature names such as `IL-6`
    are not mangled, blank fields read as `NA`, and whole numbers read
    as double rather than integer.
  - [`forcats::fct_reorder()`](https://forcats.tidyverse.org/reference/fct_reorder.html),
    [`tidytext::reorder_within()`](https://juliasilge.github.io/tidytext/reference/reorder_within.html),
    [`tidytext::scale_x_reordered()`](https://juliasilge.github.io/tidytext/reference/reorder_within.html),
    [`tidytext::scale_y_reordered()`](https://juliasilge.github.io/tidytext/reference/reorder_within.html)
    and
    [`stringr::str_wrap()`](https://stringr.tidyverse.org/reference/str_wrap.html)
    have base-R equivalents that produce identical output.
  - [`scales::hue_pal()`](https://scales.r-lib.org/reference/pal_hue.html)
    is replaced with
    [`grDevices::hcl()`](https://rdrr.io/r/grDevices/hcl.html). The
    palettes are identical up to 15 colours; beyond that one channel of
    one colour can differ by 1/255 because `scales` rounds via `farver`.

- `rlang` and `withr` stay in `Imports` even though only
  [`rlang::sym()`](https://rlang.r-lib.org/reference/sym.html)/[`syms()`](https://rlang.r-lib.org/reference/sym.html)
  and
  [`withr::local_seed()`](https://withr.r-lib.org/reference/with_seed.html)
  are used. Both remain in the install closure via `dplyr` and `ggplot2`
  regardless, so replacing them would churn hundreds of call sites
  without removing a single package.

- `glmnet` and `ranger` stay in `Imports`. They are the engines
  [`hd_model_rreg()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_model_rreg.md)
  and
  [`hd_model_rf()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_model_rf.md)
  fit through, so the package cannot work without them even though it
  never calls them directly.

- [`extract_protein_list()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/extract_protein_list.md)
  (internal) no longer takes an `UpSetR` membership matrix; it derives
  the combinations from the feature lists directly.

### Bug fixes

#### Differential expression

- [`hd_de_limma()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_de_limma.md)
  no longer fails with
  `length of 'dimnames' [2] not equal to array extent` when `correct`
  contains a categorical variable with more than two levels. Only the
  two group columns of the design matrix are renamed now.
- [`hd_de_ttest()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_de_ttest.md)
  returned `CI.L`, `CI.R` and `t` as character strings, because each row
  was assembled with [`c()`](https://rdrr.io/r/base/c.html) across mixed
  types. All statistics are numeric again.
- [`hd_plot_de_summary()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_plot_de_summary.md)
  labelled every feature `"up"` in both `proteins_df_up` and
  `proteins_df_down`. The direction is now passed explicitly.
- [`hd_plot_de_summary()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_plot_de_summary.md)
  and
  [`hd_plot_model_summary()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_plot_model_summary.md)
  no longer error when summarising a single analysis.
  [`UpSetR::fromList()`](https://rdrr.io/pkg/UpSetR/man/fromList.html)
  degenerates to a named vector for one set; the UpSet plot is now
  skipped with a message and returned as `NULL` when fewer than two
  groups have features.

#### Dimensionality reduction

- `hd_plot_dim(plot_loadings = ...)` drew every loading arrow on the
  diagonal, because the tip used the same loading value for both `xend`
  and `yend`. Arrows now run from the origin to the feature’s `(x, y)`
  loadings, and carry an arrowhead.
- [`hd_pca()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_pca.md)
  returned loadings for every component
  [`prcomp()`](https://rdrr.io/r/stats/prcomp.html) produced rather than
  the `components` that were requested, so `pca_loadings` disagreed with
  `pca_res` and `pca_variance`.
- The point layer used an invalid `Color` label, which made ggplot2 emit
  `Ignoring unknown labels`.
  [`hd_plot_model_summary()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_plot_model_summary.md)
  had the same problem: its metrics bar plot labelled `color` while
  mapping `fill`, so the legend title was dropped.

#### Clustering

- `hd_cluster(normalize = TRUE)` left the first feature completely
  unscaled and discarded the sample identifiers. It moved the sample ID
  into row names before calling
  [`hd_normalize()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_normalize.md),
  which then treated the first *feature* as the ID column. Normalisation
  now happens before the row names are set.
- [`hd_cluster()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_cluster.md)
  returned the sample ID column as a factor; it is a character vector
  again.
- [`hd_assess_clusters()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_assess_clusters.md)
  produced duplicated rows in `cluster_assessment` whenever two clusters
  had the same size and stability, because the old and new cluster
  labels were matched on `(n, Mean_ji)`. They are matched on the
  original cluster label now.

#### Enrichment

- `hd_gsea(ranked_by = "both")` computed the combined logFC /
  significance score and then ranked by `adj.P.Val` anyway. It now ranks
  by the score it computed.
- [`hd_gsea()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_gsea.md)
  renamed the ranked gene vector to ENTREZ identifiers by position.
  Because symbol mapping drops unmappable genes, every score after the
  first failure was attached to the wrong gene. Scores are now matched
  by symbol.
- **[`hd_gsea()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_gsea.md)
  is reproducible.** GSEA p-values come from a permutation test, so
  every call returned a slightly different set of enriched terms. A new
  `seed` argument (123 by default, `NULL` to opt out) fixes the random
  number generator for the duration of the call.
- **[`hd_ora()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_ora.md)
  and
  [`hd_gsea()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_gsea.md)
  no longer stop when nothing is enriched.** They warn and return the
  empty enrichment object instead. A null result is a legitimate
  outcome, and aborting on it broke the vignettes and the examples in CI
  whenever the permutation test or a Bioconductor annotation update
  happened to push everything above the threshold.
  [`hd_plot_ora()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_plot_ora.md)
  and
  [`hd_plot_gsea()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_plot_gsea.md)
  warn and return their input unchanged in that case, rather than
  failing inside `clusterProfiler`.
- [`hd_ora()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_ora.md)
  tested for missing adjusted p-values with `is.na(any(p.adjust))`,
  which coerces the numbers to logicals and never detects anything. An
  all-`NA` column now reports that nothing was enriched instead of
  failing with `missing value where TRUE/FALSE needed`.
- The returned `hd_enrichment` object carries the `pval_lim` it was
  built with, so the plotting functions know which terms were considered
  significant.

#### Co-expression networks

- [`hd_wgcna()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_wgcna.md)
  passed `NA` straight to
  [`WGCNA::blockwiseModules()`](https://rdrr.io/pkg/WGCNA/man/blockwiseModules.html)
  when `pickSoftThreshold()` found no power meeting the scale-free
  topology cut-off. It now falls back to the WGCNA default power of 6
  and says so.
- [`hd_plot_wgcna()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_plot_wgcna.md)
  errored when `clinical_vars` contained only continuous variables.
- [`hd_plot_wgcna()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_plot_wgcna.md)
  bound the module eigengenes and the metadata side by side, silently
  pairing modules with the wrong samples whenever the metadata rows were
  in a different order. They are joined on the sample ID now.

#### Models

- [`hd_model_rf()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_model_rf.md)
  failed with
  `The 'range' lower bound (2) must not exceed upper bound (1)` on
  datasets with fewer than about nine predictors, because `mtry` was
  tuned over `c(floor(sqrt(p)), floor(p / 3))`. The range is clamped
  now.

#### Utilities and reporting

- [`hd_import_data()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_import_data.md)
  returned one of the function’s own local variables instead of the
  stored object when reading `.rda` files, because it used `ls()[1]`. It
  uses the names returned by
  [`load()`](https://rdrr.io/r/base/load.html) now.
- [`hd_save_data()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_save_data.md)
  corrupted values containing a double-quote when writing TSV.
  [`utils::write.table()`](https://rdrr.io/r/utils/write.table.html)
  defaults to escaping an embedded quote as `\"`, which no reader parses
  back, so `he said "hi"` reimported as `he said \hi\`. Quotes are
  doubled now, the way
  [`utils::write.csv()`](https://rdrr.io/r/utils/write.table.html)
  already did for CSV.
- [`hd_qc_summary()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_qc_summary.md),
  [`hd_impute_median()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_impute_median.md),
  [`hd_impute_knn()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_impute_knn.md)
  and
  [`hd_impute_missForest()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_impute_missForest.md)
  printed tables as deparsed R code (`c("f2", "f1")c(1, 1)`). Tables are
  rendered the way they print at the console, truncated to the first ten
  rows.
- [`hd_filter()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_filter.md)
  and
  [`hd_save_path()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_save_path.md)
  produced messages with no spaces between the words and the values
  (`Rows remaining:3`, `Directoryoutalready exists.`).
- [`hd_normalize()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_normalize.md)
  left `scaled:center` and `scaled:scale` attributes on every column of
  the returned tibble.

### Performance

[`hd_qc_summary()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_qc_summary.md)
ran out of memory on large datasets (10,000 features, 5,000 samples).
The results are unchanged on every input; only the way they are computed
differs.

- **The correlation heatmap is capped.**
  [`hd_plot_cor_heatmap()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_plot_cor_heatmap.md)
  and
  [`hd_qc_summary()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_qc_summary.md)
  gained `max_heatmap_features` (default 1000). Above the limit the
  correlation matrix and the reported pairs are still returned, but
  `cor_heatmap` is `NULL` and a warning explains why. Clustering 10,000
  features needs a 400 MB distance matrix and hours of `hclust`, for a
  plot with 100 million unreadable cells.
- **The high-correlation pairs are read straight off the matrix.** The
  pairs used to be found by reshaping the whole correlation matrix to
  long format first, which is one row per entry: 100 million rows before
  any filtering, several GB. The new
  [`cor_pairs_above()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/cor_pairs_above.md)
  scans blocks of columns and keeps only what exceeds the threshold, in
  the same order as before.
- **[`calc_na_percentage_row()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/calc_na_percentage_row.md)
  no longer uses
  [`rowwise()`](https://dplyr.tidyverse.org/reference/rowwise.html)**,
  which evaluated once per sample. Counting with
  [`rowSums()`](https://rdrr.io/r/base/colSums.html) over blocks of
  columns is roughly 2000 times faster on a 1,000 × 3,000 dataset (21 s
  to 0.01 s) and uses a bounded amount of memory.
- **[`calc_na_percentage_col()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/calc_na_percentage_col.md)**
  counts a column at a time instead of building a one-row, 10,000-column
  summary and pivoting it.
- **[`hd_correlate()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_correlate.md)**
  substitutes `use = "everything"` for `"pairwise.complete.obs"` when
  the input has no missing values. The two are equivalent in that case,
  and the pairwise code path is about three times slower because it
  compares every pair of columns separately.
- **[`check_numeric_columns()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/check_numeric_columns.md)**
  no longer coerces already-numeric columns with
  [`as.numeric()`](https://rdrr.io/r/base/numeric.html), which allocated
  a copy of every column of the dataset.
- [`hd_qc_summary()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_qc_summary.md)
  now says up front when a correlation is going to take a while.

### Continuous integration

- Fixed `.Rbuildignore`: the patterns for `inst/extdata`,
  `inst/cheatsheet` and `inst/hdanalyzer_app` used `\e`, `\c` and `\h`
  instead of `/`, so they never matched. The source tarball was **49
  MB**; it is now **604 KB**.
- `R-CMD-check` also runs on `dev/**` and `ka/**` branches and on
  `workflow_dispatch`, not only on `main`. Pushes to working branches
  were never building.
- Added `bioc-version` to the R setup so the Bioconductor dependencies
  resolve reliably, and `oldrel-1` to the test matrix.
- The R-devel job is now `continue-on-error`. Bioconductor routinely
  lags R-devel for weeks after a release, which used to fail the whole
  matrix for reasons unrelated to this package.
- Added a job timeout and `concurrency` cancellation so superseded runs
  stop instead of queueing.
- Fixed a malformed `Suggests` field in `DESCRIPTION` (a trailing comma)
  and a redirecting URL.
- The
  [`hd_literature_search()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_literature_search.md)
  example queried PubMed live during `R CMD check`, so a throttled
  request could stall the examples step on every platform. It is wrapped
  in `\donttest{}` now.
- The
  [`hd_gsea()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_gsea.md)
  examples and the `post_analysis` vignette failed the examples and
  pkgdown steps with `No significant terms found`. GSEA is a permutation
  test, so whether anything cleared the threshold varied between
  machines and between runs.
  [`hd_gsea()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_gsea.md)
  is seeded and no longer treats an empty result as an error (see
  *Enrichment* above).
- Every vignette declared a `\VignetteIndexEntry{}` that did not match
  its YAML title, so each build printed a warning about it. The index
  entries now carry the real titles.
- **Examples that need a `Suggests` package are now conditional.**
  Twelve help topics ran examples that call into `clusterProfiler`,
  `org.Hs.eg.db`, `enrichplot`, `WGCNA`, `ppsr`, `missForest`,
  `cluster`, `fpc`, `embed` or `easyPubMed` unconditionally. Suggested
  packages are best-effort on CI — the Windows runner had no usable
  `clusterProfiler`, and `R CMD check` then failed with
  `The 'clusterProfiler' package is required ... but is not installed`
  while macOS and Ubuntu passed. Each of those topics now carries an
  `@examplesIf requireNamespace(...)` guard, so the examples run where
  the package is available and are skipped where it is not.
- **The pkgdown workflow survives, and reports, a crash in the article
  subprocess.** pkgdown renders each article with
  [`callr::r_safe()`](https://callr.r-lib.org/reference/r.html) and
  polls the subprocess every 200 ms until it exits. The site build died
  twice while polling the render of `classification.Rmd`, the longest
  article, inside processx’s own internals — `chain_call()` on one run,
  `assert_that()` on the next. pkgdown cannot format that condition
  because it carries no `$stderr`, so it reported
  `subscript out of bounds` from `wrap_rmarkdown_error()` and the real
  cause never reached the log. This is not in the package: every article
  renders cleanly in-process on the same runner, which has around 14 GB
  of memory and 79 GB of disk free. The workflow now:
  - installs the newest `callr` and `processx` rather than whichever
    version is cached, since that is where the crash happens;
  - builds with `quiet = FALSE`, so the subprocess output is streamed
    into the log and a genuine article error stays readable;
  - retries once with `clean = FALSE, lazy = TRUE` on failure, which
    resumes from the articles that already rendered instead of starting
    over;
  - reports the runner’s memory and disk, and re-renders every article
    in-process, both only when the build fails.
- Added build and check artefacts (`*.Rcheck/`, `*.tar.gz`,
  `Rplots.pdf`) to `.gitignore`.

### Testing

- The test suite has been rewritten. It now checks behaviour and numeric
  results rather than only object shapes: differential expression is
  verified against
  [`stats::t.test`](https://rdrr.io/r/stats/t.test.html), imputation and
  normalisation against hand-computed values, multiclass AUC against a
  one-vs-rest computation, and variable importance against the model
  coefficients.
- Added end-to-end tests covering the full workflows on the shipped
  example data: QC to imputation to normalisation to PCA to differential
  expression to classification, plus the multi-class and multi-model
  summary paths.
- Tests for optional packages are skipped rather than failed when the
  package is not installed.
- The PubMed tests are opt-in. NCBI throttles unauthenticated clients
  and a throttled request can stall for minutes, which is exactly the
  kind of hang that makes CI look broken. Run them with
  `HDANALYZER_TEST_PUBMED=true`.
- The end-to-end tests run on a five-disease, forty-assay subset of the
  example data. They cover the same code paths as the full dataset but
  the whole suite finishes in about a minute, so it is cheap enough to
  run on every push.

The suite is 15 files, 229 `test_that()` blocks and 608 assertions,
passing with no failures.

`R CMD check --as-cran` now completes with **0 errors and 0 warnings**.
Two NOTEs remain and neither is a defect: “New submission” (the package
is not on CRAN) and a handful of examples running longer than five
seconds, which is inherent to fitting real models on the example data.

### Internal

- Replaced superseded verbs
  ([`summarise_all()`](https://dplyr.tidyverse.org/reference/summarise_all.html),
  `gather()`, `spread()`,
  [`top_n()`](https://dplyr.tidyverse.org/reference/top_n.html),
  [`sample_n()`](https://dplyr.tidyverse.org/reference/sample_n.html),
  [`rename_all()`](https://dplyr.tidyverse.org/reference/select_all.html))
  with their current tidyverse equivalents.
- Fixed `tidyselect` “external vector in selections” deprecation
  warnings in
  [`hd_split_data()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_split_data.md)
  and the model tuning helpers.
- Replaced the deprecated `size` aesthetic with `linewidth` in
  [`hd_show_palettes()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_show_palettes.md).
- Removed 16 assigned-but-unused
  [`check_numeric_columns()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/check_numeric_columns.md)
  results and other dead code.
- Extracted
  [`multiclass_auc()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/multiclass_auc.md),
  [`model_importance()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/model_importance.md),
  [`rank_features()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/rank_features.md),
  [`rename_ranking_to_entrezid()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/rename_ranking_to_entrezid.md),
  [`build_upset_plot()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/build_upset_plot.md),
  [`message_table()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/message_table.md)
  and
  [`check_installed()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/check_installed.md)
  as documented internal helpers, which makes the previously untestable
  logic directly testable.

## HDAnalyzeR 1.0.1 (development)

### Changes

- In multi-classification models the feature importance is now returned
  for each class separately.

### Bugs

- Fix bug in hd_filter().

## HDAnalyzeR 1.0.0 (2025-11-17)

Release of HDAnalyzeR.
