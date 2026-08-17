# Differential Expression Analysis

This vignette will guide you through the differential expression
analysis of your data. We will load HDAnalyzeR, load the example data
and metadata that come with the package and initialize the HDAnalyzeR
object.

## Loading the Data

``` r

library(HDAnalyzeR)

hd_obj <- hd_initialize(dat = example_data, 
                        metadata = example_metadata, 
                        is_wide = FALSE, 
                        sample_id = "DAid",
                        var_name = "Assay",
                        value_name = "NPX")
```

## Running Differential Expression Analysis with limma

We will start by running a simple differential expression analysis using
the
[`hd_de_limma()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_de_limma.md)
function. In this function we have to state the variable of interest,
the group of this variable that will be the case, as well as the
control(s). We will also correct for both `Sex` and `Age` variables.
After the analysis is done, we will use
[`hd_plot_volcano()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_plot_volcano.md)
to visualize the results.

In the first example, we will run a differential expression analysis for
the `AML` case against the `CLL` control.

``` r

de_results <- hd_de_limma(hd_obj, 
                          variable = "Disease", 
                          case = "AML", 
                          control = "CLL", 
                          correct = c("Age", "Sex")) |> 
  hd_plot_volcano()

head(de_results$de_res)
#> # A tibble: 6 × 10
#>   Feature logFC   CI.L   CI.R AveExpr     t      P.Value adj.P.Val     B Disease
#>   <chr>   <dbl>  <dbl>  <dbl>   <dbl> <dbl>        <dbl>     <dbl> <dbl> <chr>  
#> 1 ADA      1.42  0.955  1.89    1.56   6.04 0.0000000250   2.50e-6  8.74 AML    
#> 2 ADAM8   -1.23 -1.67  -0.795   1.74  -5.59 0.000000193    9.64e-6  6.77 AML    
#> 3 AZU1     1.92  1.20   2.64    0.777  5.30 0.000000668    2.23e-5  5.57 AML    
#> 4 ARID4B  -1.38 -1.91  -0.847   1.85  -5.15 0.00000132     3.30e-5  4.92 AML    
#> 5 ARTN     1.08  0.597  1.55    0.804  4.46 0.0000227      3.84e-4  2.22 AML    
#> 6 ANGPT1  -1.71 -2.47  -0.948   0.992 -4.45 0.0000230      3.84e-4  2.21 AML
de_results$volcano_plot
```

![](differential_expression_files/figure-html/unnamed-chunk-2-1.png)

We are able to state more control groups if we want to. We can also
change the correction for the variables as well as both the p-value and
logFC significance thresholds.

``` r

de_results <- hd_de_limma(hd_obj, 
                              case = "AML", 
                              control = c("CLL", "MYEL", "GLIOM"), 
                              correct = "BMI") |> 
  hd_plot_volcano(pval_lim = 0.01, logfc_lim  = 1)

de_results$volcano_plot
```

![](differential_expression_files/figure-html/unnamed-chunk-3-1.png)

If we do not set a control group, the function will compare the case
group against all other groups.

``` r

de_results <- hd_de_limma(hd_obj, case = "AML", correct = c("Age", "Sex")) |> 
  hd_plot_volcano()

de_results$volcano_plot
```

![](differential_expression_files/figure-html/unnamed-chunk-4-1.png)

## Customizing the Volcano Plot

We can customize the volcano plot further by adding a title and not
displaying the number of significant proteins. We can also change the
number of significant proteins that will be displayed with their names
in the plot.

``` r

de_results <- hd_de_limma(hd_obj, case = "AML", correct = c("Age", "Sex")) |> 
  hd_plot_volcano(report_nproteins = FALSE, 
                  title = "AML vs all other groups",
                  top_up_prot = 3,
                  top_down_prot = 1)

de_results$volcano_plot
```

![](differential_expression_files/figure-html/unnamed-chunk-5-1.png)

## Running Differential Expression Analysis with t-test

Let’s move to another method. We will use the
[`hd_de_ttest()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_de_ttest.md)
that performs a t-test for each variable. This function takes similar
inputs with
[`hd_de_limma()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_de_limma.md)
but it cannot correct for other variables like `Sex` and `Age`.

``` r

de_results <- hd_de_ttest(hd_obj, case = "AML") |> 
  hd_plot_volcano()

de_results$volcano_plot
```

![](differential_expression_files/figure-html/unnamed-chunk-6-1.png)

## The case of Sex-specific Diseases

If we have diseases that are sex specific like Breast Cancer for
example, we should consider run the analysis only with samples of that
sex. We can easily integrate that into our pipeline using the
[`hd_filter()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_filter.md)
function. In that case, we would not be able to correct for sex, as
there will be only one sex “F” (female).

``` r

de_results <- hd_obj |> 
  hd_filter(variable = "Sex", values = "F", flag = "k") |> 
  hd_de_limma(case = "BRC", control = "AML", correct = "Age") |> 
  hd_plot_volcano()
#> Variable Sex is categorical
#> Filtering complete. Rows remaining: 366

de_results$volcano_plot
```

![](differential_expression_files/figure-html/unnamed-chunk-7-1.png)

## Running DE against other Variables

### Other Categorical Variables

We could also run differential expression against another categorical
variable like `Sex` by changing the `variable` argument.

``` r

de_results <- hd_de_limma(hd_obj, variable = "Sex", case = "F", correct = "Age") |> 
  hd_plot_volcano(report_nproteins = FALSE, title = "Sex Comparison")

de_results$volcano_plot
```

![](differential_expression_files/figure-html/unnamed-chunk-8-1.png)

### Continuous Variables

Moreover, we can also perform Differential Expression Analysis against a
continuous variable such as `Age`. This can be done only with
[`hd_de_limma()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_de_limma.md)!
We can also correct for categorical and other continuous variables. In
this case, no `case` or `control` groups are needed.

``` r

de_results <- hd_de_limma(hd_obj, variable = "Age", case = NULL, correct = c("Sex", "BMI")) |> 
  hd_plot_volcano(report_nproteins = FALSE, title = "DE against Age")

de_results$volcano_plot
```

![](differential_expression_files/figure-html/unnamed-chunk-9-1.png)

## Summarizing the Results from Multiple Analysis

As a last step, we can summarize the results via
[`hd_plot_de_summary()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_plot_de_summary.md).
Let’s first run a differential expression analysis for 4 different cases
(1 vs 3).

``` r

res_aml <- hd_de_limma(hd_obj, case = "AML", control = c("CLL", "MYEL", "GLIOM"))

res_cll <- hd_de_limma(hd_obj, case = "CLL", control = c("AML", "MYEL", "GLIOM"))

res_myel <- hd_de_limma(hd_obj, case = "MYEL" , control = c("AML", "CLL", "GLIOM"))

res_gliom <- hd_de_limma(hd_obj, case = "GLIOM" , control = c("AML", "CLL", "MYEL"))

de_summary_res <- hd_plot_de_summary(list("AML" = res_aml, 
                                          "CLL" = res_cll, 
                                          "MYEL" = res_myel, 
                                          "GLIOM" = res_gliom),
                                     class_palette = "cancers12")
```

``` r

de_summary_res$de_barplot
```

![](differential_expression_files/figure-html/unnamed-chunk-11-1.png)

``` r

de_summary_res$upset_plot_up
```

![](differential_expression_files/figure-html/unnamed-chunk-11-2.png)

``` r

de_summary_res$upset_plot_down
```

![](differential_expression_files/figure-html/unnamed-chunk-11-3.png)

> 📓 Remember that these data are a dummy-dataset with artificial data
> and the results in this guide should not be interpreted as real
> results. The purpose of this vignette is to show you how to use the
> package and its functions.

``` r

sessionInfo()
#> R version 4.6.1 (2026-06-24)
#> Platform: x86_64-pc-linux-gnu
#> Running under: Ubuntu 24.04.4 LTS
#> 
#> Matrix products: default
#> BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
#> LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.26.so;  LAPACK version 3.12.0
#> 
#> locale:
#>  [1] LC_CTYPE=C.UTF-8       LC_NUMERIC=C           LC_TIME=C.UTF-8       
#>  [4] LC_COLLATE=C.UTF-8     LC_MONETARY=C.UTF-8    LC_MESSAGES=C.UTF-8   
#>  [7] LC_PAPER=C.UTF-8       LC_NAME=C              LC_ADDRESS=C          
#> [10] LC_TELEPHONE=C         LC_MEASUREMENT=C.UTF-8 LC_IDENTIFICATION=C   
#> 
#> time zone: UTC
#> tzcode source: system (glibc)
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> other attached packages:
#> [1] viridis_0.6.5     viridisLite_0.4.3 patchwork_1.3.2   ggplot2_4.0.3    
#> [5] dplyr_1.2.1       glmnet_5.0        Matrix_1.7-5      HDAnalyzeR_1.1.0 
#> 
#> loaded via a namespace (and not attached):
#>   [1] matrixStats_1.5.0       fs_2.1.0                enrichplot_1.32.0      
#>   [4] fontawesome_0.5.3       lubridate_1.9.5         sparsevctrs_0.3.6      
#>   [7] DiceDesign_1.10         httr_1.4.8              RColorBrewer_1.1-3     
#>  [10] doParallel_1.0.17       prabclus_2.3-5          dynamicTreeCut_1.63-1  
#>  [13] backports_1.5.1         tools_4.6.1             doRNG_1.8.6.3          
#>  [16] utf8_1.2.6              R6_2.6.1                mgcv_1.9-4             
#>  [19] lazyeval_0.2.3          uwot_0.2.4              yardstick_1.4.0        
#>  [22] withr_3.0.3             gridExtra_2.3.1         downlit_0.4.5          
#>  [25] preprocessCore_1.74.0   WGCNA_1.74              cli_3.6.6              
#>  [28] Biobase_2.72.0          textshaping_1.0.5       scatterpie_0.2.6       
#>  [31] labeling_0.4.3          sass_0.4.10             diptest_0.77-2         
#>  [34] S7_0.2.2                robustbase_0.99-7       randomForest_4.7-1.2   
#>  [37] ggridges_0.5.7          tune_2.1.0              askpass_1.2.1          
#>  [40] pkgdown_2.2.1           systemfonts_1.3.2       yulab.utils_0.2.4      
#>  [43] foreign_0.8-91          gson_0.2.1              DOSE_4.6.0             
#>  [46] parallelly_1.48.0       itertools_0.1-3         limma_3.68.5           
#>  [49] impute_1.86.0           rstudioapi_0.19.0       RSQLite_3.53.3         
#>  [52] shape_1.4.6.1           generics_0.1.4          gridGraphics_0.5-1     
#>  [55] GO.db_3.23.1            ggbeeswarm_0.7.3        fansi_1.0.7            
#>  [58] S4Vectors_0.50.1        lifecycle_1.0.5         whisker_0.4.1          
#>  [61] yaml_2.3.12             recipes_1.3.3           qvalue_2.44.0          
#>  [64] grid_4.6.1              blob_1.3.0              crayon_1.5.3           
#>  [67] ggtangle_0.1.2          lattice_0.22-9          KEGGREST_1.52.2        
#>  [70] pillar_1.11.1           knitr_1.51              fpc_2.2-14             
#>  [73] future.apply_1.20.2     codetools_0.2-20        glue_1.8.1             
#>  [76] ggiraph_0.9.6           rsample_1.3.2           ggfun_0.2.1            
#>  [79] fontLiberation_0.1.0    data.table_1.18.4       vctrs_0.7.3            
#>  [82] png_0.1-9               treeio_1.36.1           Rdpack_2.6.6           
#>  [85] gtable_0.3.6            kernlab_0.9-33          cachem_1.1.0           
#>  [88] gower_1.0.2             xfun_0.60               rbibutils_2.4.1        
#>  [91] prodlim_2026.03.11      tidygraph_1.3.1         Seqinfo_1.2.0          
#>  [94] survival_3.8-6          timeDate_4052.112       aisdk_1.4.12           
#>  [97] pheatmap_1.0.13         iterators_1.0.14        hardhat_1.4.3          
#> [100] lava_1.9.2              statmod_1.5.2           ipred_0.9-15           
#> [103] nlme_3.1-169            ggtree_4.2.0            bit64_4.8.2            
#> [106] fontquiver_0.2.1        RcppAnnoy_0.0.23        UpSetR_1.4.1           
#> [109] bslib_0.12.0            vipor_0.4.7             otel_0.2.0             
#> [112] rpart_4.1.27            colorspace_2.1-3        Hmisc_5.2-6            
#> [115] BiocGenerics_0.58.1     DBI_1.3.0               nnet_7.3-20            
#> [118] ppsr_0.0.5              tidyselect_1.2.1        processx_3.9.0         
#> [121] bit_4.6.0               compiler_4.6.1          curl_7.1.0             
#> [124] httr2_1.3.0             htmlTable_2.5.0         xml2_1.6.0             
#> [127] desc_1.4.3              fontBitstreamVera_0.1.1 checkmate_2.3.4        
#> [130] scales_1.4.0            DEoptimR_1.2-0          callr_3.8.0            
#> [133] rappdirs_0.3.4          stringr_1.6.0           digest_0.6.39          
#> [136] rmarkdown_2.31          XVector_0.52.0          base64enc_0.1-6        
#> [139] htmltools_0.5.9         pkgconfig_2.0.3         fastmap_1.2.0          
#> [142] rlang_1.3.0             htmlwidgets_1.6.4       farver_2.1.2           
#> [145] jquerylib_0.1.4         jsonlite_2.0.0          mclust_6.1.3           
#> [148] GOSemSim_2.38.3         magrittr_2.0.5          Formula_1.2-6          
#> [151] modeltools_0.2-24       ggplotify_0.1.3         tailor_0.1.0           
#> [154] Rcpp_1.1.2              ape_5.8-1               ggnewscale_0.5.2       
#> [157] gdtools_0.5.1           furrr_0.4.0             stringi_1.8.9          
#> [160] ggraph_2.2.2            MASS_7.3-65             plyr_1.8.9             
#> [163] org.Hs.eg.db_3.23.1     embed_1.2.2             flexmix_2.3-20         
#> [166] tidyheatmaps_0.2.1      parallel_4.6.1          listenv_1.0.0          
#> [169] ggrepel_0.9.8           graphlayouts_1.2.5      Biostrings_2.80.1      
#> [172] splines_4.6.1           ps_1.9.3                fastcluster_1.3.0      
#> [175] igraph_2.3.3            ranger_0.18.0           enrichit_0.2.1         
#> [178] dials_1.4.4             rngtools_1.5.2          reshape2_1.4.5         
#> [181] parsnip_1.6.0           stats4_4.6.1            evaluate_1.0.5         
#> [184] foreach_1.5.2           missForest_1.6.1        tweenr_2.0.3           
#> [187] tidyr_1.3.2             openssl_2.4.2           purrr_1.2.2            
#> [190] polyclip_1.10-7         future_1.75.0           ggforce_0.5.0          
#> [193] easyPubMed_3.1.6        RSpectra_0.16-2         tidytree_0.4.8         
#> [196] tidydr_0.0.6            class_7.3-23            ragg_1.5.2             
#> [199] tibble_3.3.1            clusterProfiler_4.20.0  aplot_0.3.1            
#> [202] beeswarm_0.4.0          memoise_2.0.1           AnnotationDbi_1.74.0   
#> [205] IRanges_2.46.0          cluster_2.1.8.2         workflows_1.3.0        
#> [208] timechange_0.4.0        globals_0.19.1
```
