# Post Analysis: Pathway Enrichment

This vignette will guide you through the post analysis of the results
obtained from the HDAnalyzeR pipeline. The pathway enrichment analysis
is performed using the Gene Ontology, KEGG and Reactome databases from
`clusterProfiler` and `ReactomePA` packages respectively.

If you want to learn more about ORA and GSEA, please refer to the
following publications:

- Chicco D, Agapito G. Nine quick tips for pathway enrichment analysis.
  PLoS Comput Biol. 2022 Aug 11;18(8):e1010348. doi:
  10.1371/journal.pcbi.1010348. PMID: 35951505; PMCID: PMC9371296.
  <https://pmc.ncbi.nlm.nih.gov/articles/PMC9371296/>
- <https://yulab-smu.top/biomedical-knowledge-mining-book/enrichment-overview.html#gsea-algorithm>

> 📓 Remember that these data are a dummy-dataset with artificial data
> and the results in this guide should not be interpreted as real
> results. This is why we are using extremely large p-value cutoffs in
> this case that should not be used in real data.

## Loading the Data

We will load HDAnalyzeR and dplyr, load the example data and metadata
that come with the package and initialize the HDAnalyzeR object.

``` r

library(HDAnalyzeR)
library(dplyr)

hd_obj <- hd_initialize(dat = example_data, 
                        metadata = example_metadata, 
                        is_wide = FALSE, 
                        sample_id = "DAid",
                        var_name = "Assay",
                        value_name = "NPX")
```

For the Over Representation Analysis we are going to use a list of
differentially expressed proteins. In this example we are going to use
the up-regulated proteins. We could also use the features list from the
classification models or even run both and get the intersect as it is
done in the Get Started guide.

``` r

de_res <- hd_de_limma(hd_obj, case = "AML")
```

## Over Representation Analysis

First, we will perform an Over Representation Analysis (ORA) using the
Gene Ontology database and the BP ontology. We will use the
[`hd_ora()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_ora.md)
and
[`hd_plot_ora()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_plot_ora.md)
functions to run the analysis and plot the results respectively.

``` r

proteins <- de_res$de_res |> 
  filter(logFC > 0 & adj.P.Val < 0.05) |> 
  pull(Feature)

enrichment <- hd_ora(proteins, database = "GO", ontology = "BP")

enrichment_plots <- hd_plot_ora(enrichment)

enrichment_plots$dotplot
```

![](post_analysis_files/figure-html/unnamed-chunk-3-1.png)

``` r

enrichment_plots$treeplot
```

![](post_analysis_files/figure-html/unnamed-chunk-3-2.png)

``` r

enrichment_plots$cnetplot
```

![](post_analysis_files/figure-html/unnamed-chunk-3-3.png)

Let’s change the database and the p-value threshold.

``` r

enrichment  <- hd_ora(proteins, database = "Reactome", pval_lim = 0.2)

enrichment_plots <- hd_plot_ora(enrichment)

enrichment_plots$dotplot
```

![](post_analysis_files/figure-html/unnamed-chunk-4-1.png)

``` r

enrichment_plots$treeplot
```

![](post_analysis_files/figure-html/unnamed-chunk-4-2.png)

``` r

enrichment_plots$cnetplot
```

![](post_analysis_files/figure-html/unnamed-chunk-4-3.png)

## Gene Set Enrichment Analysis

We can also run a Gene Set Enrichment Analysis (GSEA) using the
[`hd_gsea()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_gsea.md)
and `hd_plot_gsea` functions. The
[`hd_plot_gsea()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_plot_gsea.md)
function will plot the results.

> ⚠️ In this case, the function requires strictly differential
> expression results, so a ranked list of proteins is derived based on
> the `ranked_by` argument.

> 📓 GSEA p-values come from a permutation test, so two runs on the same
> data do not give exactly the same numbers.
> [`hd_gsea()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_gsea.md)
> fixes the random number generator with its `seed` argument (123 by
> default) so that a run is reproducible. If nothing passes `pval_lim`,
> the function warns and returns an empty result instead of stopping.

``` r

enrichment <- hd_gsea(de_res, database = "GO", ontology = "BP", pval_lim = 0.55)

enrichment_plots <- hd_plot_gsea(enrichment)

enrichment_plots$dotplot
```

![](post_analysis_files/figure-html/unnamed-chunk-5-1.png)

``` r

enrichment_plots$gseaplot
```

![](post_analysis_files/figure-html/unnamed-chunk-5-2.png)

``` r

enrichment_plots$cnetplot
```

![](post_analysis_files/figure-html/unnamed-chunk-5-3.png)

``` r

enrichment_plots$ridgeplot
```

![](post_analysis_files/figure-html/unnamed-chunk-5-4.png)

We can also change the ranking variable to the product of logFC and
-log(adjusted p value) instead of the default logFC by changing the
`ranked_by` argument to “both”. We could also use other variables such
as p-value or any other variable in the DE results. However, you should
use as ranking a variable that has some form of biological relevance of
the variable.

``` r

enrichment <- hd_gsea(de_res, 
                      database = "GO", 
                      ontology = "BP", 
                      pval_lim = 0.9, 
                      ranked_by = "both")

enrichment_plots <- hd_plot_gsea(enrichment)
enrichment_plots$cnetplot
#> NULL
```

> 📓 Remember once again that these data are a dummy-dataset with
> artificial data and the results in this guide should not be
> interpreted as real results. The purpose of this vignette is to show
> you how to use the package and its functions.

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
#> [1] stats4    stats     graphics  grDevices utils     datasets  methods  
#> [8] base     
#> 
#> other attached packages:
#>  [1] org.Hs.eg.db_3.23.1  AnnotationDbi_1.74.0 IRanges_2.46.0      
#>  [4] S4Vectors_0.50.1     Biobase_2.72.0       BiocGenerics_0.58.1 
#>  [7] generics_0.1.4       viridis_0.6.5        viridisLite_0.4.3   
#> [10] patchwork_1.3.2      ggplot2_4.0.3        dplyr_1.2.1         
#> [13] glmnet_5.0           Matrix_1.7-5         HDAnalyzeR_1.1.0    
#> 
#> loaded via a namespace (and not attached):
#>   [1] matrixStats_1.5.0       fs_2.1.0                enrichplot_1.32.0      
#>   [4] fontawesome_0.5.3       lubridate_1.9.5         sparsevctrs_0.3.6      
#>   [7] DiceDesign_1.10         httr_1.4.8              RColorBrewer_1.1-3     
#>  [10] doParallel_1.0.17       prabclus_2.3-5          dynamicTreeCut_1.63-1  
#>  [13] backports_1.5.1         tools_4.6.1             doRNG_1.8.6.3          
#>  [16] utf8_1.2.6              R6_2.6.1                mgcv_1.9-4             
#>  [19] lazyeval_0.2.3          uwot_0.2.4              yardstick_1.4.0        
#>  [22] withr_3.0.3             graphite_1.58.0         gridExtra_2.3.1        
#>  [25] downlit_0.4.5           preprocessCore_1.74.0   WGCNA_1.74             
#>  [28] cli_3.6.6               textshaping_1.0.5       scatterpie_0.2.6       
#>  [31] labeling_0.4.3          sass_0.4.10             diptest_0.77-2         
#>  [34] S7_0.2.2                robustbase_0.99-7       randomForest_4.7-1.2   
#>  [37] ggridges_0.5.7          tune_2.1.0              askpass_1.2.1          
#>  [40] pkgdown_2.2.1           systemfonts_1.3.2       yulab.utils_0.2.4      
#>  [43] foreign_0.8-91          gson_0.2.1              DOSE_4.6.0             
#>  [46] parallelly_1.48.0       itertools_0.1-3         limma_3.68.5           
#>  [49] impute_1.86.0           rstudioapi_0.19.0       RSQLite_3.53.3         
#>  [52] shape_1.4.6.1           gridGraphics_0.5-1      GO.db_3.23.1           
#>  [55] ggbeeswarm_0.7.3        fansi_1.0.7             lifecycle_1.0.5        
#>  [58] whisker_0.4.1           yaml_2.3.12             recipes_1.3.3          
#>  [61] qvalue_2.44.0           grid_4.6.1              blob_1.3.0             
#>  [64] crayon_1.5.3            ggtangle_0.1.2          lattice_0.22-9         
#>  [67] KEGGREST_1.52.2         pillar_1.11.1           knitr_1.51             
#>  [70] fpc_2.2-14              future.apply_1.20.2     codetools_0.2-20       
#>  [73] glue_1.8.1              ggiraph_0.9.6           rsample_1.3.2          
#>  [76] ggfun_0.2.1             fontLiberation_0.1.0    data.table_1.18.4      
#>  [79] vctrs_0.7.3             png_0.1-9               treeio_1.36.1          
#>  [82] Rdpack_2.6.6            gtable_0.3.6            kernlab_0.9-33         
#>  [85] cachem_1.1.0            gower_1.0.2             xfun_0.60              
#>  [88] rbibutils_2.4.1         prodlim_2026.03.11      tidygraph_1.3.1        
#>  [91] Seqinfo_1.2.0           survival_3.8-6          timeDate_4052.112      
#>  [94] aisdk_1.4.12            pheatmap_1.0.13         iterators_1.0.14       
#>  [97] hardhat_1.4.3           lava_1.9.2              statmod_1.5.2          
#> [100] ipred_0.9-15            nlme_3.1-169            ggtree_4.2.0           
#> [103] bit64_4.8.2             fontquiver_0.2.1        RcppAnnoy_0.0.23       
#> [106] UpSetR_1.4.1            bslib_0.12.0            vipor_0.4.7            
#> [109] otel_0.2.0              rpart_4.1.27            colorspace_2.1-3       
#> [112] Hmisc_5.2-6             DBI_1.3.0               nnet_7.3-20            
#> [115] ppsr_0.0.5              tidyselect_1.2.1        processx_3.9.0         
#> [118] bit_4.6.0               compiler_4.6.1          curl_7.1.0             
#> [121] graph_1.90.0            httr2_1.3.0             htmlTable_2.5.0        
#> [124] xml2_1.6.0              desc_1.4.3              fontBitstreamVera_0.1.1
#> [127] checkmate_2.3.4         scales_1.4.0            DEoptimR_1.2-0         
#> [130] callr_3.8.0             rappdirs_0.3.4          stringr_1.6.0          
#> [133] digest_0.6.39           rmarkdown_2.31          XVector_0.52.0         
#> [136] base64enc_0.1-6         htmltools_0.5.9         pkgconfig_2.0.3        
#> [139] fastmap_1.2.0           rlang_1.3.0             htmlwidgets_1.6.4      
#> [142] farver_2.1.2            jquerylib_0.1.4         jsonlite_2.0.0         
#> [145] mclust_6.1.3            GOSemSim_2.38.3         magrittr_2.0.5         
#> [148] Formula_1.2-6           modeltools_0.2-24       ggplotify_0.1.3        
#> [151] tailor_0.1.0            Rcpp_1.1.2              ape_5.8-1              
#> [154] ggnewscale_0.5.2        gdtools_0.5.1           furrr_0.4.0            
#> [157] stringi_1.8.9           ggraph_2.2.2            MASS_7.3-65            
#> [160] plyr_1.8.9              embed_1.2.2             flexmix_2.3-20         
#> [163] tidyheatmaps_0.2.1      parallel_4.6.1          listenv_1.0.0          
#> [166] ggrepel_0.9.8           graphlayouts_1.2.5      Biostrings_2.80.1      
#> [169] splines_4.6.1           ps_1.9.3                fastcluster_1.3.0      
#> [172] igraph_2.3.3            ranger_0.18.0           enrichit_0.2.1         
#> [175] dials_1.4.4             rngtools_1.5.2          reshape2_1.4.5         
#> [178] parsnip_1.6.0           evaluate_1.0.5          foreach_1.5.2          
#> [181] missForest_1.6.1        tweenr_2.0.3            tidyr_1.3.2            
#> [184] openssl_2.4.2           purrr_1.2.2             polyclip_1.10-7        
#> [187] future_1.75.0           ReactomePA_1.56.0       ggforce_0.5.0          
#> [190] reactome.db_1.96.0      easyPubMed_3.1.6        RSpectra_0.16-2        
#> [193] tidytree_0.4.8          tidydr_0.0.6            class_7.3-23           
#> [196] ragg_1.5.2              tibble_3.3.1            clusterProfiler_4.20.0 
#> [199] aplot_0.3.1             beeswarm_0.4.0          memoise_2.0.1          
#> [202] cluster_2.1.8.2         workflows_1.3.0         timechange_0.4.0       
#> [205] globals_0.19.1
```
