# Data Preparation for HDAnalyzeR: What You Need Before Using the Package

## Introduction

Before you can start using HDAnalyzeR for biomarker discovery, it is
essential to ensure that your data is prepared in the appropriate
format. While HDAnalyzeR offers a variety of powerful tools, it does not
include technology-specific quality control (QC) or preprocessing
functions. This design choice allows the package to be flexible and
usable with a wide range of proteomics technologies without being
limited to specific workflows. Many labs already use their own
preprocessing pipelines, tailored to their specific research needs and
technologies. Integrating all these varied pipelines into the package
would make it unnecessarily complex and restrictive.

Therefore, the data provided to HDAnalyzeR must be preprocessed in order
to follow specific requirements (Table 1).

Table 1. General requirements

[TABLE]

> ⚠️ Although HDAnalyzeR is primarily designed for proteomics data, some
> functions in the package can also be applied to other omics data types
> such as genomics, transcriptomics, or metabolomics. However, it is
> important to note that the specific choice of functions and how they
> should be used will depend on the study’s goals and the nature of the
> data. HDAnalyzeR does not provide explicit guarantees and the user
> must make informed decisions regarding its application to other types
> of omics data. Documentation and the source code of all functions is
> freely available.

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
#> [1] glmnet_5.0       Matrix_1.7-5     HDAnalyzeR_1.1.0
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
#>  [55] dplyr_1.2.1             GO.db_3.23.1            ggbeeswarm_0.7.3       
#>  [58] fansi_1.0.7             S4Vectors_0.50.1        lifecycle_1.0.5        
#>  [61] whisker_0.4.1           yaml_2.3.12             recipes_1.3.3          
#>  [64] qvalue_2.44.0           grid_4.6.1              blob_1.3.0             
#>  [67] crayon_1.5.3            ggtangle_0.1.2          lattice_0.22-9         
#>  [70] KEGGREST_1.52.2         pillar_1.11.1           knitr_1.51             
#>  [73] fpc_2.2-14              future.apply_1.20.2     codetools_0.2-20       
#>  [76] glue_1.8.1              ggiraph_0.9.6           rsample_1.3.2          
#>  [79] ggfun_0.2.1             fontLiberation_0.1.0    data.table_1.18.4      
#>  [82] vctrs_0.7.3             png_0.1-9               treeio_1.36.1          
#>  [85] Rdpack_2.6.6            gtable_0.3.6            kernlab_0.9-33         
#>  [88] cachem_1.1.0            gower_1.0.2             xfun_0.60              
#>  [91] rbibutils_2.4.1         prodlim_2026.03.11      Seqinfo_1.2.0          
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
#> [151] modeltools_0.2-24       ggplotify_0.1.3         patchwork_1.3.2        
#> [154] tailor_0.1.0            Rcpp_1.1.2              ape_5.8-1              
#> [157] ggnewscale_0.5.2        gdtools_0.5.1           furrr_0.4.0            
#> [160] stringi_1.8.9           MASS_7.3-65             plyr_1.8.9             
#> [163] org.Hs.eg.db_3.23.1     embed_1.2.2             flexmix_2.3-20         
#> [166] tidyheatmaps_0.2.1      parallel_4.6.1          listenv_1.0.0          
#> [169] ggrepel_0.9.8           Biostrings_2.80.1       splines_4.6.1          
#> [172] ps_1.9.3                fastcluster_1.3.0       igraph_2.3.3           
#> [175] ranger_0.18.0           enrichit_0.2.1          dials_1.4.4            
#> [178] rngtools_1.5.2          reshape2_1.4.5          parsnip_1.6.0          
#> [181] stats4_4.6.1            evaluate_1.0.5          foreach_1.5.2          
#> [184] missForest_1.6.1        tweenr_2.0.3            tidyr_1.3.2            
#> [187] openssl_2.4.2           purrr_1.2.2             polyclip_1.10-7        
#> [190] future_1.75.0           ggplot2_4.0.3           ggforce_0.5.0          
#> [193] easyPubMed_3.1.6        RSpectra_0.16-2         tidytree_0.4.8         
#> [196] tidydr_0.0.6            class_7.3-23            ragg_1.5.2             
#> [199] tibble_3.3.1            clusterProfiler_4.20.0  aplot_0.3.1            
#> [202] beeswarm_0.4.0          memoise_2.0.1           AnnotationDbi_1.74.0   
#> [205] IRanges_2.46.0          cluster_2.1.8.2         workflows_1.3.0        
#> [208] timechange_0.4.0        globals_0.19.1
```
