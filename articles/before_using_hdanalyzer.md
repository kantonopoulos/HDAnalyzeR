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
#> R version 4.5.2 (2025-10-31)
#> Platform: x86_64-pc-linux-gnu
#> Running under: Ubuntu 24.04.3 LTS
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
#> [1] HDAnalyzeR_0.99.0
#> 
#> loaded via a namespace (and not attached):
#>  [1] digest_0.6.37     desc_1.4.3        R6_2.6.1          fastmap_1.2.0    
#>  [5] xfun_0.54         cachem_1.1.0      knitr_1.50        htmltools_0.5.8.1
#>  [9] rmarkdown_2.30    lifecycle_1.0.4   cli_3.6.5         sass_0.4.10      
#> [13] pkgdown_2.2.0     textshaping_1.0.4 jquerylib_0.1.4   systemfonts_1.3.1
#> [17] compiler_4.5.2    tools_4.5.2       ragg_1.5.0        bslib_0.9.0      
#> [21] evaluate_1.0.5    yaml_2.3.10       jsonlite_2.0.0    rlang_1.1.6      
#> [25] fs_1.6.6          htmlwidgets_1.6.4
```
