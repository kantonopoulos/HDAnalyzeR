# Fetch PubMed articles for disease-protein biomarker associations

`hd_literature_search()` fetches PubMed articles for disease-protein
biomarker associations.

## Usage

``` r
hd_literature_search(query_list, max_results = 20, min_year = NULL)
```

## Arguments

- query_list:

  Named list of diseases with associated proteins. Example: list( "Acute
  Myeloid Leukemia" = c("TCL1A", "TNFRSF9"), "Chronic Lymphocytic
  Leukemia" = c("CD22"))

- max_results:

  Maximum number of articles to return per protein query.

- min_year:

  Only include articles published in or after this year.

## Value

A dataframe with columns: Disease, Protein, PMID, Title, and Abstract

## Examples

``` r
queries <- list(
  "Acute Myeloid Leukemia" = c("TCL1A", "TNFRSF9"),
  "Chronic Lymphocytic Leukemia" = c("CD22")
)

res <- hd_literature_search(queries, max_results = 2, min_year = 2015)
#> 
#> Retrieving records 
#> .
#> 
#> Parsing record information
#> |----+----+----+----| 100%
#> |
#> ...................|
head(res, 1)
#>                  Disease Protein     PMID
#> 1 Acute Myeloid Leukemia   TCL1A 41200167
#>                                                                                                                                            Title
#> 1 Identification and validation of plasma protein biomarkers as therapeutic targets in acute myeloid leukemia: an integrative multi-omics study.
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            Abstract
#> 1 Acute myeloid leukemia (AML) remains a therapeutic challenge due to its high relapse rate and limited treatment options. This study aimed to identify and validate novel circulating protein biomarkers with causal roles in AML pathogenesis using an integrative multi-omics approach. We performed proteome-wide Mendelian randomization (MR) analyses using protein quantitative trait locus (pQTL) data from two large-scale proteomic studies (deCODE and UK Biobank Pharma Proteomics Project) and genome-wide association study (GWAS) data from two cohorts (FinnGen and UK Biobank). Single-cell RNA sequencing was used to analyze the expression patterns of candidate proteins in hematopoietic progenitor and immune cells. Plasma protein levels were experimentally validated via ELISA in AML patients and healthy controls, and their dynamic changes relative to disease status were assessed. Drug repurposing analysis and phenome-wide association studies (PheWAS) were conducted to evaluate potential therapeutic agents and their safety profiles. Three independent MR analyses identified TNFAIP8, TCL1A, and WFDC1 as risk factors for AML, while TNFSF8 was identified as a protective factor. Single-cell RNA sequencing revealed distinct expression patterns of these proteins within hematopoietic progenitor and immune cells, suggesting roles in microenvironmental dysregulation. ELISA validation confirmed elevated plasma levels of TNFAIP8, TCL1A, and WFDC1 and reduced levels of TNFSF8 in AML patients compared to healthy controls. Dynamic changes were observed for TNFAIP8 and TNFSF8, supporting their potential for disease monitoring. Drug repurposing analysis prioritized 13 candidates targeting these proteins, including FDA-approved agents, and PheWAS supported their safety. This study provides the first genetic evidence supporting the causal roles of TNFAIP8, TCL1A, WFDC1, and TNFSF8 in AML, offering new insights for targeted therapy development and biomarker-based disease monitoring.
```
