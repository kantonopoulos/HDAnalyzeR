# Gene set enrichment analysis

`hd_gsea()` performs gene set enrichment analysis (GSEA) using the
clusterProfiler package.

## Usage

``` r
hd_gsea(
  de_results,
  database = c("GO", "Reactome", "KEGG"),
  ontology = c("BP", "CC", "MF", "ALL"),
  ranked_by = "logFC",
  pval_lim = 0.05,
  seed = 123
)
```

## Arguments

- de_results:

  An `hd_de` object from
  [`hd_de_limma()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_de_limma.md)
  or
  [`hd_de_ttest()`](https://kantonopoulos.github.io/HDAnalyzeR/reference/hd_de_ttest.md)
  or a tibble containing the results of a differential expression
  analysis.

- database:

  The database to perform the ORA. It can be either "GO", "KEGG", or
  "Reactome".

- ontology:

  The ontology to use when database = "GO". It can be "BP" (Biological
  Process), "CC" (Cellular Component), "MF" (Molecular Function), or
  "ALL". In the case of KEGG and Reactome, this parameter is ignored.

- ranked_by:

  The variable to rank the proteins. It can be "logFC", "both" which is
  the product of logFC and -log(adj.P.Val) or a custom sorting variable.
  It should be however a column in the DE results tibble (`de_results`
  argument).

- pval_lim:

  The p-value threshold to consider a term as significant in the
  enrichment analysis. Default is 0.05.

- seed:

  Seed for reproducibility. Default is 123. Set to NULL to leave the
  random number generator untouched.

## Value

A list containing the results of the GSEA.

## Details

GSEA p-values come from a permutation test, so repeated runs on the same
data return slightly different results. `seed` fixes the random number
generator for the duration of the call to make a run reproducible.

When nothing passes the significance threshold the function warns and
returns the (empty) enrichment object instead of stopping, so that a
null result does not abort a longer pipeline.

To perform the GSEA, `clusterProfiler` package is used. For more
information, please refer to the `clusterProfiler` documentation.

If you want to learn more about GSEA, please refer to the following
publications:

- Chicco D, Agapito G. Nine quick tips for pathway enrichment analysis.
  PLoS Comput Biol. 2022 Aug 11;18(8):e1010348. doi:
  10.1371/journal.pcbi.1010348. PMID: 35951505; PMCID: PMC9371296.
  https://pmc.ncbi.nlm.nih.gov/articles/PMC9371296/

- https://yulab-smu.top/biomedical-knowledge-mining-book/enrichment-overview.html#gsea-algorithm

## Examples

``` r
# Initialize an HDAnalyzeR object
hd_object <- hd_initialize(example_data, example_metadata)

# Run differential expression analysis for AML vs all others
de_results <- hd_de_limma(hd_object, case = "AML")

# Run GSEA with GO database
hd_gsea(de_results,
        database = "GO",
        ontology = "BP",
        ranked_by = "logFC",
        pval_lim = 0.9)
#> 'select()' returned 1:1 mapping between keys and columns
#> $gene_list
#>          328          566          100         9289        54518         9048 
#>  1.559583713  1.532943471  1.462382647  1.231859388  1.173013982  0.828793582 
#>          285          181           25          191        51129         9296 
#>  0.772836518  0.756171703  0.746830743  0.734041132  0.522682913  0.519848239 
#>           59        59272        51742        25814          199          267 
#>  0.504682910  0.466035852  0.458170283  0.451765263  0.432416150  0.428812794 
#>         2683         9938          976        30817         1386          405 
#>  0.421745793  0.397319743  0.365431820  0.359446007  0.338581655  0.337841863 
#>        23452        10159        51816          374          410          133 
#>  0.292444281  0.271377450  0.253959306  0.242008838  0.226357122  0.199535962 
#>       170689        51327          411        84335          214       375790 
#>  0.177164675  0.175706968  0.173203848  0.170442918  0.162775605  0.159570184 
#>         8751           95       129138          308        80755          558 
#>  0.123946384  0.122930955  0.113185504  0.108778505  0.106165339  0.093214100 
#>         9068          283          218          258          189        81693 
#>  0.092176270  0.086140961  0.081630236  0.059789250  0.045913185  0.043794218 
#>          290           54           81         8745           51         1109 
#>  0.026847324  0.019847155 -0.003178737 -0.009723351 -0.009971140 -0.016368666 
#>          231          177          383          117          392          350 
#> -0.018310077 -0.038228906 -0.039306455 -0.043239615 -0.054674439 -0.076035424 
#>        51382          539          333          419       155465         9131 
#> -0.097147761 -0.098498094 -0.104025127 -0.104158209 -0.104553945 -0.110089916 
#>          127         8312          101          259           30        27329 
#> -0.129895411 -0.135089674 -0.138922933 -0.160969394 -0.161931907 -0.183584355 
#>         8639          216       347902          279        11199           26 
#> -0.194111783 -0.195512297 -0.200816339 -0.215411972 -0.244774741 -0.245691583 
#>        10218       170690        51205        11093        10000          475 
#> -0.272748339 -0.298014216 -0.308354203 -0.314105581 -0.333881970 -0.337183300 
#>        11095          307        93974        10149          280          176 
#> -0.339134804 -0.342512836 -0.356798140 -0.375853514 -0.376116879 -0.383419881 
#>          311        55937          203          306       115201        10551 
#> -0.404836340 -0.440825951 -0.481572885 -0.619682407 -0.629226223 -0.704655666 
#>        23365          351          250          284 
#> -0.806377522 -0.822744636 -1.036735621 -1.696033915 
#> 
#> $enrichment
#> #
#> # Gene Set Enrichment Analysis
#> #
#> #...@organism     Homo sapiens 
#> #...@setType      BP 
#> #...@keytype      ENTREZID 
#> #...@geneList     Named num [1:100] 1.56 1.53 1.46 1.23 1.17 ...
#>  - attr(*, "names")= chr [1:100] "328" "566" "100" "9289" ...
#> #...nPerm     1000 
#> #...pvalues adjusted by 'BH' with cutoff < 0.9
#> #...202 enriched terms found
#> 'data.frame':    202 obs. of  12 variables:
#>  $ ID             : chr  "GO:0033036" "GO:0008104" "GO:0065007" "GO:0007154" ...
#>  $ Description    : chr  "macromolecule localization" "intracellular protein localization" "biological regulation" "cell communication" ...
#>  $ setSize        : int  18 15 75 52 52 67 51 60 65 40 ...
#>  $ enrichmentScore: num  -0.615 -0.62 0.555 0.512 0.512 ...
#>  $ NES            : num  -1.8 -1.65 1.64 1.56 1.56 ...
#>  $ pvalue         : num  0.01044 0.01137 0.00237 0.01131 0.01131 ...
#>  $ p.adjust       : num  0.42 0.42 0.42 0.42 0.42 ...
#>  $ qvalue         : num  0.0495 0.0495 0.0495 0.0495 0.0495 ...
#>  $ rank           : int  15 7 28 28 28 28 28 28 24 20 ...
#>  $ leading_edge   : chr  "tags=33%, list=15%, signal=35%" "tags=27%, list=7%, signal=29%" "tags=36%, list=28%, signal=104%" "tags=42%, list=28%, signal=63%" ...
#>  $ core_enrichment: chr  "93974/55937/115201/10551/351/284" "115201/10551/351/284" "328/566/100/9289/54518/9048/285/181/25/51129/9296/59/59272/51742/25814/199/267/2683/9938/976/30817/1386/405/234"| __truncated__ "566/100/9289/54518/9048/285/181/25/59/59272/51742/199/267/9938/976/30817/1386/405/23452/10159/51816/374" ...
#>  $ log2err        : num  0.381 0.381 0.432 0.381 0.381 ...
#> #...Citation
#> S Xu, E Hu, Y Cai, Z Xie, X Luo, L Zhan, W Tang, Q Wang, B Liu, R Wang, W Xie, T Wu, L Xie, G Yu. Using clusterProfiler to characterize multiomics data. Nature Protocols. 2024, 19(11):3292-3320 
#> 
#> 
#> $pval_lim
#> [1] 0.9
#> 
#> attr(,"class")
#> [1] "hd_enrichment"
# Remember that the data is artificial, this is why we use an absurdly high p-value cutoff

# Run GSEA with different ranking variable
enrichment <- hd_gsea(de_results,
                      database = "GO",
                      ontology = "BP",
                      ranked_by = "both",
                      pval_lim = 0.9)
#> 'select()' returned 1:1 mapping between keys and columns
#> Warning: No significant terms found in the gene set enrichment analysis. The returned object contains the (empty) enrichment result, so no plots can be produced. Consider relaxing `pval_lim` or using a larger gene list.

# Access the results
head(enrichment$enrichment@result)
#>                    ID                             Description setSize
#> GO:0033036 GO:0033036              macromolecule localization      18
#> GO:0008104 GO:0008104      intracellular protein localization      15
#> GO:0051246 GO:0051246 regulation of protein metabolic process      10
#> GO:0000165 GO:0000165                            MAPK cascade      10
#> GO:0042592 GO:0042592                     homeostatic process      19
#> GO:0030335 GO:0030335   positive regulation of cell migration      11
#>            enrichmentScore       NES     pvalue  p.adjust   qvalue rank
#> GO:0033036      -0.8667594 -1.618639 0.01117754 0.9250806 0.411937    9
#> GO:0008104      -0.8762718 -1.507876 0.02210625 0.9250806 0.411937    7
#> GO:0051246      -0.8827495 -1.501269 0.08012821 0.9250806 0.411937    3
#> GO:0000165      -0.8712859 -1.481773 0.09935897 0.9250806 0.411937    3
#> GO:0042592      -0.7810418 -1.407670 0.04169602 0.9250806 0.411937    3
#> GO:0030335      -0.7865683 -1.366826 0.15517241 0.9250806 0.411937    8
#>                             leading_edge            core_enrichment   log2err
#> GO:0033036 tags=28%, list=9%, signal=31% 55937/10551/115201/351/284 0.3807304
#> GO:0008104 tags=27%, list=7%, signal=29%       10551/115201/351/284 0.3524879
#> GO:0051246 tags=20%, list=3%, signal=22%                    351/284 0.2878571
#> GO:0000165 tags=20%, list=3%, signal=22%                    351/284 0.2572065
#> GO:0042592 tags=11%, list=3%, signal=13%                    351/284 0.3217759
#> GO:0030335 tags=27%, list=8%, signal=28%                306/351/284 0.2114002
```
