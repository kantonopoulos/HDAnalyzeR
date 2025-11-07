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
  pval_lim = 0.05
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

## Value

A list containing the results of the GSEA.

## Details

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
#> 
#> 
#> 'select()' returned 1:1 mapping between keys and columns
#> using 'fgsea' for GSEA analysis, please cite Korotkevich et al (2019).
#> preparing geneSet collections...
#> GSEA analysis...
#> leading edge analysis...
#> done...
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
#> #...nPerm     
#> #...pvalues adjusted by 'BH' with cutoff <0.9 
#> #...233 enriched terms found
#> 'data.frame':    233 obs. of  11 variables:
#>  $ ID             : chr  "GO:0015031" "GO:0045184" "GO:0033036" "GO:0048518" ...
#>  $ Description    : chr  "protein transport" "establishment of protein localization" "macromolecule localization" "positive regulation of biological process" ...
#>  $ setSize        : int  10 11 19 42 77 71 11 70 16 11 ...
#>  $ enrichmentScore: num  -0.769 -0.706 -0.61 0.557 0.55 ...
#>  $ NES            : num  -1.88 -1.77 -1.76 1.64 1.61 ...
#>  $ pvalue         : num  0.00409 0.00544 0.00614 0.00634 0.00475 ...
#>  $ p.adjust       : num  0.264 0.264 0.264 0.264 0.264 ...
#>  $ qvalue         : num  0.25 0.25 0.25 0.25 0.25 ...
#>  $ rank           : num  7 15 15 19 28 28 7 24 7 20 ...
#>  $ leading_edge   : chr  "tags=30%, list=7%, signal=31%" "tags=36%, list=15%, signal=35%" "tags=32%, list=15%, signal=33%" "tags=36%, list=19%, signal=50%" ...
#>  $ core_enrichment: chr  "115201/351/284" "93974/115201/351/284" "93974/55937/115201/10551/351/284" "328/566/100/9289/54518/9048/285/181/25/51129/59/59272/51742/199/2683" ...
#> #...Citation
#> S Xu, E Hu, Y Cai, Z Xie, X Luo, L Zhan, W Tang, Q Wang, B Liu, R Wang, W Xie, T Wu, L Xie, G Yu. Using clusterProfiler to characterize multiomics data. Nature Protocols. 2024, 19(11):3292-3320 
#> 
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
#> using 'fgsea' for GSEA analysis, please cite Korotkevich et al (2019).
#> preparing geneSet collections...
#> GSEA analysis...
#> Warning: There are ties in the preranked stats (22% of the list).
#> The order of those tied genes will be arbitrary, which may produce unexpected results.
#> Warning: All values in the stats vector are greater than zero and scoreType is "std", maybe you should switch to scoreType = "pos".
#> leading edge analysis...
#> done...

# Access the results
head(enrichment$enrichment@result)
#>                    ID                         Description setSize
#> GO:0003008 GO:0003008                      system process      17
#> GO:0044283 GO:0044283 small molecule biosynthetic process      10
#> GO:0003013 GO:0003013          circulatory system process      12
#> GO:0008015 GO:0008015                   blood circulation      12
#> GO:0080134 GO:0080134    regulation of response to stress      16
#> GO:0002252 GO:0002252             immune effector process      12
#>            enrichmentScore      NES      pvalue  p.adjust    qvalue rank
#> GO:0003008       0.7487751 1.375646 0.005471063 0.7639186 0.7596414   19
#> GO:0044283       0.7847467 1.374343 0.018755240 0.7639186 0.7596414   13
#> GO:0003013       0.7432055 1.322340 0.028197382 0.7639186 0.7596414   10
#> GO:0008015       0.7432055 1.322340 0.028197382 0.7639186 0.7596414   10
#> GO:0080134       0.7162173 1.309517 0.012654474 0.7639186 0.7596414   18
#> GO:0002252       0.7285557 1.296274 0.033232628 0.7639186 0.7596414   10
#>                              leading_edge        core_enrichment
#> GO:0003008 tags=29%, list=19%, signal=29% 231/290/177/81693/8312
#> GO:0044283 tags=50%, list=13%, signal=48%    51/231/1109/189/117
#> GO:0003013 tags=17%, list=10%, signal=17%                290/177
#> GO:0008015 tags=17%, list=10%, signal=17%                290/177
#> GO:0080134 tags=31%, list=18%, signal=31%     54/383/177/117/350
#> GO:0002252 tags=25%, list=10%, signal=26%             54/383/177
```
