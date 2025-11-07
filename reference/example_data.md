# Cancer cohort Olink data

A data subset from a synthetic cancer dataset keeping the first 100
Assays. DAid, Assay_Warning, QC_Warning, and PlateID have been added as
extra columns. The original dataset was processed with the
`process_example_data` script.

## Usage

``` r
example_data
```

## Format

A tibble with 56,142 rows and 10 columns:

- DAid:

  The Disease Atlas sample ID

- Sample:

  The Sample ID

- OlinkID:

  The Olink Assay ID

- UniProt:

  The UniProt Assay ID

- Assay:

  The Assay name

- Panel:

  The Olink Panel in which the Assay belongs to

- NPX:

  The NPX value for each Sample and Assay

- Assay_Warning:

  The Assay warning status for the sample

- QC_Warning:

  The QC warning status for the sample

- PlateID:

  The ID of the plate where the sample was processed

## Source

<https://github.com/buenoalvezm/Pan-cancer-profiling/blob/main/data/cancer_data_synthetic.rds>

## Examples

``` r
example_data
#> # A tibble: 56,142 × 10
#>    DAid    Sample   OlinkID UniProt Assay Panel     NPX Assay_Warning QC_Warning
#>    <chr>   <chr>    <chr>   <chr>   <chr> <chr>   <dbl> <chr>         <chr>     
#>  1 DA00001 AML_syn… OID213… Q9BTE6  AARS… Onco…  3.39   PASS          PASS      
#>  2 DA00001 AML_syn… OID212… P00519  ABL1  Onco…  2.76   PASS          PASS      
#>  3 DA00001 AML_syn… OID212… P09110  ACAA1 Onco…  1.71   PASS          PASS      
#>  4 DA00001 AML_syn… OID201… P16112  ACAN  Card…  0.0333 PASS          PASS      
#>  5 DA00001 AML_syn… OID201… Q9BYF1  ACE2  Card…  1.76   PASS          PASS      
#>  6 DA00001 AML_syn… OID201… Q15067  ACOX1 Card… -0.919  PASS          PASS      
#>  7 DA00001 AML_syn… OID203… P13686  ACP5  Card…  1.54   PASS          PASS      
#>  8 DA00001 AML_syn… OID214… Q9NPH0  ACP6  Onco…  2.15   PASS          PASS      
#>  9 DA00001 AML_syn… OID200… P62736  ACTA2 Card…  2.81   PASS          PASS      
#> 10 DA00001 AML_syn… OID204… O43707  ACTN4 Infl…  0.742  PASS          PASS      
#> # ℹ 56,132 more rows
#> # ℹ 1 more variable: PlateID <chr>
```
