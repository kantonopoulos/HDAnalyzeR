# Cancer cohort metadata

A data subset from synthetic cancer metadata. DAid, Age, BMI, and Cohort
have been added as extra columns. The original dataset was processed
with the `process_example_metadata` script.

## Usage

``` r
example_metadata
```

## Format

A tibble with 586 rows and 9 columns:

- DAid:

  The Disease Atlas sample ID

- Sample:

  The Sample ID

- Disease:

  The cancer type

- Stage:

  The cancer stage

- Grade:

  The cancer grade

- Sex:

  The patient sex

- Age:

  The patient age

- BMI:

  The patient BMI

- Cohort:

  The cohort the patient belongs to

## Source

<https://github.com/buenoalvezm/Pan-cancer-profiling/blob/main/data/cancer_metadata_synthetic.rds>

## Examples

``` r
example_metadata
#> # A tibble: 586 × 9
#>    DAid    Sample     Disease Stage   Grade Sex     Age   BMI Cohort
#>    <chr>   <chr>      <chr>   <chr>   <chr> <chr> <dbl> <dbl> <chr> 
#>  1 DA00001 AML_syn_1  AML     2       NA    F        42  22.7 UCAN  
#>  2 DA00002 AML_syn_2  AML     Unknown NA    M        69  33.1 UCAN  
#>  3 DA00003 AML_syn_3  AML     2       NA    F        61  26.2 UCAN  
#>  4 DA00004 AML_syn_4  AML     Unknown NA    M        54  28.1 UCAN  
#>  5 DA00005 AML_syn_5  AML     2       NA    F        57  21.4 UCAN  
#>  6 DA00006 AML_syn_6  AML     Unknown NA    M        86  33.9 UCAN  
#>  7 DA00007 AML_syn_7  AML     1       NA    F        85  28.7 UCAN  
#>  8 DA00008 AML_syn_8  AML     3       NA    F        88  32.6 UCAN  
#>  9 DA00009 AML_syn_9  AML     Unknown NA    M        80  26.1 UCAN  
#> 10 DA00010 AML_syn_10 AML     3       NA    M        48  33.8 UCAN  
#> # ℹ 576 more rows
```
