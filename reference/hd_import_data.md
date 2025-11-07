# Import data from file

`hd_import_data()` imports data from a file. The file format can be CSV,
TSV, TXT, RDA, RDS, XLSX, or Parquet format. It recognizes the file
format, reads it and returns it as a tibble or an R object.

## Usage

``` r
hd_import_data(path_name)
```

## Arguments

- path_name:

  The path to the file to import.

## Value

The imported data as a tibble or an R object.

## Examples

``` r
# Save a dataframe as an RDS file
hd_save_data(example_metadata, "my_data/metadata.rds")
#> [1] "File saved as my_data/metadata.rds"

# Import the saved RDS file again as a tibble
hd_import_data("my_data/metadata.rds")
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

unlink("my_data", recursive = TRUE)  # Clean up the created directory
```
