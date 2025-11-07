# Convert gene names to ENTREZID

`gene_to_entrezid()` converts gene names to ENTREZID using the
`org.Hs.eg.db` database.

## Usage

``` r
gene_to_entrezid(gene_list, background = NULL)
```

## Arguments

- gene_list:

  A character vector containing the gene names.

- background:

  A character vector containing the background genes if any.

## Value

A list containing the gene list and the background with ENTREZID.
