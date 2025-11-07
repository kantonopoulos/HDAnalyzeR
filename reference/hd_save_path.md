# Create directory to save results

`hd_save_path()` creates a directory with in a specified or the current
path to save results. The user can optionally choose to create another
inner directory named with the current date. If the directory already
exists, a message is printed.

## Usage

``` r
hd_save_path(path_name, date = FALSE)
```

## Arguments

- path_name:

  The name of the directory to create.

- date:

  If TRUE, a directory with the current date as name will be created
  inside the directory with `path_name`.

## Value

The relative directory path as a string.

## Examples

``` r
# Create a directory
hd_save_path("my_directory", date = FALSE)
#> [1] "my_directory"
unlink("my_directory", recursive = TRUE)  # Clean up the created directory

# Create a directory and an inner directory with the current date as name
hd_save_path("my_directory", date = TRUE)
#> [1] "my_directory/2025_11_07"
unlink("my_directory", recursive = TRUE)

# Create a directory inside another directory
hd_save_path("outer_directory/inner_directory", date = FALSE)
#> [1] "outer_directory/inner_directory"
unlink("outer_directory", recursive = TRUE)

# Create a directory inside a pre existing one
hd_save_path("outer_directory", date = FALSE)
#> [1] "outer_directory"
hd_save_path("outer_directory/inner_directory", date = FALSE)
#> [1] "outer_directory/inner_directory"
unlink("outer_directory", recursive = TRUE)

# Create a directory with the current date as name inside a pre existing one
hd_save_path("outer_directory", date = FALSE)
#> [1] "outer_directory"
hd_save_path("outer_directory", date = TRUE)
#> [1] "outer_directory/2025_11_07"
unlink("outer_directory", recursive = TRUE)
```
