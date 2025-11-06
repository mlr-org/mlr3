# Create a Data Backend

Wraps a
[DataBackend](https://mlr3.mlr-org.com/dev/reference/DataBackend.md)
around data. [mlr3](https://CRAN.R-project.org/package=mlr3) ships with
methods for `data.frame` (converted to a
[DataBackendDataTable](https://mlr3.mlr-org.com/dev/reference/DataBackendDataTable.md).

Additional methods are implemented in the package
[mlr3db](https://CRAN.R-project.org/package=mlr3db), e.g. to connect to
real DBMS like PostgreSQL (via
[dbplyr](https://CRAN.R-project.org/package=dbplyr)) or DuckDB (via
[DBI](https://CRAN.R-project.org/package=DBI)/[duckdb](https://CRAN.R-project.org/package=duckdb)).

## Usage

``` r
as_data_backend(data, primary_key = NULL, ...)

# S3 method for class 'data.frame'
as_data_backend(data, primary_key = NULL, keep_rownames = FALSE, ...)
```

## Arguments

- data:

  ([`data.frame()`](https://rdrr.io/r/base/data.frame.html))  
  The input [`data.frame()`](https://rdrr.io/r/base/data.frame.html).
  Automatically converted to a
  [`data.table::data.table()`](https://rdatatable.gitlab.io/data.table/reference/data.table.html).

- primary_key:

  (`character(1)` \|
  [`integer()`](https://rdrr.io/r/base/integer.html))  
  Name of the primary key column, or integer vector of row ids.

- ...:

  (any)  
  Additional arguments passed to the respective
  [DataBackend](https://mlr3.mlr-org.com/dev/reference/DataBackend.md)
  method.

- keep_rownames:

  (`logical(1)` \| `character(1)`)  
  If `TRUE` or a single string, keeps the row names of `data` as a new
  column. The column is named like the provided string, defaulting to
  `"..rownames"` for `keep_rownames == TRUE`. Note that the created
  column will be used as a regular feature by the task unless you
  manually change the column role. Also see
  [`data.table::as.data.table()`](https://rdatatable.gitlab.io/data.table/reference/as.data.table.html).

## Value

[DataBackend](https://mlr3.mlr-org.com/dev/reference/DataBackend.md).

## See also

- Chapter in the [mlr3book](https://mlr3book.mlr-org.com/):
  <https://mlr3book.mlr-org.com/chapters/chapter10/advanced_technical_aspects_of_mlr3.html#sec-backends>

- Package [mlr3db](https://CRAN.R-project.org/package=mlr3db) to
  interface out-of-memory data, e.g. SQL servers or
  [duckdb](https://CRAN.R-project.org/package=duckdb).

Other DataBackend:
[`DataBackend`](https://mlr3.mlr-org.com/dev/reference/DataBackend.md),
[`DataBackendDataTable`](https://mlr3.mlr-org.com/dev/reference/DataBackendDataTable.md)

## Examples

``` r
# create a new backend using the penguins data:
as_data_backend(palmerpenguins::penguins)
#> 
#> ── <DataBackendDataTable> (344x9) ──────────────────────────────────────────────
#>  species    island bill_length_mm bill_depth_mm flipper_length_mm body_mass_g
#>   <fctr>    <fctr>          <num>         <num>             <int>       <int>
#>   Adelie Torgersen           39.1          18.7               181        3750
#>   Adelie Torgersen           39.5          17.4               186        3800
#>   Adelie Torgersen           40.3          18.0               195        3250
#>   Adelie Torgersen             NA            NA                NA          NA
#>   Adelie Torgersen           36.7          19.3               193        3450
#>   Adelie Torgersen           39.3          20.6               190        3650
#>     sex  year ..row_id
#>  <fctr> <int>    <int>
#>    male  2007        1
#>  female  2007        2
#>  female  2007        3
#>    <NA>  2007        4
#>  female  2007        5
#>    male  2007        6
#> [...] (338 rows omitted)
```
