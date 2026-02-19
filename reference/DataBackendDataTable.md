# DataBackend for data.table

[DataBackend](https://mlr3.mlr-org.com/reference/DataBackend.md) for
[data.table](https://CRAN.R-project.org/package=data.table) which serves
as an efficient in-memory data base.

## See also

- Chapter in the [mlr3book](https://mlr3book.mlr-org.com/):
  <https://mlr3book.mlr-org.com/chapters/chapter10/advanced_technical_aspects_of_mlr3.html#sec-backends>

- Package [mlr3db](https://CRAN.R-project.org/package=mlr3db) to
  interface out-of-memory data, e.g. SQL servers or
  [duckdb](https://CRAN.R-project.org/package=duckdb).

Other DataBackend:
[`DataBackend`](https://mlr3.mlr-org.com/reference/DataBackend.md),
[`as_data_backend()`](https://mlr3.mlr-org.com/reference/as_data_backend.md)

## Super class

[`mlr3::DataBackend`](https://mlr3.mlr-org.com/reference/DataBackend.md)
-\> `DataBackendDataTable`

## Public fields

- `compact_seq`:

  `logical(1)`  
  If `TRUE`, row ids are a natural sequence from 1 to `nrow(data)`
  (determined internally). In this case, row lookup uses faster
  positional indices instead of equi joins.

## Active bindings

- `rownames`:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  Returns vector of all distinct row identifiers, i.e. the contents of
  the primary key column.

- `colnames`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Returns vector of all column names, including the primary key column.

- `nrow`:

  (`integer(1)`)  
  Number of rows (observations).

- `ncol`:

  (`integer(1)`)  
  Number of columns (variables), including the primary key column.

## Methods

### Public methods

- [`DataBackendDataTable$new()`](#method-DataBackendDataTable-new)

- [`DataBackendDataTable$data()`](#method-DataBackendDataTable-data)

- [`DataBackendDataTable$head()`](#method-DataBackendDataTable-head)

- [`DataBackendDataTable$distinct()`](#method-DataBackendDataTable-distinct)

- [`DataBackendDataTable$missings()`](#method-DataBackendDataTable-missings)

Inherited methods

- [`mlr3::DataBackend$format()`](https://mlr3.mlr-org.com/reference/DataBackend.html#method-format)
- [`mlr3::DataBackend$print()`](https://mlr3.mlr-org.com/reference/DataBackend.html#method-print)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

Note that `DataBackendDataTable` does not copy the input data, while
[`as_data_backend()`](https://mlr3.mlr-org.com/reference/as_data_backend.md)
calls
[`data.table::copy()`](https://rdrr.io/pkg/data.table/man/copy.html).
[`as_data_backend()`](https://mlr3.mlr-org.com/reference/as_data_backend.md)
also takes care about casting to a
[`data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html) and
adds a primary key column if necessary.

#### Usage

    DataBackendDataTable$new(data, primary_key)

#### Arguments

- `data`:

  ([`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html))  
  The input
  [`data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html).

- `primary_key`:

  (`character(1)` \|
  [`integer()`](https://rdrr.io/r/base/integer.html))  
  Name of the primary key column, or integer vector of row ids.

------------------------------------------------------------------------

### Method [`data()`](https://rdrr.io/r/utils/data.html)

Returns a slice of the data. The rows must be addressed as vector of
primary key values, columns must be referred to via column names.
Queries for rows with no matching row id and queries for columns with no
matching column name are silently ignored. Rows are guaranteed to be
returned in the same order as `rows`, columns may be returned in an
arbitrary order. Duplicated row ids result in duplicated rows,
duplicated column names lead to an exception.

#### Usage

    DataBackendDataTable$data(rows, cols)

#### Arguments

- `rows`:

  (positive [`integer()`](https://rdrr.io/r/base/integer.html) \|
  `NULL`)  
  Vector or row indices. Always refers to the complete data set, even
  after filtering.

- `cols`:

  ([`character()`](https://rdrr.io/r/base/character.html) \| `NULL`)  
  Vector of column names.

------------------------------------------------------------------------

### Method [`head()`](https://rdrr.io/r/utils/head.html)

Retrieve the first `n` rows.

#### Usage

    DataBackendDataTable$head(n = 6L)

#### Arguments

- `n`:

  (`integer(1)`)  
  Number of rows.

#### Returns

[`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html)
of the first `n` rows.

------------------------------------------------------------------------

### Method `distinct()`

Returns a named list of vectors of distinct values for each column
specified. If `na_rm` is `TRUE`, missing values are removed from the
returned vectors of distinct values. Non-existing rows and columns are
silently ignored.

#### Usage

    DataBackendDataTable$distinct(rows, cols, na_rm = TRUE)

#### Arguments

- `rows`:

  (positive [`integer()`](https://rdrr.io/r/base/integer.html) \|
  `NULL`)  
  Vector or row indices. Always refers to the complete data set, even
  after filtering.

- `cols`:

  ([`character()`](https://rdrr.io/r/base/character.html) \| `NULL`)  
  Vector of column names.

- `na_rm`:

  `logical(1)`  
  Whether to remove NAs or not.

#### Returns

Named [`list()`](https://rdrr.io/r/base/list.html) of distinct values.

------------------------------------------------------------------------

### Method `missings()`

Returns the number of missing values per column in the specified slice
of data. Non-existing rows and columns are silently ignored.

#### Usage

    DataBackendDataTable$missings(rows, cols)

#### Arguments

- `rows`:

  (positive [`integer()`](https://rdrr.io/r/base/integer.html) \|
  `NULL`)  
  Vector or row indices. Always refers to the complete data set, even
  after filtering.

- `cols`:

  ([`character()`](https://rdrr.io/r/base/character.html) \| `NULL`)  
  Vector of column names.

#### Returns

Total of missing values per column (named
[`numeric()`](https://rdrr.io/r/base/numeric.html)).

## Examples

``` r
data = as.data.table(palmerpenguins::penguins)
data$id = seq_len(nrow(palmerpenguins::penguins))
b = DataBackendDataTable$new(data = data, primary_key = "id")
print(b)
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
#>     sex  year    id
#>  <fctr> <int> <int>
#>    male  2007     1
#>  female  2007     2
#>  female  2007     3
#>    <NA>  2007     4
#>  female  2007     5
#>    male  2007     6
#> [...] (338 rows omitted)
b$head()
#> Key: <id>
#>    species    island bill_length_mm bill_depth_mm flipper_length_mm body_mass_g
#>     <fctr>    <fctr>          <num>         <num>             <int>       <int>
#> 1:  Adelie Torgersen           39.1          18.7               181        3750
#> 2:  Adelie Torgersen           39.5          17.4               186        3800
#> 3:  Adelie Torgersen           40.3          18.0               195        3250
#> 4:  Adelie Torgersen             NA            NA                NA          NA
#> 5:  Adelie Torgersen           36.7          19.3               193        3450
#> 6:  Adelie Torgersen           39.3          20.6               190        3650
#>       sex  year    id
#>    <fctr> <int> <int>
#> 1:   male  2007     1
#> 2: female  2007     2
#> 3: female  2007     3
#> 4:   <NA>  2007     4
#> 5: female  2007     5
#> 6:   male  2007     6
b$data(rows = 100:101, cols = "species")
#>    species
#>     <fctr>
#> 1:  Adelie
#> 2:  Adelie

b$nrow
#> [1] 344
head(b$rownames)
#> [1] 1 2 3 4 5 6

b$ncol
#> [1] 9
b$colnames
#> [1] "species"           "island"            "bill_length_mm"   
#> [4] "bill_depth_mm"     "flipper_length_mm" "body_mass_g"      
#> [7] "sex"               "year"              "id"               

# alternative construction
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
