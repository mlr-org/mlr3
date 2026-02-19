# DataBackend

This is the abstract base class for data backends.

Data backends provide a layer of abstraction for various data storage
systems. It is not recommended to work directly with the DataBackend.
Instead, all data access is handled transparently via the
[Task](https://mlr3.mlr-org.com/reference/Task.md).

This package currently ships with one implementation for backends:

- [DataBackendDataTable](https://mlr3.mlr-org.com/reference/DataBackendDataTable.md)
  which stores the data as
  [`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html).

To connect to out-of-memory database management systems such as SQL
servers, see the extension package
[mlr3db](https://CRAN.R-project.org/package=mlr3db).

## Details

The required set of fields and methods to implement a custom
`DataBackend` is listed in the respective sections (see
[DataBackendDataTable](https://mlr3.mlr-org.com/reference/DataBackendDataTable.md)).

## See also

- Chapter in the [mlr3book](https://mlr3book.mlr-org.com/):
  <https://mlr3book.mlr-org.com/chapters/chapter10/advanced_technical_aspects_of_mlr3.html#sec-backends>

- Package [mlr3db](https://CRAN.R-project.org/package=mlr3db) to
  interface out-of-memory data, e.g. SQL servers or
  [duckdb](https://CRAN.R-project.org/package=duckdb).

Other DataBackend:
[`DataBackendDataTable`](https://mlr3.mlr-org.com/reference/DataBackendDataTable.md),
[`as_data_backend()`](https://mlr3.mlr-org.com/reference/as_data_backend.md)

## Public fields

- `primary_key`:

  (`character(1)`)  
  Column name of the primary key column of positive and unique integer
  row ids.

## Active bindings

- `hash`:

  (`character(1)`)  
  Hash (unique identifier) for this object.

- `col_hashes`:

  (named `character`)  
  Hash (unique identifier) for all columns except the `primary_key`: A
  `character` vector, named by the columns that each element refers
  to.  
  Columns of different
  [`Task`](https://mlr3.mlr-org.com/reference/Task.md)s or
  `DataBackend`s that have agreeing `col_hashes` always represent the
  same data, given that the same `row`s are selected. The reverse is not
  necessarily true: There can be columns with the same content that have
  different `col_hashes`.

## Methods

### Public methods

- [`DataBackend$new()`](#method-DataBackend-new)

- [`DataBackend$format()`](#method-DataBackend-format)

- [`DataBackend$print()`](#method-DataBackend-print)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

Note: This object is typically constructed via a derived classes, e.g.
[DataBackendDataTable](https://mlr3.mlr-org.com/reference/DataBackendDataTable.md),
or via the S3 method
[`as_data_backend()`](https://mlr3.mlr-org.com/reference/as_data_backend.md).

#### Usage

    DataBackend$new(data, primary_key)

#### Arguments

- `data`:

  (any)  
  The format of the input data depends on the specialization. E.g.,
  [DataBackendDataTable](https://mlr3.mlr-org.com/reference/DataBackendDataTable.md)
  expects a
  [`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html).

- `primary_key`:

  (`character(1)`)  
  Each DataBackend needs a way to address rows, which is done via a
  column of unique integer values, referenced here by `primary_key`. The
  use of this variable may differ between backends.

------------------------------------------------------------------------

### Method [`format()`](https://rdrr.io/r/base/format.html)

Helper for print outputs.

#### Usage

    DataBackend$format(...)

#### Arguments

- `...`:

  (ignored).

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Printer.

#### Usage

    DataBackend$print()

## Examples

``` r
data = data.table::data.table(id = 1:5, x = runif(5),
  y = sample(letters[1:3], 5, replace = TRUE))

b = DataBackendDataTable$new(data, primary_key = "id")
print(b)
#> 
#> ── <DataBackendDataTable> (5x3) ────────────────────────────────────────────────
#>     id          x      y
#>  <int>      <num> <char>
#>      1 0.74678791      a
#>      2 0.41931352      b
#>      3 0.58214390      a
#>      4 0.83073406      a
#>      5 0.01353063      a
b$head(2)
#> Key: <id>
#>       id         x      y
#>    <int>     <num> <char>
#> 1:     1 0.7467879      a
#> 2:     2 0.4193135      b
b$data(rows = 1:2, cols = "x")
#>            x
#>        <num>
#> 1: 0.7467879
#> 2: 0.4193135
b$distinct(rows = b$rownames, "y")
#> $y
#> [1] "a" "b"
#> 
b$missings(rows = b$rownames, cols = names(data))
#> id  x  y 
#>  0  0  0 
```
