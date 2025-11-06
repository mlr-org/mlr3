# Column Information for Backend

Collects column information for backend.

Currently, this includes:

- storage type

- levels (factor / ordered), but not for the primary key column

## Usage

``` r
col_info(x, ...)

# S3 method for class 'data.table'
col_info(x, primary_key = character(), ...)

# S3 method for class 'DataBackend'
col_info(x, ...)
```

## Arguments

- x:

  (any)  
  A backend-like object for which to retrieve column information.

- ...:

  (any)  
  Additional arguments.

- primary_key:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  The primary key of the backend.
