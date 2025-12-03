# Convert to a Resampling

Convert object to a
[Resampling](https://mlr3.mlr-org.com/reference/Resampling.md) or a list
of [Resampling](https://mlr3.mlr-org.com/reference/Resampling.md). This
method e.g. allows to convert an `OMLTask` of
[mlr3oml](https://CRAN.R-project.org/package=mlr3oml) to a
[`Resampling`](https://mlr3.mlr-org.com/reference/Resampling.md).

## Usage

``` r
as_resampling(x, ...)

# S3 method for class 'Resampling'
as_resampling(x, clone = FALSE, ...)

as_resamplings(x, ...)

# Default S3 method
as_resamplings(x, ...)

# S3 method for class 'list'
as_resamplings(x, ...)
```

## Arguments

- x:

  (any)  
  Object to convert.

- ...:

  (any)  
  Additional arguments.

- clone:

  (`logical(1)`)  
  If `TRUE`, ensures that the returned object is not the same as the
  input `x`.
