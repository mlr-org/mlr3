# Convert to a Measure

Convert object to a
[Measure](https://mlr3.mlr-org.com/reference/Measure.md) or a list of
[Measure](https://mlr3.mlr-org.com/reference/Measure.md).

## Usage

``` r
as_measure(x, task_type = NULL, clone = FALSE, ...)

# S3 method for class '`NULL`'
as_measure(x, task_type = NULL, clone = FALSE, ...)

# S3 method for class 'Measure'
as_measure(x, task_type = NULL, clone = FALSE, ...)

as_measures(x, task_type = NULL, clone = FALSE, ...)

# Default S3 method
as_measures(x, task_type = NULL, clone = FALSE, ...)

# S3 method for class '`NULL`'
as_measures(x, task_type = NULL, clone = FALSE, ...)

# S3 method for class 'list'
as_measures(x, task_type = NULL, clone = FALSE, ...)
```

## Arguments

- x:

  (any)  
  Object to convert.

- task_type:

  (`character(1)`)  
  Used if `x` is `NULL` to construct a default measure for the
  respective task type. The default measures are stored in
  [`mlr_reflections$default_measures`](https://mlr3.mlr-org.com/reference/mlr_reflections.md).

- clone:

  (`logical(1)`)  
  If `TRUE`, ensures that the returned object is not the same as the
  input `x`.

- ...:

  (any)  
  Additional arguments.

## Value

[Measure](https://mlr3.mlr-org.com/reference/Measure.md).
