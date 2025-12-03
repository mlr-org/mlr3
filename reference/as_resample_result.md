# Convert to ResampleResult

Convert object to a
[ResampleResult](https://mlr3.mlr-org.com/reference/ResampleResult.md).

The S3 method for `list` expects argument `x` to be a list of
[Prediction](https://mlr3.mlr-org.com/reference/Prediction.md) objects
and all other relevant objects
([Task](https://mlr3.mlr-org.com/reference/Task.md),
[Learner](https://mlr3.mlr-org.com/reference/Learner.md)s, and
instantiated
[Resampling](https://mlr3.mlr-org.com/reference/Resampling.md)) must be
provided, too. A more flexible way to manually create a
[ResampleResult](https://mlr3.mlr-org.com/reference/ResampleResult.md)
is implemented in
[`as_result_data()`](https://mlr3.mlr-org.com/reference/as_result_data.md).

## Usage

``` r
as_resample_result(x, ...)

# S3 method for class 'ResampleResult'
as_resample_result(x, ...)

# S3 method for class 'ResultData'
as_resample_result(x, view = NULL, ...)

# S3 method for class 'list'
as_resample_result(x, task, learners, resampling, store_backends = TRUE, ...)
```

## Arguments

- x:

  (any)  
  Object to convert.

- ...:

  (any)  
  Currently not used.

- view:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  See construction argument `view` of
  [`ResampleResult`](https://mlr3.mlr-org.com/reference/ResampleResult.md).

- task:

  ([Task](https://mlr3.mlr-org.com/reference/Task.md)).

- learners:

  (list of trained
  [Learner](https://mlr3.mlr-org.com/reference/Learner.md)s).

- resampling:

  ([Resampling](https://mlr3.mlr-org.com/reference/Resampling.md)).

- store_backends:

  (`logical(1)`)  
  If set to `FALSE`, the backends of the
  [Task](https://mlr3.mlr-org.com/reference/Task.md)s provided in `data`
  are removed.

## Value

([ResampleResult](https://mlr3.mlr-org.com/reference/ResampleResult.md)).
