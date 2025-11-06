# Convert to a Prediction

Convert object to a
[Prediction](https://mlr3.mlr-org.com/dev/reference/Prediction.md) or a
list of
[Prediction](https://mlr3.mlr-org.com/dev/reference/Prediction.md).

## Usage

``` r
as_prediction(x, check = FALSE, ...)

# S3 method for class 'Prediction'
as_prediction(x, check = FALSE, ...)

# S3 method for class 'PredictionDataClassif'
as_prediction(x, check = FALSE, ...)

# S3 method for class 'PredictionDataRegr'
as_prediction(x, check = FALSE, ...)

as_predictions(x, predict_sets = "test", ...)

# S3 method for class 'list'
as_predictions(x, predict_sets = "test", ...)
```

## Arguments

- x:

  (any)  
  Object to convert.

- check:

  (`logical(1)`)  
  Perform argument checks and type conversions?

- ...:

  (any)  
  Additional arguments.

- predict_sets:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Prediction sets to operate on, used in
  [`aggregate()`](https://rdrr.io/r/stats/aggregate.html) to extract the
  matching `predict_sets` from the
  [ResampleResult](https://mlr3.mlr-org.com/dev/reference/ResampleResult.md).
  Multiple predict sets are calculated by the respective
  [Learner](https://mlr3.mlr-org.com/dev/reference/Learner.md) during
  [`resample()`](https://mlr3.mlr-org.com/dev/reference/resample.md)/[`benchmark()`](https://mlr3.mlr-org.com/dev/reference/benchmark.md).
  Must be a non-empty subset of `{"train", "test", "internal_valid"}`.
  If multiple sets are provided, these are first combined to a single
  prediction object. Default is `"test"`.

## Value

[Prediction](https://mlr3.mlr-org.com/dev/reference/Prediction.md).
