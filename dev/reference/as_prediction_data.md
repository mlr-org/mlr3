# PredictionData

Convert object to a
[PredictionData](https://mlr3.mlr-org.com/dev/reference/PredictionData.md)
or a list of
[PredictionData](https://mlr3.mlr-org.com/dev/reference/PredictionData.md).

## Usage

``` r
as_prediction_data(x, task, row_ids = task$row_ids, check = TRUE, ...)

# S3 method for class 'Prediction'
as_prediction_data(x, task, row_ids = task$row_ids, check = TRUE, ...)

# S3 method for class 'PredictionData'
as_prediction_data(x, task, row_ids = task$row_ids, check = TRUE, ...)

# S3 method for class 'list'
as_prediction_data(
  x,
  task,
  row_ids = task$row_ids,
  check = TRUE,
  ...,
  train_task
)
```

## Arguments

- x:

  (any)  
  Object to convert.

- task:

  ([Task](https://mlr3.mlr-org.com/dev/reference/Task.md)).

- row_ids:

  [`integer()`](https://rdrr.io/r/base/integer.html)  
  Row indices.

- check:

  (`logical(1)`)  
  Perform argument checks and type conversions?

- ...:

  (any)  
  Additional arguments.

- train_task:

  ([Task](https://mlr3.mlr-org.com/dev/reference/Task.md))  
  Task used for training the learner.

## Value

[PredictionData](https://mlr3.mlr-org.com/dev/reference/PredictionData.md).
