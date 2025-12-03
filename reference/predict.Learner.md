# Predict Method for Learners

Extends the generic
[`stats::predict()`](https://rdrr.io/r/stats/predict.html) with a method
for [Learner](https://mlr3.mlr-org.com/reference/Learner.md). Note that
this function is intended as glue code to be used in third party
packages. We recommend to work with the
[Learner](https://mlr3.mlr-org.com/reference/Learner.md) directly, i.e.
calling `learner$predict()` or `learner$predict_newdata()` directly.

Performs the following steps:

- Sets additional hyperparameters passed to this function.

- Creates a
  [Prediction](https://mlr3.mlr-org.com/reference/Prediction.md) object
  by calling `learner$predict_newdata()`.

- Returns (subset of)
  [Prediction](https://mlr3.mlr-org.com/reference/Prediction.md).

## Usage

``` r
# S3 method for class 'Learner'
predict(object, newdata, predict_type = NULL, ...)
```

## Arguments

- object:

  ([Learner](https://mlr3.mlr-org.com/reference/Learner.md))  
  Any [Learner](https://mlr3.mlr-org.com/reference/Learner.md).

- newdata:

  ([`data.frame()`](https://rdrr.io/r/base/data.frame.html))  
  New data to predict on.

- predict_type:

  (`character(1)`)  
  The predict type to return. Set to `<Prediction>` to retrieve the
  complete
  [Prediction](https://mlr3.mlr-org.com/reference/Prediction.md) object.
  If set to `NULL` (default), the first predict type for the respective
  class of the [Learner](https://mlr3.mlr-org.com/reference/Learner.md)
  as stored in
  [mlr_reflections](https://mlr3.mlr-org.com/reference/mlr_reflections.md)
  is used.

- ...:

  (any)  
  Hyperparameters to pass down to the
  [Learner](https://mlr3.mlr-org.com/reference/Learner.md).

## Examples

``` r
task = tsk("spam")

learner = lrn("classif.rpart", predict_type = "prob")
learner$train(task)
predict(learner, task$data(1:3), predict_type = "response")
#> [1] spam spam spam
#> Levels: spam nonspam
predict(learner, task$data(1:3), predict_type = "prob")
#>           spam    nonspam
#> [1,] 0.8513514 0.14864865
#> [2,] 0.9339623 0.06603774
#> [3,] 0.9339623 0.06603774
predict(learner, task$data(1:3), predict_type = "<Prediction>")
#> 
#> ── <PredictionClassif> for 3 observations: ─────────────────────────────────────
#>  row_ids truth response prob.spam prob.nonspam
#>        1  spam     spam 0.8513514   0.14864865
#>        2  spam     spam 0.9339623   0.06603774
#>        3  spam     spam 0.9339623   0.06603774
```
