# Convert to a Regression Prediction

Convert object to a
[PredictionRegr](https://mlr3.mlr-org.com/dev/reference/PredictionRegr.md).

## Usage

``` r
as_prediction_regr(x, ...)

# S3 method for class 'PredictionRegr'
as_prediction_regr(x, ...)

# S3 method for class 'data.frame'
as_prediction_regr(x, ...)
```

## Arguments

- x:

  (any)  
  Object to convert.

- ...:

  (any)  
  Additional arguments.

## Value

[PredictionRegr](https://mlr3.mlr-org.com/dev/reference/PredictionRegr.md).

## Examples

``` r
# create a prediction object
task = tsk("mtcars")
learner = lrn("regr.rpart")
learner$train(task)
p = learner$predict(task)

# convert to a data.table
tab = as.data.table(p)

# convert back to a Prediction
as_prediction_regr(tab)
#> 
#> ── <PredictionRegr> for 32 observations: ───────────────────────────────────────
#>  row_ids truth response
#>        1  21.0 18.26429
#>        2  21.0 18.26429
#>        3  22.8 26.66364
#>      ---   ---      ---
#>       30  19.7 18.26429
#>       31  15.0 13.41429
#>       32  21.4 26.66364

# split data.table into a list of data.tables
tabs = split(tab, cut(tab$truth, 3))

# convert back to list of predictions
preds = lapply(tabs, as_prediction_regr)

# calculate performance in each group
sapply(preds, function(p) p$score())
#> (10.4,18.2].regr.mse (18.2,26.1].regr.mse (26.1,33.9].regr.mse 
#>             4.278393             9.122466            22.719322 
```
