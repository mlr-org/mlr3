# Convert to a Classification Prediction

Convert object to a
[PredictionClassif](https://mlr3.mlr-org.com/dev/reference/PredictionClassif.md).

## Usage

``` r
as_prediction_classif(x, ...)

# S3 method for class 'PredictionClassif'
as_prediction_classif(x, ...)

# S3 method for class 'data.frame'
as_prediction_classif(x, ...)
```

## Arguments

- x:

  (any)  
  Object to convert.

- ...:

  (any)  
  Additional arguments.

## Value

[PredictionClassif](https://mlr3.mlr-org.com/dev/reference/PredictionClassif.md).

## Examples

``` r
# create a prediction object
task = tsk("penguins")
learner = lrn("classif.rpart", predict_type = "prob")
learner$train(task)
p = learner$predict(task)

# convert to a data.table
tab = as.data.table(p)

# convert back to a Prediction
as_prediction_classif(tab)
#> 
#> ── <PredictionClassif> for 344 observations: ───────────────────────────────────
#>  row_ids     truth  response prob.Adelie prob.Chinstrap prob.Gentoo
#>        1    Adelie    Adelie  0.96688742     0.03311258  0.00000000
#>        2    Adelie    Adelie  0.96688742     0.03311258  0.00000000
#>        3    Adelie    Adelie  0.96688742     0.03311258  0.00000000
#>      ---       ---       ---         ---            ---         ---
#>      342 Chinstrap Chinstrap  0.06349206     0.92063492  0.01587302
#>      343 Chinstrap Chinstrap  0.28571429     0.71428571  0.00000000
#>      344 Chinstrap Chinstrap  0.06349206     0.92063492  0.01587302

# split data.table into a list of data.tables
tabs = split(tab, tab$truth)

# convert back to list of predictions
preds = lapply(tabs, as_prediction_classif)

# calculate performance in each group
sapply(preds, function(p) p$score())
#>    Adelie.classif.ce Chinstrap.classif.ce    Gentoo.classif.ce 
#>          0.039473684          0.073529412          0.008064516 
```
