# Get the Default Measure

Gets the default measures using the information in
[mlr_reflections\$default_measures](https://mlr3.mlr-org.com/dev/reference/mlr_reflections.md):

- [`"classif.ce"`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.ce.md)
  for classification (`"classif"`).

- [`"regr.mse"`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_regr.mse.md)
  for regression (`"regr"`).

- Add-on package may register additional default measures for their own
  task types.

## Usage

``` r
default_measures(task_type)
```

## Arguments

- task_type:

  (`character(1)`)  
  Get the default measure for the task type `task_type`, e.g.,
  `"classif"` or `"regr"`. If `task_type` is `NULL`, an empty list is
  returned.

## Value

list of [Measure](https://mlr3.mlr-org.com/dev/reference/Measure.md).

## Examples

``` r
default_measures("classif")
#> $classif.ce
#> 
#> ── <MeasureClassifSimple> (classif.ce): Classification Error ───────────────────
#> • Packages: mlr3 and mlr3measures
#> • Range: [0, 1]
#> • Minimize: TRUE
#> • Average: macro
#> • Parameters: list()
#> • Properties: weights
#> • Predict type: response
#> • Predict sets: test
#> • Aggregator: mean()
#> 
default_measures("regr")
#> $regr.mse
#> 
#> ── <MeasureRegrSimple> (regr.mse): Mean Squared Error ──────────────────────────
#> • Packages: mlr3 and mlr3measures
#> • Range: [0, Inf]
#> • Minimize: TRUE
#> • Average: macro
#> • Parameters: list()
#> • Properties: weights
#> • Predict type: response
#> • Predict sets: test
#> • Aggregator: mean()
#> 
```
