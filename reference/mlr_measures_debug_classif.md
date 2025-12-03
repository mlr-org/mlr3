# Debug Measure for Classification

This measure returns the number of observations in the
[PredictionClassif](https://mlr3.mlr-org.com/reference/PredictionClassif.md)
object. Its main purpose is debugging. The parameter `na_ratio`
(`numeric(1)`) controls the ratio of scores which randomly are set to
`NA`, between 0 (default) and 1.

## Dictionary

This [Measure](https://mlr3.mlr-org.com/reference/Measure.md) can be
instantiated via the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr_measures](https://mlr3.mlr-org.com/reference/mlr_measures.md) or
with the associated sugar function
[`msr()`](https://mlr3.mlr-org.com/reference/mlr_sugar.md):

    mlr_measures$get("debug_classif")
    msr("debug_classif")

## Meta Information

- Task type: “NA”

- Range: \\\[0, \infty)\\

- Minimize: NA

- Average: macro

- Required Prediction: “response”

- Required Packages: [mlr3](https://CRAN.R-project.org/package=mlr3)

## Parameters

|          |         |         |              |
|----------|---------|---------|--------------|
| Id       | Type    | Default | Range        |
| na_ratio | numeric | \-      | \\\[0, 1\]\\ |

## See also

- Chapter in the [mlr3book](https://mlr3book.mlr-org.com/):
  <https://mlr3book.mlr-org.com/chapters/chapter2/data_and_basic_modeling.html#sec-eval>

- Package
  [mlr3measures](https://CRAN.R-project.org/package=mlr3measures) for
  the scoring functions.
  [Dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
  of [Measures](https://mlr3.mlr-org.com/reference/Measure.md):
  [mlr_measures](https://mlr3.mlr-org.com/reference/mlr_measures.md)
  `as.data.table(mlr_measures)` for a table of available
  [Measures](https://mlr3.mlr-org.com/reference/Measure.md) in the
  running session (depending on the loaded packages).

- Extension packages for additional task types:

  - [mlr3proba](https://CRAN.R-project.org/package=mlr3proba) for
    probabilistic supervised regression and survival analysis.

  - [mlr3cluster](https://CRAN.R-project.org/package=mlr3cluster) for
    unsupervised clustering.

Other Measure:
[`Measure`](https://mlr3.mlr-org.com/reference/Measure.md),
[`MeasureClassif`](https://mlr3.mlr-org.com/reference/MeasureClassif.md),
[`MeasureRegr`](https://mlr3.mlr-org.com/reference/MeasureRegr.md),
[`MeasureSimilarity`](https://mlr3.mlr-org.com/reference/MeasureSimilarity.md),
[`mlr_measures`](https://mlr3.mlr-org.com/reference/mlr_measures.md),
[`mlr_measures_aic`](https://mlr3.mlr-org.com/reference/mlr_measures_aic.md),
[`mlr_measures_bic`](https://mlr3.mlr-org.com/reference/mlr_measures_bic.md),
[`mlr_measures_classif.costs`](https://mlr3.mlr-org.com/reference/mlr_measures_classif.costs.md),
[`mlr_measures_elapsed_time`](https://mlr3.mlr-org.com/reference/mlr_measures_elapsed_time.md),
[`mlr_measures_internal_valid_score`](https://mlr3.mlr-org.com/reference/mlr_measures_internal_valid_score.md),
[`mlr_measures_oob_error`](https://mlr3.mlr-org.com/reference/mlr_measures_oob_error.md),
[`mlr_measures_regr.pinball`](https://mlr3.mlr-org.com/reference/mlr_measures_regr.pinball.md),
[`mlr_measures_regr.rqr`](https://mlr3.mlr-org.com/reference/mlr_measures_regr.rqr.md),
[`mlr_measures_regr.rsq`](https://mlr3.mlr-org.com/reference/mlr_measures_regr.rsq.md),
[`mlr_measures_selected_features`](https://mlr3.mlr-org.com/reference/mlr_measures_selected_features.md)

## Super class

[`mlr3::Measure`](https://mlr3.mlr-org.com/reference/Measure.md) -\>
`MeasureDebugClassif`

## Methods

### Public methods

- [`MeasureDebugClassif$new()`](#method-MeasureDebugClassif-new)

- [`MeasureDebugClassif$clone()`](#method-MeasureDebugClassif-clone)

Inherited methods

- [`mlr3::Measure$aggregate()`](https://mlr3.mlr-org.com/reference/Measure.html#method-aggregate)
- [`mlr3::Measure$format()`](https://mlr3.mlr-org.com/reference/Measure.html#method-format)
- [`mlr3::Measure$help()`](https://mlr3.mlr-org.com/reference/Measure.html#method-help)
- [`mlr3::Measure$obs_loss()`](https://mlr3.mlr-org.com/reference/Measure.html#method-obs_loss)
- [`mlr3::Measure$print()`](https://mlr3.mlr-org.com/reference/Measure.html#method-print)
- [`mlr3::Measure$score()`](https://mlr3.mlr-org.com/reference/Measure.html#method-score)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    MeasureDebugClassif$new()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    MeasureDebugClassif$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
task = tsk("wine")
learner = lrn("classif.featureless")
measure = msr("debug_classif", na_ratio = 0.5)
rr = resample(task, learner, rsmp("cv", folds = 5))
rr$score(measure)
#>    task_id          learner_id resampling_id iteration debug_classif
#>     <char>              <char>        <char>     <int>         <num>
#> 1:    wine classif.featureless            cv         1            36
#> 2:    wine classif.featureless            cv         2            NA
#> 3:    wine classif.featureless            cv         3            36
#> 4:    wine classif.featureless            cv         4            35
#> 5:    wine classif.featureless            cv         5            35
#> Hidden columns: task, learner, resampling, prediction_test
```
