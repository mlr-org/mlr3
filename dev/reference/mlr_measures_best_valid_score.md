# Measure Best Validation Score

Returns the selected best internal validation score of the
[Learner](https://mlr3.mlr-org.com/dev/reference/Learner.md). This is
only available for learners that have both the `"validation"` and the
`"internal_tuning"` property, because tracking a best iteration only
makes sense for learners that iterate. Returns `NA` for unsupported
learners, when no validation was done, or when the selected id was not
found. The `id` of this measure is set to the value of `select` if
provided.

While
[`msr("internal_valid_score")`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_internal_valid_score.md)
reports the validation score of the *final* model, this measure reports
the *best* validation score observed during training.

Some learners automatically use the best found model for prediction
instead of the one from the last iteration. For those the two measures
report the same value.

## Dictionary

This [Measure](https://mlr3.mlr-org.com/dev/reference/Measure.md) can be
instantiated via the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr_measures](https://mlr3.mlr-org.com/dev/reference/mlr_measures.md)
or with the associated sugar function
[`msr()`](https://mlr3.mlr-org.com/dev/reference/mlr_sugar.md):

    mlr_measures$get("best_valid_score")
    msr("best_valid_score")

## Meta Information

- Task type: “NA”

- Range: \\(-\infty, \infty)\\

- Minimize: NA

- Average: macro

- Required Prediction: “NA”

- Required Packages: [mlr3](https://CRAN.R-project.org/package=mlr3)

## Parameters

Empty ParamSet

## See also

- Chapter in the [mlr3book](https://mlr3book.mlr-org.com/):
  <https://mlr3book.mlr-org.com/chapters/chapter2/data_and_basic_modeling.html#sec-eval>

- Package
  [mlr3measures](https://CRAN.R-project.org/package=mlr3measures) for
  the scoring functions.
  [Dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
  of [Measures](https://mlr3.mlr-org.com/dev/reference/Measure.md):
  [mlr_measures](https://mlr3.mlr-org.com/dev/reference/mlr_measures.md)
  `as.data.table(mlr_measures)` for a table of available
  [Measures](https://mlr3.mlr-org.com/dev/reference/Measure.md) in the
  running session (depending on the loaded packages).

- Extension packages for additional task types:

  - [mlr3proba](https://CRAN.R-project.org/package=mlr3proba) for
    probabilistic supervised regression and survival analysis.

  - [mlr3cluster](https://CRAN.R-project.org/package=mlr3cluster) for
    unsupervised clustering.

Other Measure:
[`Measure`](https://mlr3.mlr-org.com/dev/reference/Measure.md),
[`MeasureClassif`](https://mlr3.mlr-org.com/dev/reference/MeasureClassif.md),
[`MeasureRegr`](https://mlr3.mlr-org.com/dev/reference/MeasureRegr.md),
[`MeasureSimilarity`](https://mlr3.mlr-org.com/dev/reference/MeasureSimilarity.md),
[`mlr_measures`](https://mlr3.mlr-org.com/dev/reference/mlr_measures.md),
[`mlr_measures_aic`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_aic.md),
[`mlr_measures_bic`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_bic.md),
[`mlr_measures_classif.costs`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.costs.md),
[`mlr_measures_debug_classif`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_debug_classif.md),
[`mlr_measures_elapsed_time`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_elapsed_time.md),
[`mlr_measures_internal_valid_score`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_internal_valid_score.md),
[`mlr_measures_oob_error`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_oob_error.md),
[`mlr_measures_regr.pinball`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_regr.pinball.md),
[`mlr_measures_regr.rqr`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_regr.rqr.md),
[`mlr_measures_regr.rsq`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_regr.rsq.md),
[`mlr_measures_selected_features`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_selected_features.md)

## Super classes

[`Measure`](https://mlr3.mlr-org.com/dev/reference/Measure.md) -\>
`MeasureValidScore` -\> `MeasureBestValidScore`

## Methods

### Public methods

- [`MeasureBestValidScore$new()`](#method-MeasureBestValidScore-initialize)

- [`MeasureBestValidScore$clone()`](#method-MeasureBestValidScore-clone)

Inherited methods

- [`Measure$aggregate()`](https://mlr3.mlr-org.com/dev/reference/Measure.html#method-aggregate)
- [`Measure$format()`](https://mlr3.mlr-org.com/dev/reference/Measure.html#method-format)
- [`Measure$help()`](https://mlr3.mlr-org.com/dev/reference/Measure.html#method-help)
- [`Measure$obs_loss()`](https://mlr3.mlr-org.com/dev/reference/Measure.html#method-obs_loss)
- [`Measure$print()`](https://mlr3.mlr-org.com/dev/reference/Measure.html#method-print)
- [`Measure$score()`](https://mlr3.mlr-org.com/dev/reference/Measure.html#method-score)

------------------------------------------------------------------------

### `MeasureBestValidScore$new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    MeasureBestValidScore$new(select = NULL, minimize = NA)

#### Arguments

- `select`:

  (`character(1)`)  
  Which of the best validation scores to select. Which scores are
  available depends on the learner and its configuration. By default,
  the first score is chosen.

- `minimize`:

  (`logical(1)`)  
  Whether smaller values are better. Must be set to use for tuning.

------------------------------------------------------------------------

### `MeasureBestValidScore$clone()`

The objects of this class are cloneable with this method.

#### Usage

    MeasureBestValidScore$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
rr = resample(tsk("iris"), lrn("classif.debug", validate = 0.3), rsmp("holdout"))
rr$score(msr("best_valid_score", select = "acc"))
#>    task_id    learner_id resampling_id iteration       acc
#>     <char>        <char>        <char>     <int>     <num>
#> 1:    iris classif.debug       holdout         1 0.3666667
#> Hidden columns: task, learner, resampling, prediction_test
```
