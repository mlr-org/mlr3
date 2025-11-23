# Measure Internal Validation Score

Returns the selected internal validation score of the
[Learner](https://mlr3.mlr-org.com/dev/reference/Learner.md) for
learners with property `"validation"`. Returns `NA` for unsupported
learners, when no validation was done, or when the selected id was not
found. The `id` of this measure is set to the value of `select` if
provided.

## Dictionary

This [Measure](https://mlr3.mlr-org.com/dev/reference/Measure.md) can be
instantiated via the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr_measures](https://mlr3.mlr-org.com/dev/reference/mlr_measures.md)
or with the associated sugar function
[`msr()`](https://mlr3.mlr-org.com/dev/reference/mlr_sugar.md):

    mlr_measures$get("internal_valid_score")
    msr("internal_valid_score")

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
[`mlr_measures_oob_error`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_oob_error.md),
[`mlr_measures_regr.pinball`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_regr.pinball.md),
[`mlr_measures_regr.rqr`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_regr.rqr.md),
[`mlr_measures_regr.rsq`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_regr.rsq.md),
[`mlr_measures_selected_features`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_selected_features.md)

## Super class

[`mlr3::Measure`](https://mlr3.mlr-org.com/dev/reference/Measure.md) -\>
`MeasureInternalValidScore`

## Methods

### Public methods

- [`MeasureInternalValidScore$new()`](#method-MeasureInternalValidScore-new)

- [`MeasureInternalValidScore$clone()`](#method-MeasureInternalValidScore-clone)

Inherited methods

- [`mlr3::Measure$aggregate()`](https://mlr3.mlr-org.com/dev/reference/Measure.html#method-aggregate)
- [`mlr3::Measure$format()`](https://mlr3.mlr-org.com/dev/reference/Measure.html#method-format)
- [`mlr3::Measure$help()`](https://mlr3.mlr-org.com/dev/reference/Measure.html#method-help)
- [`mlr3::Measure$obs_loss()`](https://mlr3.mlr-org.com/dev/reference/Measure.html#method-obs_loss)
- [`mlr3::Measure$print()`](https://mlr3.mlr-org.com/dev/reference/Measure.html#method-print)
- [`mlr3::Measure$score()`](https://mlr3.mlr-org.com/dev/reference/Measure.html#method-score)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    MeasureInternalValidScore$new(select = NULL, minimize = NA)

#### Arguments

- `select`:

  (`character(1)`)  
  Which of the internal validation scores to select. Which scores are
  available depends on the learner and its configuration. By default,
  the first score is chosen.

- `minimize`:

  (`logical(1)`)  
  Whether smaller values are better. Must be set to use for tuning.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    MeasureInternalValidScore$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
rr = resample(tsk("iris"), lrn("classif.debug", validate = 0.3), rsmp("holdout"))
rr$score(msr("internal_valid_score", select = "acc"))
#>    task_id    learner_id resampling_id iteration       acc
#>     <char>        <char>        <char>     <int>     <num>
#> 1:    iris classif.debug       holdout         1 0.3333333
#> Hidden columns: task, learner, resampling, prediction_test
```
