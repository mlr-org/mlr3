# Elapsed Time Measure

Measures the elapsed time during train ("time_train"), predict
("time_predict"), or both ("time_both"). Aggregation of elapsed time
defaults to mean but can be configured via the field `aggregator` of the
[Measure](https://mlr3.mlr-org.com/dev/reference/Measure.md).

When predictions for multiple predict sets were made during
[`resample()`](https://mlr3.mlr-org.com/dev/reference/resample.md) or
[`benchmark()`](https://mlr3.mlr-org.com/dev/reference/benchmark.md),
the predict time shows the cumulative duration of all predictions. If
`learner$predict()` is called manually, the last predict time gets
overwritten. The elapsed time accounts only for the training duration of
the primary learner, excluding the time required for training the
fallback learner.

## Dictionary

This [Measure](https://mlr3.mlr-org.com/dev/reference/Measure.md) can be
instantiated via the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr_measures](https://mlr3.mlr-org.com/dev/reference/mlr_measures.md)
or with the associated sugar function
[`msr()`](https://mlr3.mlr-org.com/dev/reference/mlr_sugar.md):

    mlr_measures$get("time_train")
    msr("time_train")

## Meta Information

- Task type: “NA”

- Range: \\\[0, \infty)\\

- Minimize: TRUE

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
[`mlr_measures_internal_valid_score`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_internal_valid_score.md),
[`mlr_measures_oob_error`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_oob_error.md),
[`mlr_measures_regr.pinball`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_regr.pinball.md),
[`mlr_measures_regr.rqr`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_regr.rqr.md),
[`mlr_measures_regr.rsq`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_regr.rsq.md),
[`mlr_measures_selected_features`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_selected_features.md)

## Super class

[`mlr3::Measure`](https://mlr3.mlr-org.com/dev/reference/Measure.md) -\>
`MeasureElapsedTime`

## Public fields

- `stages`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Which stages of the learner to measure? Usually set during
  construction.

## Methods

### Public methods

- [`MeasureElapsedTime$new()`](#method-MeasureElapsedTime-new)

- [`MeasureElapsedTime$clone()`](#method-MeasureElapsedTime-clone)

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

    MeasureElapsedTime$new(id = "elapsed_time", stages)

#### Arguments

- `id`:

  (`character(1)`)  
  Identifier for the new instance.

- `stages`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Subset of `("train", "predict")`. The runtime of provided stages will
  be summed.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    MeasureElapsedTime$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
