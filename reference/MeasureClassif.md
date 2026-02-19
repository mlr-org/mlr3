# Classification Measure

This measure specializes
[Measure](https://mlr3.mlr-org.com/reference/Measure.md) for
classification problems:

- `task_type` is set to `"classif"`.

- Possible values for `predict_type` are `"response"` and `"prob"`.

Predefined measures can be found in the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr_measures](https://mlr3.mlr-org.com/reference/mlr_measures.md). The
default measure for classification is
[`classif.ce`](https://mlr3.mlr-org.com/reference/mlr_measures_classif.ce.md).

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
[`MeasureRegr`](https://mlr3.mlr-org.com/reference/MeasureRegr.md),
[`MeasureSimilarity`](https://mlr3.mlr-org.com/reference/MeasureSimilarity.md),
[`mlr_measures`](https://mlr3.mlr-org.com/reference/mlr_measures.md),
[`mlr_measures_aic`](https://mlr3.mlr-org.com/reference/mlr_measures_aic.md),
[`mlr_measures_bic`](https://mlr3.mlr-org.com/reference/mlr_measures_bic.md),
[`mlr_measures_classif.costs`](https://mlr3.mlr-org.com/reference/mlr_measures_classif.costs.md),
[`mlr_measures_debug_classif`](https://mlr3.mlr-org.com/reference/mlr_measures_debug_classif.md),
[`mlr_measures_elapsed_time`](https://mlr3.mlr-org.com/reference/mlr_measures_elapsed_time.md),
[`mlr_measures_internal_valid_score`](https://mlr3.mlr-org.com/reference/mlr_measures_internal_valid_score.md),
[`mlr_measures_oob_error`](https://mlr3.mlr-org.com/reference/mlr_measures_oob_error.md),
[`mlr_measures_regr.pinball`](https://mlr3.mlr-org.com/reference/mlr_measures_regr.pinball.md),
[`mlr_measures_regr.rqr`](https://mlr3.mlr-org.com/reference/mlr_measures_regr.rqr.md),
[`mlr_measures_regr.rsq`](https://mlr3.mlr-org.com/reference/mlr_measures_regr.rsq.md),
[`mlr_measures_selected_features`](https://mlr3.mlr-org.com/reference/mlr_measures_selected_features.md)

## Super class

[`mlr3::Measure`](https://mlr3.mlr-org.com/reference/Measure.md) -\>
`MeasureClassif`

## Methods

### Public methods

- [`MeasureClassif$new()`](#method-MeasureClassif-new)

- [`MeasureClassif$clone()`](#method-MeasureClassif-clone)

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

    MeasureClassif$new(
      id,
      param_set = ps(),
      range,
      minimize = NA,
      average = "macro",
      aggregator = NULL,
      properties = character(),
      predict_type = "response",
      predict_sets = "test",
      task_properties = character(),
      packages = character(),
      label = NA_character_,
      man = NA_character_
    )

#### Arguments

- `id`:

  (`character(1)`)  
  Identifier for the new instance.

- `param_set`:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html))  
  Set of hyperparameters.

- `range`:

  (`numeric(2)`)  
  Feasible range for this measure as `c(lower_bound, upper_bound)`. Both
  bounds may be infinite.

- `minimize`:

  (`logical(1)`)  
  Set to `TRUE` if good predictions correspond to small values, and to
  `FALSE` if good predictions correspond to large values. If set to `NA`
  (default), tuning this measure is not possible.

- `average`:

  (`character(1)`)  
  How to average multiple
  [Prediction](https://mlr3.mlr-org.com/reference/Prediction.md)s from a
  [ResampleResult](https://mlr3.mlr-org.com/reference/ResampleResult.md).

  The default, `"macro"`, calculates the individual performances scores
  for each
  [Prediction](https://mlr3.mlr-org.com/reference/Prediction.md) and
  then uses the function defined in `$aggregator` to average them to a
  single number.

  `"macro_weighted"` is similar to `"macro"`, but uses weighted
  averages. Weights are taken from the `weights_measure` column of the
  resampled [Task](https://mlr3.mlr-org.com/reference/Task.md) if
  present. Note that `"macro_weighted"` can differ from `"macro"` even
  if no weights are present or if `$use_weights` is set to `"ignore"`,
  since then aggregation is done using *uniform sample weights*, which
  result in non-uniform weights for
  [Prediction](https://mlr3.mlr-org.com/reference/Prediction.md)s if
  they contain different numbers of samples.

  If set to `"micro"`, the individual
  [Prediction](https://mlr3.mlr-org.com/reference/Prediction.md) objects
  are first combined into a single new
  [Prediction](https://mlr3.mlr-org.com/reference/Prediction.md) object
  which is then used to assess the performance. The function in
  `$aggregator` is not used in this case.

- `aggregator`:

  (`function()` \| `NULL`)  
  Function to aggregate over multiple iterations. The role of this
  function depends on the value of field `"average"`:

  - `"macro"`: A numeric vector of scores (one per iteration) is passed.
    The aggregate function defaults to
    [`mean()`](https://rdrr.io/r/base/mean.html) in this case.

  - `"micro"`: The `aggregator` function is not used. Instead,
    predictions from multiple iterations are first combined and then
    scored in one go.

  - `"custom"`: A
    [ResampleResult](https://mlr3.mlr-org.com/reference/ResampleResult.md)
    is passed to the aggregate function.

- `properties`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Properties of the measure. Must be a subset of
  [mlr_reflections\$measure_properties](https://mlr3.mlr-org.com/reference/mlr_reflections.md).
  Supported by `mlr3`:

  - `"requires_task"` (requires the complete
    [Task](https://mlr3.mlr-org.com/reference/Task.md)),

  - `"requires_learner"` (requires the trained
    [Learner](https://mlr3.mlr-org.com/reference/Learner.md)),

  - `"requires_model"` (requires the trained
    [Learner](https://mlr3.mlr-org.com/reference/Learner.md), including
    the fitted model),

  - `"requires_train_set"` (requires the training indices from the
    [Resampling](https://mlr3.mlr-org.com/reference/Resampling.md)),

  - `"na_score"` (the measure is expected to occasionally return `NA` or
    `NaN`),

  - `"weights"` (support weighted scoring using sample weights from
    task, column role `weights_measure`), and

  - `"primary_iters"` (the measure explictly handles resamplings that
    only use a subset of their iterations for the point estimate)

  - `"requires_no_prediction"` (No prediction is required; This usually
    means that the measure extracts some information from the learner
    state.).

- `predict_type`:

  (`character(1)`)  
  Required predict type of the
  [Learner](https://mlr3.mlr-org.com/reference/Learner.md). Possible
  values are stored in
  [mlr_reflections\$learner_predict_types](https://mlr3.mlr-org.com/reference/mlr_reflections.md).

- `predict_sets`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Prediction sets to operate on, used in
  [`aggregate()`](https://rdrr.io/r/stats/aggregate.html) to extract the
  matching `predict_sets` from the
  [ResampleResult](https://mlr3.mlr-org.com/reference/ResampleResult.md).
  Multiple predict sets are calculated by the respective
  [Learner](https://mlr3.mlr-org.com/reference/Learner.md) during
  [`resample()`](https://mlr3.mlr-org.com/reference/resample.md)/[`benchmark()`](https://mlr3.mlr-org.com/reference/benchmark.md).
  Must be a non-empty subset of `{"train", "test", "internal_valid"}`.
  If multiple sets are provided, these are first combined to a single
  prediction object. Default is `"test"`.

- `task_properties`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Required task properties, see
  [Task](https://mlr3.mlr-org.com/reference/Task.md).

- `packages`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Set of required packages. A warning is signaled by the constructor if
  at least one of the packages is not installed, but loaded (not
  attached) later on-demand via
  [`requireNamespace()`](https://rdrr.io/r/base/ns-load.html).

- `label`:

  (`character(1)`)  
  Label for the new instance.

- `man`:

  (`character(1)`)  
  String in the format `[pkg]::[topic]` pointing to a manual page for
  this object. The referenced help package can be opened via method
  `$help()`.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    MeasureClassif$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
