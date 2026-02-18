# Measure Class

This is the abstract base class for measures like
[MeasureClassif](https://mlr3.mlr-org.com/dev/reference/MeasureClassif.md)
and
[MeasureRegr](https://mlr3.mlr-org.com/dev/reference/MeasureRegr.md).

Measures are classes tailored around two functions doing the work:

1.  A function `$score()` which quantifies the performance on a
    [Prediction](https://mlr3.mlr-org.com/dev/reference/Prediction.md)
    object, so a set of predicted observation via a scalar number –
    usually an aggregate of losses on the contained observations, by
    comparing the truth and prediction columns in the prediction object.

2.  A function `$aggregator()` which combines multiple performance
    scores returned by `$score()` obtained in different resampling
    iterations to a scalar performance value associated with the
    complete resampling – usually by averaging or summing.

In addition to these two functions, meta-information about the
performance measure is stored.

Predefined measures are stored in the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr_measures](https://mlr3.mlr-org.com/dev/reference/mlr_measures.md),
e.g.
[`classif.auc`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.auc.md)
or
[`time_train`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_elapsed_time.md).
Many of the measures in mlr3 are implemented in
[mlr3measures](https://CRAN.R-project.org/package=mlr3measures) as
ordinary functions.

A guide on how to extend [mlr3](https://CRAN.R-project.org/package=mlr3)
with custom measures can be found in the
[mlr3book](https://mlr3book.mlr-org.com).

## Inheriting

For some measures (such as confidence intervals from `mlr3inferr`) it is
necessary that a measure returns more than one value. In such cases it
is necessary to overwrite the public methods `$aggregate()` and/or
`$score()` to return a named
[`numeric()`](https://rdrr.io/r/base/numeric.html) where at least one of
its names corresponds to the `id` of the measure itself.

## Weights

Many measures support observation weights, indicated by their property
`"weights"`. The weights are stored in the
[Task](https://mlr3.mlr-org.com/dev/reference/Task.md) where the column
role `weights_measure` needs to be assigned to a single numeric column.
The weights are automatically used if found in the task, this can be
disabled by setting the field `use_weights` to `"ignore"`. See the
description of `use_weights` for more information.

If the measure is set-up to use weights but the task does not have a
designated `weights_measure` column, an unweighted version is calculated
instead. The weights do not necessarily need to sum up to 1, they are
normalized by the measure if necessary.

Most measures are so-called decomposable loss functions where a
point-wise loss is computed and then either mean-aggregated or summed
over the test set. For measures that do mean-aggregation, weights are
typically used to compute the weighted mean, which normalizes weights to
sum to 1. Measures that use sum-aggregation do not normalize weights and
instead multiply individual losses with the given weights. See the
documentation of specific measures for more details.

## Missing Values during Scoring

Many measurements cannot be calculated if the test set or predictions
are unfortunate, for example because a denominator is 0. This typically
occurs during (binary) classification if some entries of the confusion
matrix are 0. For this reason, many measures which originate in
[mlr3measures](https://CRAN.R-project.org/package=mlr3measures) allow to
change the default missing value (`NaN`) via the field `na_value`.

If you encounter missing values in a compound object like a
[ResampleResult](https://mlr3.mlr-org.com/dev/reference/ResampleResult.md)
or
[BenchmarkResult](https://mlr3.mlr-org.com/dev/reference/BenchmarkResult.md)
during scoring or aggregating, simply removing iterations with missing
values is statistically arguable (but technically possible by providing
a custom aggregation function which handles missing values, e.g.
`function(x) mean(x, na.rm = TRUE)`). Instead, consider stratification
on the target of the
[Task](https://mlr3.mlr-org.com/dev/reference/Task.md) to work around
missing values. Switching to micro averaging in the
[Resampling](https://mlr3.mlr-org.com/dev/reference/Resampling.md) can
also be a solution here.

## See also

- Chapter in the [mlr3book](https://mlr3book.mlr-org.com/):
  <https://mlr3book.mlr-org.com/chapters/chapter2/data_and_basic_modeling.html#sec-eval>

- Package
  [mlr3measures](https://CRAN.R-project.org/package=mlr3measures) for
  the scoring functions.
  [Dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
  of Measures:
  [mlr_measures](https://mlr3.mlr-org.com/dev/reference/mlr_measures.md)
  `as.data.table(mlr_measures)` for a table of available Measures in the
  running session (depending on the loaded packages).

- Extension packages for additional task types:

  - [mlr3proba](https://CRAN.R-project.org/package=mlr3proba) for
    probabilistic supervised regression and survival analysis.

  - [mlr3cluster](https://CRAN.R-project.org/package=mlr3cluster) for
    unsupervised clustering.

Other Measure:
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

## Active bindings

- `predict_sets`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  During
  [`resample()`](https://mlr3.mlr-org.com/dev/reference/resample.md)/[`benchmark()`](https://mlr3.mlr-org.com/dev/reference/benchmark.md),
  a [Learner](https://mlr3.mlr-org.com/dev/reference/Learner.md) can
  predict on multiple sets. Per default, a learner only predicts
  observations in the test set (`predict_sets == "test"`). To change
  this behavior, set `predict_sets` to a non-empty subset of
  `{"train", "test", "internal_valid"}`. The `"train"` predict set
  contains the train ids from the resampling. This means that if a
  learner does validation and sets `$validate` to a ratio (creating the
  validation data from the training data), the train predictions will
  include the predictions for the validation data. Each set yields a
  separate
  [Prediction](https://mlr3.mlr-org.com/dev/reference/Prediction.md)
  object. Those can be combined via getters in
  [ResampleResult](https://mlr3.mlr-org.com/dev/reference/ResampleResult.md)/[BenchmarkResult](https://mlr3.mlr-org.com/dev/reference/BenchmarkResult.md),
  or Measures can be configured to operate on specific subsets of the
  calculated prediction sets.

- `hash`:

  (`character(1)`)  
  Hash (unique identifier) for this object. The hash is calculated based
  on the id, the parameter settings, predict sets and the `$score`,
  `$average`, `$aggregator`, `$obs_loss`, `$trafo` method. Measure can
  define additional fields to be included in the hash by setting the
  field `$.extra_hash`.

- `properties`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Properties of this measure.

- `average`:

  (`character(1)`)  
  Method for aggregation:

  - `"micro"`: All predictions from multiple resampling iterations are
    first combined into a single
    [Prediction](https://mlr3.mlr-org.com/dev/reference/Prediction.md)
    object. Next, the scoring function of the measure is applied on this
    combined object, yielding a single numeric score.

  - `"macro"`: The scoring function is applied on the
    [Prediction](https://mlr3.mlr-org.com/dev/reference/Prediction.md)
    object of each resampling iterations, each yielding a single numeric
    score. Next, the scores are combined with the `aggregator` function
    to a single numerical score.

  - `"macro_weighted"`: The scoring function is applied on the
    [Prediction](https://mlr3.mlr-org.com/dev/reference/Prediction.md)
    object of each resampling iterations, each yielding a single numeric
    score. Next, the scores are combined with the `aggregator` function
    to a single numerical score. The scores are weighted by the total
    sample weights (if present, and if `$use_weights` is set to
    `"use"`), or the number of samples in each resampling iteration.

  - `"custom"`: The measure comes with a custom aggregation method which
    directly operates on a
    [ResampleResult](https://mlr3.mlr-org.com/dev/reference/ResampleResult.md).

- `aggregator`:

  (`function()`)  
  Function to aggregate scores computed on different resampling
  iterations.

- `use_weights`:

  (`character(1)`)  
  How to handle weights. Settings are `"use"`, `"ignore"`, and
  `"error"`.

  - `"use"`: Weights are used automatically if found in the task, as
    supported by the measure.

  - `"ignore"`: Weights are ignored.

  - `"error"`: throw an error if weights are present in the training
    `Task`.

  For measures with the property `"weights"`, this is initialized as
  `"use"`. For measures with the property `"requires_no_prediction"`,
  this is initialized as `"ignore"`. For measures that have neither of
  the properties, this is initialized as `"error"`. The latter behavior
  is to avoid cases where a user erroneously assumes that a measure
  supports weights when it does not. For measures that do not support
  weights, `use_weights` needs to be set to `"ignore"` if tasks with
  weights should be handled (by dropping the weights).

- `id`:

  (`character(1)`)  
  Identifier of the object. Used in tables, plot and text output.

- `label`:

  (`character(1)`)  
  Label for this object. Can be used in tables, plot and text output
  instead of the ID.

- `task_type`:

  (`character(1)`)  
  Task type, e.g. `"classif"` or `"regr"`.

  For a complete list of possible task types (depending on the loaded
  packages), see
  [`mlr_reflections$task_types$type`](https://mlr3.mlr-org.com/dev/reference/mlr_reflections.md).

- `param_set`:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html))  
  Set of hyperparameters.

- `predict_type`:

  (`character(1)`)  
  Required predict type of the
  [Learner](https://mlr3.mlr-org.com/dev/reference/Learner.md).

- `check_prerequisites`:

  (`character(1)`)  
  How to proceed if one of the following prerequisites is not met:

  - wrong predict type (e.g., probabilities required, but only labels
    available).

  - wrong predict set (e.g., learner predicted on training set, but
    predictions of test set required).

  - task properties not satisfied (e.g., binary classification measure
    on multiclass task).

  Possible values are `"ignore"` (just return `NaN`) and `"warn"`
  (default, raise a warning before returning `NaN`).

- `task_properties`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Required properties of the
  [Task](https://mlr3.mlr-org.com/dev/reference/Task.md).

- `range`:

  (`numeric(2)`)  
  Lower and upper bound of possible performance scores.

- `minimize`:

  (`logical(1)`)  
  If `TRUE`, good predictions correspond to small values of performance
  scores.

- `packages`:

  (`character(1)`)  
  Set of required packages. These packages are loaded, but not attached.

- `man`:

  (`character(1)` \| `NULL`)  
  String in the format `[pkg]::[topic]` pointing to a manual page for
  this object. Defaults to `NA`, but can be set by child classes.

- `trafo`:

  ([`list()`](https://rdrr.io/r/base/list.html) \| `NULL`)  
  `NULL` or a list with two elements:

  - `fn`: the transformation function applied after aggregating
    observation-wise losses (e.g. `sqrt` for RMSE)

  - `deriv`: The derivative of the `fn`.

## Methods

### Public methods

- [`Measure$new()`](#method-Measure-new)

- [`Measure$format()`](#method-Measure-format)

- [`Measure$print()`](#method-Measure-print)

- [`Measure$help()`](#method-Measure-help)

- [`Measure$score()`](#method-Measure-score)

- [`Measure$aggregate()`](#method-Measure-aggregate)

- [`Measure$obs_loss()`](#method-Measure-obs_loss)

- [`Measure$clone()`](#method-Measure-clone)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

Note that this object is typically constructed via a derived classes,
e.g.
[MeasureClassif](https://mlr3.mlr-org.com/dev/reference/MeasureClassif.md)
or [MeasureRegr](https://mlr3.mlr-org.com/dev/reference/MeasureRegr.md).

#### Usage

    Measure$new(
      id,
      task_type = NA,
      param_set = ps(),
      range = c(-Inf, Inf),
      minimize = NA,
      average = "macro",
      aggregator = NULL,
      properties = character(),
      predict_type = "response",
      predict_sets = "test",
      task_properties = character(),
      packages = character(),
      label = NA_character_,
      man = NA_character_,
      trafo = NULL
    )

#### Arguments

- `id`:

  (`character(1)`)  
  Identifier for the new instance.

- `task_type`:

  (`character(1)`)  
  Type of task, e.g. `"regr"` or `"classif"`. Must be an element of
  [mlr_reflections\$task_types\$type](https://mlr3.mlr-org.com/dev/reference/mlr_reflections.md).

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
  [Prediction](https://mlr3.mlr-org.com/dev/reference/Prediction.md)s
  from a
  [ResampleResult](https://mlr3.mlr-org.com/dev/reference/ResampleResult.md).

  The default, `"macro"`, calculates the individual performances scores
  for each
  [Prediction](https://mlr3.mlr-org.com/dev/reference/Prediction.md) and
  then uses the function defined in `$aggregator` to average them to a
  single number.

  `"macro_weighted"` is similar to `"macro"`, but uses weighted
  averages. Weights are taken from the `weights_measure` column of the
  resampled [Task](https://mlr3.mlr-org.com/dev/reference/Task.md) if
  present. Note that `"macro_weighted"` can differ from `"macro"` even
  if no weights are present or if `$use_weights` is set to `"ignore"`,
  since then aggregation is done using *uniform sample weights*, which
  result in non-uniform weights for
  [Prediction](https://mlr3.mlr-org.com/dev/reference/Prediction.md)s if
  they contain different numbers of samples.

  If set to `"micro"`, the individual
  [Prediction](https://mlr3.mlr-org.com/dev/reference/Prediction.md)
  objects are first combined into a single new
  [Prediction](https://mlr3.mlr-org.com/dev/reference/Prediction.md)
  object which is then used to assess the performance. The function in
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
    [ResampleResult](https://mlr3.mlr-org.com/dev/reference/ResampleResult.md)
    is passed to the aggregate function.

- `properties`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Properties of the measure. Must be a subset of
  [mlr_reflections\$measure_properties](https://mlr3.mlr-org.com/dev/reference/mlr_reflections.md).
  Supported by `mlr3`:

  - `"requires_task"` (requires the complete
    [Task](https://mlr3.mlr-org.com/dev/reference/Task.md)),

  - `"requires_learner"` (requires the trained
    [Learner](https://mlr3.mlr-org.com/dev/reference/Learner.md)),

  - `"requires_model"` (requires the trained
    [Learner](https://mlr3.mlr-org.com/dev/reference/Learner.md),
    including the fitted model),

  - `"requires_train_set"` (requires the training indices from the
    [Resampling](https://mlr3.mlr-org.com/dev/reference/Resampling.md)),

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
  [Learner](https://mlr3.mlr-org.com/dev/reference/Learner.md). Possible
  values are stored in
  [mlr_reflections\$learner_predict_types](https://mlr3.mlr-org.com/dev/reference/mlr_reflections.md).

- `predict_sets`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Prediction sets to operate on, used in
  [`aggregate()`](https://rdrr.io/r/stats/aggregate.html) to extract the
  matching `predict_sets` from the
  [ResampleResult](https://mlr3.mlr-org.com/dev/reference/ResampleResult.md).
  Multiple predict sets are calculated by the respective
  [Learner](https://mlr3.mlr-org.com/dev/reference/Learner.md) during
  [`resample()`](https://mlr3.mlr-org.com/dev/reference/resample.md)/[`benchmark()`](https://mlr3.mlr-org.com/dev/reference/benchmark.md).
  Must be a non-empty subset of `{"train", "test", "internal_valid"}`.
  If multiple sets are provided, these are first combined to a single
  prediction object. Default is `"test"`.

- `task_properties`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Required task properties, see
  [Task](https://mlr3.mlr-org.com/dev/reference/Task.md).

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

- `trafo`:

  ([`list()`](https://rdrr.io/r/base/list.html) \| `NULL`)  
  An optional list with two elements, containing the transformation
  `"fn"` and its derivative `"deriv"`. The transformation function is
  the function that is applied after aggregating the pointwise losses,
  i.e. this requires an `$obs_loss` to be present. An example is `sqrt`
  for RMSE.

------------------------------------------------------------------------

### Method [`format()`](https://rdrr.io/r/base/format.html)

Helper for print outputs.

#### Usage

    Measure$format(...)

#### Arguments

- `...`:

  (ignored).

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Printer.

#### Usage

    Measure$print(...)

#### Arguments

- `...`:

  (ignored).

------------------------------------------------------------------------

### Method [`help()`](https://rdrr.io/r/utils/help.html)

Opens the corresponding help page referenced by field `$man`.

#### Usage

    Measure$help()

------------------------------------------------------------------------

### Method `score()`

Takes a
[Prediction](https://mlr3.mlr-org.com/dev/reference/Prediction.md) (or a
list of
[Prediction](https://mlr3.mlr-org.com/dev/reference/Prediction.md)
objects named with valid `predict_sets`) and calculates a numeric score.
If the measure if flagged with the properties `"requires_task"`,
`"requires_learner"`, `"requires_model"` or `"requires_train_set"`, you
must additionally pass the respective
[Task](https://mlr3.mlr-org.com/dev/reference/Task.md), the (trained)
[Learner](https://mlr3.mlr-org.com/dev/reference/Learner.md) or the
training set indices. This is handled internally during
[`resample()`](https://mlr3.mlr-org.com/dev/reference/resample.md)/[`benchmark()`](https://mlr3.mlr-org.com/dev/reference/benchmark.md).

#### Usage

    Measure$score(prediction, task = NULL, learner = NULL, train_set = NULL)

#### Arguments

- `prediction`:

  ([Prediction](https://mlr3.mlr-org.com/dev/reference/Prediction.md) \|
  named list of
  [Prediction](https://mlr3.mlr-org.com/dev/reference/Prediction.md)).

- `task`:

  ([Task](https://mlr3.mlr-org.com/dev/reference/Task.md)).

- `learner`:

  ([Learner](https://mlr3.mlr-org.com/dev/reference/Learner.md)).

- `train_set`:

  ([`integer()`](https://rdrr.io/r/base/integer.html)).

#### Returns

`numeric(1)`.

#### Examples

    task = tsk("penguins")
    learner = lrn("classif.rpart")$train(task)
    prediction = learner$predict(task)
    msr("classif.ce")$score(prediction)

------------------------------------------------------------------------

### Method [`aggregate()`](https://rdrr.io/r/stats/aggregate.html)

Aggregates multiple performance scores into a single score, e.g. by
using the `aggregator` function of the measure.

#### Usage

    Measure$aggregate(rr)

#### Arguments

- `rr`:

  [ResampleResult](https://mlr3.mlr-org.com/dev/reference/ResampleResult.md).

#### Returns

`numeric(1)`.

#### Examples

    task = tsk("penguins")
    learner = lrn("classif.rpart")
    rr = resample(task, learner, rsmp("holdout"))
    msr("classif.ce")$aggregate(rr)

------------------------------------------------------------------------

### Method `obs_loss()`

Calculates the observation-wise loss. Returns a
[`numeric()`](https://rdrr.io/r/base/numeric.html) with one element for
each row in the
[Prediction](https://mlr3.mlr-org.com/dev/reference/Prediction.md). If
there is no observation-wise loss function for the measure, `NA_real_`
values are returned.

#### Usage

    Measure$obs_loss(prediction, task = NULL, learner = NULL)

#### Arguments

- `prediction`:

  ([Prediction](https://mlr3.mlr-org.com/dev/reference/Prediction.md)).

- `task`:

  ([Task](https://mlr3.mlr-org.com/dev/reference/Task.md)).

- `learner`:

  ([Learner](https://mlr3.mlr-org.com/dev/reference/Learner.md)).

#### Returns

[`numeric()`](https://rdrr.io/r/base/numeric.html) with one element for
each row in the
[Prediction](https://mlr3.mlr-org.com/dev/reference/Prediction.md).

#### Examples

    task = tsk("penguins")
    learner = lrn("classif.rpart")
    learner$train(task)
    prediction = learner$predict(task)
    msr("classif.ce")$obs_loss(prediction)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Measure$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
## ------------------------------------------------
## Method `Measure$score`
## ------------------------------------------------

task = tsk("penguins")
learner = lrn("classif.rpart")$train(task)
prediction = learner$predict(task)
msr("classif.ce")$score(prediction)
#> [1] 0.03488372

## ------------------------------------------------
## Method `Measure$aggregate`
## ------------------------------------------------

task = tsk("penguins")
learner = lrn("classif.rpart")
rr = resample(task, learner, rsmp("holdout"))
msr("classif.ce")$aggregate(rr)
#> classif.ce 
#> 0.03478261 

## ------------------------------------------------
## Method `Measure$obs_loss`
## ------------------------------------------------

task = tsk("penguins")
learner = lrn("classif.rpart")
learner$train(task)
prediction = learner$predict(task)
msr("classif.ce")$obs_loss(prediction)
#>   [1] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#>  [38] 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1
#>  [75] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#> [112] 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#> [149] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#> [186] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#> [223] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0
#> [260] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#> [297] 1 0 1 0 0 0 0 0 0 0 1 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0
#> [334] 0 0 0 0 0 0 0 0 0 0 0
```
