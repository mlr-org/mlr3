# Learner Class

This is the abstract base class for learner objects like
[LearnerClassif](https://mlr3.mlr-org.com/dev/reference/LearnerClassif.md)
and
[LearnerRegr](https://mlr3.mlr-org.com/dev/reference/LearnerRegr.md).

Learners are built around the three following key parts:

- Methods `$train()` and `$predict()` which call internal methods or
  private methods `$.train()`/`$.predict()`).

- A
  [paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html)
  which stores meta-information about available hyperparameters, and
  also stores hyperparameter settings.

- Meta-information about the requirements and capabilities of the
  learner.

- The fitted model stored in field `$model`, available after calling
  `$train()`.

Predefined learners are stored in the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr_learners](https://mlr3.mlr-org.com/dev/reference/mlr_learners.md),
e.g.
[`classif.rpart`](https://mlr3.mlr-org.com/dev/reference/mlr_learners_classif.rpart.md)
or
[`regr.rpart`](https://mlr3.mlr-org.com/dev/reference/mlr_learners_regr.rpart.md).

More classification and regression learners are implemented in the
add-on package
[mlr3learners](https://CRAN.R-project.org/package=mlr3learners).
Learners for survival analysis (or more general, for probabilistic
regression) can be found in
[mlr3proba](https://CRAN.R-project.org/package=mlr3proba). Unsupervised
cluster algorithms are implemented in
[mlr3cluster](https://CRAN.R-project.org/package=mlr3cluster). The
dictionary
[mlr_learners](https://mlr3.mlr-org.com/dev/reference/mlr_learners.md)
gets automatically populated with the new learners as soon as the
respective packages are loaded.

More (experimental) learners can be found in the GitHub repository:
<https://github.com/mlr-org/mlr3extralearners>. A guide on how to extend
[mlr3](https://CRAN.R-project.org/package=mlr3) with custom learners can
be found in the [mlr3book](https://mlr3book.mlr-org.com).

To combine the learner with preprocessing operations like factor
encoding,
[mlr3pipelines](https://CRAN.R-project.org/package=mlr3pipelines) is
recommended. Hyperparameters stored in the `param_set` can be tuned with
[mlr3tuning](https://CRAN.R-project.org/package=mlr3tuning).

## Optional Extractors

Specific learner implementations are free to implement additional
getters to ease the access of certain parts of the model in the
inherited subclasses.

For the following operations, extractors are standardized:

- `importance(...)`: Returns the feature importance score as numeric
  vector. The higher the score, the more important the variable. The
  returned vector is named with feature names and sorted in decreasing
  order. Note that the model might omit features it has not used at all.
  The learner must be tagged with property `"importance"`. To filter
  variables using the importance scores, see package
  [mlr3filters](https://CRAN.R-project.org/package=mlr3filters).

- `selected_features(...)`: Returns a subset of selected features as
  [`character()`](https://rdrr.io/r/base/character.html). The learner
  must be tagged with property `"selected_features"`.

- `oob_error(...)`: Returns the out-of-bag error of the model as
  `numeric(1)`. The learner must be tagged with property `"oob_error"`.

- `internal_valid_scores`: Returns the internal validation score(s) of
  the model as a named [`list()`](https://rdrr.io/r/base/list.html).
  Only available for `Learner`s with the `"validation"` property. If the
  learner is not trained yet, this returns `NULL`.

- `internal_tuned_values`: Returns the internally tuned hyperparameters
  of the model as a named [`list()`](https://rdrr.io/r/base/list.html).
  Only available for `Learner`s with the `"internal_tuning"` property.
  If the learner is not trained yet, this returns `NULL`.

## Weights

Many learners support observation weights, indicated by their property
`"weights"`. The weights are stored in the
[Task](https://mlr3.mlr-org.com/dev/reference/Task.md) where the column
role `weights_learner` needs to be assigned to a single numeric column.
If a task has weights and the learner supports them, they are used
automatically. If a task has weights but the learner does not support
them, an error is thrown by default. Both of these behaviors can be
disabled by setting the `use_weights` field to `"ignore"`. See the
description of `use_weights` for more information.

If the learner is set-up to use weights but the task does not have a
designated weight column, samples are considered to have equal weight.
When weights are being used, they are passed down to the learner
directly; the effect of weights depends on the specific learner.
Generally, weights do not need to sum up to 1.

When implementing a Learner that uses weights, the `"weights"` property
should be set. The `$.train()`-method should then call the
`$.get_weights()`-method to retrieve the weights from the task.
`$.get_weights()` will automatically discard weights when `use_weights`
is set to `"ignore"`;

## Setting Hyperparameters

All information about hyperparameters is stored in the slot `param_set`
which is a
[paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html).
The printer gives an overview about the ids of available
hyperparameters, their storage type, lower and upper bounds, possible
levels (for factors), default values and assigned values. To set
hyperparameters, call the `set_values()` method on the `param_set`:

    lrn = lrn("classif.rpart")
    lrn$param_set$set_values(minsplit = 3, cp = 0.01)

Note that this operation replaces all previously set hyperparameter
values. If you only intend to change one specific hyperparameter value
and leave the others as-is, you can use the helper function
[`mlr3misc::insert_named()`](https://mlr3misc.mlr-org.com/reference/insert_named.html):

    lrn$param_set$values = mlr3misc::insert_named(lrn$param_set$values, list(cp = 0.001))

If the learner has additional hyperparameters which are not encoded in
the [ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html), you
can easily extend the learner. Here, we add a factor hyperparameter with
id `"foo"` and possible levels `"a"` and `"b"`:

    lrn$param_set$add(paradox::ParamFct$new("foo", levels = c("a", "b")))

## Implementing Validation

Some Learners, such as `XGBoost`, other boosting algorithms, or deep
learning models (`mlr3torch`), utilize validation data during the
training to prevent overfitting or to log the validation performance. It
is possible to configure learners to be able to receive such an
independent validation set during training. To do so, one must:

- annotate the learner with the `"validation"` property

- implement the active binding `$internal_valid_scores` (see section
  *Optional Extractors*), as well as the private method
  `$.extract_internal_valid_scores()` which returns the (final) internal
  validation scores from the model of the `Learner` and returns them as
  a named [`list()`](https://rdrr.io/r/base/list.html) of `numeric(1)`.
  If the model is not trained yet, this method should return `NULL`.

- Add the `validate` parameter, which can be either `NULL`, a ratio in
  \$(0, 1)\$, `"test"`, or `"predefined"`:

  - `NULL`: no validation

  - `ratio`: only proportion `1 - ratio` of the task is used for
    training and `ratio` is used for validation.

  - `"test"` means that the `"test"` task is used. **Warning**: This can
    lead to biased performance estimation. This option is only available
    if the learner is being trained via
    [`resample()`](https://mlr3.mlr-org.com/dev/reference/resample.md),
    [`benchmark()`](https://mlr3.mlr-org.com/dev/reference/benchmark.md)
    or functions that internally use them, e.g. `tune()` of
    [mlr3tuning](https://CRAN.R-project.org/package=mlr3tuning) or
    `batchmark()` of
    [mlr3batchmark](https://CRAN.R-project.org/package=mlr3batchmark).
    This is especially useful for hyperparameter tuning, where one might
    e.g. want to use the same validation data for early stopping and
    model evaluation.

  - `"predefined"` means that the task's (manually set)
    `$internal_valid_task` is used. See the
    [`Task`](https://mlr3.mlr-org.com/dev/reference/Task.md)
    documentation for more information.

For an example how to do this, see
[`LearnerClassifDebug`](https://mlr3.mlr-org.com/dev/reference/mlr_learners_classif.debug.md).
Note that in `.train()`, the `$internal_valid_task` will only be present
if the `$validate` field of the `Learner` is set to a non-`NULL` value.

## Implementing Internal Tuning

Some learners such as `XGBoost` or `cv.glmnet` can internally tune
hyperparameters. XGBoost, for example, can tune the number of boosting
rounds based on the validation performance. CV Glmnet, on the other
hand, can tune the regularization parameter based on an internal
cross-validation. Internal tuning *can* therefore rely on the internal
validation data, but does not necessarily do so.

In order to be able to combine this internal hyperparameter tuning with
the standard hyperparameter optimization implemented via
[mlr3tuning](https://CRAN.R-project.org/package=mlr3tuning), one must:

- annotate the learner with the `"internal_tuning"` property

- implement the active binding `$internal_tuned_values` (see section
  *Optional Extractors*) as well as the private method
  `$.extract_internal_tuned_values()` which extracts the internally
  tuned values from the `Learner`'s model and returns them as a named
  [`list()`](https://rdrr.io/r/base/list.html). If the model is not
  trained yet, this method should return `NULL`.

- Have at least one parameter tagged with `"internal_tuning"`, which
  requires to also provide a `in_tune_fn` and `disable_tune_fn`, and
  *should* also include a default `aggr`egation function.

For an example how to do this, see
[`LearnerClassifDebug`](https://mlr3.mlr-org.com/dev/reference/mlr_learners_classif.debug.md).

## Implementing Marshaling

Some `Learner`s have models that cannot be serialized as they e.g.
contain external pointers. In order to still be able to save them, use
them with parallelization or callr encapsulation it is necessary to
implement how they should be (un)-marshaled. See
[`marshaling`](https://mlr3.mlr-org.com/dev/reference/marshaling.md) for
how to do this.

## Implementing Out-of-Bag Error

Some `Learner`s can compute the out-of-bag error during training. In
order to do this, the learner must:

- annotate the learner with the `"oob_error"` property

- implement the private method `$.extract_oob_error()` which extracts
  the out-of-bag error from the `Learner`'s model and returns it as a
  `numeric(1)`.

## See also

- Chapter in the [mlr3book](https://mlr3book.mlr-org.com/):
  <https://mlr3book.mlr-org.com/chapters/chapter2/data_and_basic_modeling.html#sec-learners>

- Package
  [mlr3learners](https://CRAN.R-project.org/package=mlr3learners) for a
  solid collection of essential learners.

- Package
  [mlr3extralearners](https://github.com/mlr-org/mlr3extralearners) for
  more learners.

- [Dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
  of Learners:
  [mlr_learners](https://mlr3.mlr-org.com/dev/reference/mlr_learners.md)

- `as.data.table(mlr_learners)` for a table of available Learners in the
  running session (depending on the loaded packages).

- [mlr3pipelines](https://CRAN.R-project.org/package=mlr3pipelines) to
  combine learners with pre- and postprocessing steps.

- Package [mlr3viz](https://CRAN.R-project.org/package=mlr3viz) for some
  generic visualizations.

- Extension packages for additional task types:

  - [mlr3proba](https://CRAN.R-project.org/package=mlr3proba) for
    probabilistic supervised regression and survival analysis.

  - [mlr3cluster](https://CRAN.R-project.org/package=mlr3cluster) for
    unsupervised clustering.

- [mlr3tuning](https://CRAN.R-project.org/package=mlr3tuning) for tuning
  of hyperparameters,
  [mlr3tuningspaces](https://CRAN.R-project.org/package=mlr3tuningspaces)
  for established default tuning spaces.

Other Learner:
[`LearnerClassif`](https://mlr3.mlr-org.com/dev/reference/LearnerClassif.md),
[`LearnerRegr`](https://mlr3.mlr-org.com/dev/reference/LearnerRegr.md),
[`mlr_learners`](https://mlr3.mlr-org.com/dev/reference/mlr_learners.md),
[`mlr_learners_classif.debug`](https://mlr3.mlr-org.com/dev/reference/mlr_learners_classif.debug.md),
[`mlr_learners_classif.featureless`](https://mlr3.mlr-org.com/dev/reference/mlr_learners_classif.featureless.md),
[`mlr_learners_classif.rpart`](https://mlr3.mlr-org.com/dev/reference/mlr_learners_classif.rpart.md),
[`mlr_learners_regr.debug`](https://mlr3.mlr-org.com/dev/reference/mlr_learners_regr.debug.md),
[`mlr_learners_regr.featureless`](https://mlr3.mlr-org.com/dev/reference/mlr_learners_regr.featureless.md),
[`mlr_learners_regr.rpart`](https://mlr3.mlr-org.com/dev/reference/mlr_learners_regr.rpart.md)

## Public fields

- `state`:

  (named [`list()`](https://rdrr.io/r/base/list.html) \| `NULL`)  
  Current (internal) state of the learner. Contains all information
  gathered during `train()` and
  [`predict()`](https://rdrr.io/r/stats/predict.html). It is not
  recommended to access elements from `state` directly. This is an
  internal data structure which may change in the future.

## Active bindings

- `use_weights`:

  (`character(1)`)  
  How weights should be handled. Settings are `"use"` `"ignore"`, and
  `"error"`.

  - `"use"`: use weights, as supported by the underlying `Learner`. Only
    available for `Learner`s with the property `"weights"`.

  - `"ignore"`: do not use weights.

  - `"error"`: throw an error if weights are present in the training
    `Task`.

  For `Learner`s with the property `"weights"`, this is initialized as
  `"use"`. For `Learner`s that do not support weights, i.e. without the
  `"weights"` property, this is initialized as `"error"`. The latter
  behavior is to avoid cases where a user erroneously assumes that a
  `Learner` supports weights when it does not. For `Learner`s that do
  not support weights, `use_weights` needs to be set to `"ignore"` if
  tasks with weights should be handled (by dropping the weights). See
  Section 'weights' for more details.

- `model`:

  (any)  
  The fitted model. Only available after `$train()` has been called.

- `native_model`:

  (any)  
  The native model object from the upstream package. For most learners,
  this is identical to `$model`. However, some learners store additional
  information beyond the model from the upstream package. In such cases,
  `$model` contains a named list with the native model stored in element
  `model` along with additional information. The `$native_model` field
  can be overwritten by the learner to return the actual model object
  from the upstream package. The default returns `$model`.

- `timings`:

  (named `numeric(2)`)  
  Elapsed time in seconds for the steps `"train"` and `"predict"`.

  When predictions for multiple predict sets were made during
  [`resample()`](https://mlr3.mlr-org.com/dev/reference/resample.md) or
  [`benchmark()`](https://mlr3.mlr-org.com/dev/reference/benchmark.md),
  the predict time shows the cumulative duration of all predictions. If
  `learner$predict()` is called manually, the last predict time gets
  overwritten.

  Measured via
  [`mlr3misc::encapsulate()`](https://mlr3misc.mlr-org.com/reference/encapsulate.html).

- `log`:

  ([`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html))  
  Returns the output (including warning and errors) as table with
  columns

  - `"stage"` ("train" or "predict"),

  - `"class"` ("output", "warning", or "error"), and

  - `"condition"` (`condition`).

- `warnings`:

  ([`list()`](https://rdrr.io/r/base/list.html))  
  Logged warnings condition objects.

- `errors`:

  ([`list()`](https://rdrr.io/r/base/list.html))  
  Logged error condition objects.

- `hash`:

  (`character(1)`)  
  Hash (unique identifier) for this object. The hash is calculated based
  on the learner id, the parameter settings, the predict type, the
  fallback hash, the parallel predict setting, the validate setting, and
  the predict sets.

- `phash`:

  (`character(1)`)  
  Hash (unique identifier) for this partial object, excluding some
  components which are varied systematically during tuning (parameter
  values).

- `predict_type`:

  (`character(1)`)  
  Stores the currently active predict type, e.g. `"response"`. Must be
  an element of `$predict_types`. A few learners already use the predict
  type during training. So there is no guarantee that changing the
  predict type after training will have any effect or does not lead to
  errors.

- `param_set`:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html))  
  Set of hyperparameters.

- `fallback`:

  (Learner)  
  Returns the fallback learner set with `$encapsulate()`.

- `encapsulation`:

  (`character(2)`)  
  Returns the encapsulation settings set with `$encapsulate()`.

- `hotstart_stack`:

  ([HotstartStack](https://mlr3.mlr-org.com/dev/reference/HotstartStack.md))  
  . Stores `HotstartStack`.

- `selected_features_impute`:

  (`character(1)`)  
  Controls the behavior if the learner does not support feature
  selection. If set to `"error"`, an error is thrown. If set to `"all"`
  the complete feature set is returned.

- `predict_types`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Stores the possible predict types the learner is capable of. A
  complete list of candidate predict types, grouped by task type, is
  stored in
  [`mlr_reflections$learner_predict_types`](https://mlr3.mlr-org.com/dev/reference/mlr_reflections.md).
  This field is read-only.

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

- `feature_types`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Stores the feature types the learner can handle, e.g. `"logical"`,
  `"numeric"`, or `"factor"`. A complete list of candidate feature
  types, grouped by task type, is stored in
  [`mlr_reflections$task_feature_types`](https://mlr3.mlr-org.com/dev/reference/mlr_reflections.md).

- `properties`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Stores a set of properties/capabilities the learner has. A complete
  list of candidate properties, grouped by task type, is stored in
  [`mlr_reflections$learner_properties`](https://mlr3.mlr-org.com/dev/reference/mlr_reflections.md).

- `packages`:

  (`character(1)`)  
  Set of required packages. These packages are loaded, but not attached.

- `predict_sets`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  During
  [`resample()`](https://mlr3.mlr-org.com/dev/reference/resample.md)/[`benchmark()`](https://mlr3.mlr-org.com/dev/reference/benchmark.md),
  a Learner can predict on multiple sets. Per default, a learner only
  predicts observations in the test set (`predict_sets == "test"`). To
  change this behavior, set `predict_sets` to a non-empty subset of
  `{"train", "test", "internal_valid"}`. The `"train"` predict set
  contains the train ids from the resampling. This means that if a
  learner does validation and sets `$validate` to a ratio (creating the
  validation data from the training data), the train predictions will
  include the predictions for the validation data. Each set yields a
  separate
  [Prediction](https://mlr3.mlr-org.com/dev/reference/Prediction.md)
  object. Those can be combined via getters in
  [ResampleResult](https://mlr3.mlr-org.com/dev/reference/ResampleResult.md)/[BenchmarkResult](https://mlr3.mlr-org.com/dev/reference/BenchmarkResult.md),
  or [Measure](https://mlr3.mlr-org.com/dev/reference/Measure.md)s can
  be configured to operate on specific subsets of the calculated
  prediction sets.

- `parallel_predict`:

  (`logical(1)`)  
  If set to `TRUE`, use
  [future](https://CRAN.R-project.org/package=future) to calculate
  predictions in parallel (default: `FALSE`). The row ids of the `task`
  will be split into
  [`future::nbrOfWorkers()`](https://future.futureverse.org/reference/nbrOfWorkers.html)
  chunks, and predictions are evaluated according to the active
  [`future::plan()`](https://future.futureverse.org/reference/plan.html).
  This currently only works for methods `Learner$predict()` and
  `Learner$predict_newdata()`, and has no effect during
  [`resample()`](https://mlr3.mlr-org.com/dev/reference/resample.md) or
  [`benchmark()`](https://mlr3.mlr-org.com/dev/reference/benchmark.md)
  where you have other means to parallelize.

  Note that the recorded time required for prediction reports the time
  required to predict is not properly defined and depends on the
  parallelization backend.

- `timeout`:

  (named `numeric(2)`)  
  Timeout for the learner's train and predict steps, in seconds. This
  works differently for different encapsulation methods, see
  [`mlr3misc::encapsulate()`](https://mlr3misc.mlr-org.com/reference/encapsulate.html).
  Default is `c(train = Inf, predict = Inf)`. Also see the section on
  error handling in the mlr3book:
  <https://mlr3book.mlr-org.com/chapters/chapter10/advanced_technical_aspects_of_mlr3.html#sec-error-handling>

- `man`:

  (`character(1)` \| `NULL`)  
  String in the format `[pkg]::[topic]` pointing to a manual page for
  this object. Defaults to `NA`, but can be set by child classes.

## Methods

### Public methods

- [`Learner$new()`](#method-Learner-new)

- [`Learner$format()`](#method-Learner-format)

- [`Learner$print()`](#method-Learner-print)

- [`Learner$help()`](#method-Learner-help)

- [`Learner$train()`](#method-Learner-train)

- [`Learner$predict()`](#method-Learner-predict)

- [`Learner$predict_newdata()`](#method-Learner-predict_newdata)

- [`Learner$reset()`](#method-Learner-reset)

- [`Learner$base_learner()`](#method-Learner-base_learner)

- [`Learner$encapsulate()`](#method-Learner-encapsulate)

- [`Learner$configure()`](#method-Learner-configure)

- [`Learner$selected_features()`](#method-Learner-selected_features)

- [`Learner$clone()`](#method-Learner-clone)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

Note that this object is typically constructed via a derived classes,
e.g.
[LearnerClassif](https://mlr3.mlr-org.com/dev/reference/LearnerClassif.md)
or [LearnerRegr](https://mlr3.mlr-org.com/dev/reference/LearnerRegr.md).

#### Usage

    Learner$new(
      id,
      task_type,
      param_set = ps(),
      predict_types = character(),
      feature_types = character(),
      properties = character(),
      packages = character(),
      label = NA_character_,
      man = NA_character_
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

- `predict_types`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Supported predict types. Must be a subset of
  [`mlr_reflections$learner_predict_types`](https://mlr3.mlr-org.com/dev/reference/mlr_reflections.md).

- `feature_types`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Feature types the learner operates on. Must be a subset of
  [`mlr_reflections$task_feature_types`](https://mlr3.mlr-org.com/dev/reference/mlr_reflections.md).

- `properties`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Set of properties of the Learner. Must be a subset of
  [`mlr_reflections$learner_properties`](https://mlr3.mlr-org.com/dev/reference/mlr_reflections.md).
  The following properties are currently standardized and understood by
  learners in [mlr3](https://CRAN.R-project.org/package=mlr3):

  - `"missings"`: The learner can handle missing values in the data.

  - `"weights"`: The learner supports observation weights.

  - `"offset"`: The learner can incorporate offset values to adjust
    predictions.

  - `"importance"`: The learner supports extraction of importance
    scores, i.e. comes with an `$importance()` extractor function (see
    section on optional extractors in Learner).

  - `"selected_features"`: The learner supports extraction of the set of
    selected features, i.e. comes with a `$selected_features()`
    extractor function (see section on optional extractors in Learner).

  - `"oob_error"`: The learner supports extraction of estimated out of
    bag error, i.e. comes with a `oob_error()` extractor function (see
    section on optional extractors in Learner).

  - `"validation"`: The learner can use a validation task during
    training.

  - `"internal_tuning"`: The learner is able to internally optimize
    hyperparameters (those are also tagged with `"internal_tuning"`).

  - `"marshal"`: To save learners with this property, you need to call
    `$marshal()` first. If a learner is in a marshaled state, you call
    first need to call `$unmarshal()` to use its model, e.g. for
    prediction.

  - `"hotstart_forward"`: The learner supports to hotstart a model
    forward.

  - `"hotstart_backward"`: The learner supports hotstarting a model
    backward.

  - \`"featureless": The learner does not use features.

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

### Method [`format()`](https://rdrr.io/r/base/format.html)

Helper for print outputs.

#### Usage

    Learner$format(...)

#### Arguments

- `...`:

  (ignored).

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Printer.

#### Usage

    Learner$print(...)

#### Arguments

- `...`:

  (ignored).

------------------------------------------------------------------------

### Method [`help()`](https://rdrr.io/r/utils/help.html)

Opens the corresponding help page referenced by field `$man`.

#### Usage

    Learner$help()

------------------------------------------------------------------------

### Method `train()`

Train the learner on a set of observations of the provided `task`.
Mutates the learner by reference, i.e. stores the model alongside other
information in field `$state`.

#### Usage

    Learner$train(task, row_ids = NULL)

#### Arguments

- `task`:

  ([Task](https://mlr3.mlr-org.com/dev/reference/Task.md)).

- `row_ids`:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  Vector of training indices as subset of `task$row_ids`. For a simple
  split into training and test set, see
  [`partition()`](https://mlr3.mlr-org.com/dev/reference/partition.md).

#### Returns

Returns the object itself, but modified **by reference**. You need to
explicitly `$clone()` the object beforehand if you want to keep the
object in its previous state.

#### Examples

    task   = tsk("penguins")
    learner = lrn("classif.rpart")
    learner$train(task)

------------------------------------------------------------------------

### Method [`predict()`](https://rdrr.io/r/stats/predict.html)

Uses the fitted model stored in `$state` to generate predictions for a
set of observations from the provided `task`. This method requires that
the learner has been previously trained using `$train()`.

#### Usage

    Learner$predict(task, row_ids = NULL)

#### Arguments

- `task`:

  ([Task](https://mlr3.mlr-org.com/dev/reference/Task.md))  
  The task containing the observations to predict on. Must be compatible
  with the learner's task type and feature types. Unlike
  `$predict_newdata()`, no type conversion is done.

- `row_ids`:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  Vector of row indices from `task$row_ids` to predict on. If `NULL`
  (default), predictions are made for all rows in the task. For a simple
  train-test split, see
  [`partition()`](https://mlr3.mlr-org.com/dev/reference/partition.md).

#### Returns

[Prediction](https://mlr3.mlr-org.com/dev/reference/Prediction.md)
object containing the predictions for the specified observations.

#### Examples

    task = tsk("penguins")
    learner = lrn("classif.rpart")$train(task)
    learner$predict(task)

------------------------------------------------------------------------

### Method `predict_newdata()`

Uses the model fitted during `$train()` to create a new
[Prediction](https://mlr3.mlr-org.com/dev/reference/Prediction.md) based
on the new data in `newdata`. Object `task` is the task used during
`$train()` and required for conversion of `newdata`. If the learner's
`$train()` method has been called, there is a (size reduced) version of
the training task stored in the learner. If the learner has been fitted
via [`resample()`](https://mlr3.mlr-org.com/dev/reference/resample.md)
or [`benchmark()`](https://mlr3.mlr-org.com/dev/reference/benchmark.md),
you need to pass the corresponding task stored in the
[ResampleResult](https://mlr3.mlr-org.com/dev/reference/ResampleResult.md)
or
[BenchmarkResult](https://mlr3.mlr-org.com/dev/reference/BenchmarkResult.md),
respectively. Further,
[`auto_convert`](https://mlr3.mlr-org.com/dev/reference/auto_convert.md)
is used for type-conversions to ensure compatibility of features between
`$train()` and `$predict()`.

If the stored training task has a `weights_measure` column, *and* if
`newdata` contains a column with the same name, that column must be
numeric with no missing values and is used as measure weights column.
Otherwise, no measure weights are used.

#### Usage

    Learner$predict_newdata(newdata, task = NULL)

#### Arguments

- `newdata`:

  (any object supported by
  [`as_data_backend()`](https://mlr3.mlr-org.com/dev/reference/as_data_backend.md))  
  New data to predict on. All data formats convertible by
  [`as_data_backend()`](https://mlr3.mlr-org.com/dev/reference/as_data_backend.md)
  are supported, e.g.
  [`data.frame()`](https://rdrr.io/r/base/data.frame.html) or
  [DataBackend](https://mlr3.mlr-org.com/dev/reference/DataBackend.md).
  If a
  [DataBackend](https://mlr3.mlr-org.com/dev/reference/DataBackend.md)
  is provided as `newdata`, the row ids are preserved, otherwise they
  are set to the sequence `1:nrow(newdata)`.

- `task`:

  ([Task](https://mlr3.mlr-org.com/dev/reference/Task.md)).

#### Returns

[Prediction](https://mlr3.mlr-org.com/dev/reference/Prediction.md).

#### Examples

    task = tsk("penguins")
    learner = lrn("classif.rpart")$train(task)
    learner$predict_newdata(task$data(rows = 1:5))

------------------------------------------------------------------------

### Method `reset()`

Reset the learner, i.e. un-train by resetting the `state`.

#### Usage

    Learner$reset()

#### Returns

Returns the object itself, but modified **by reference**. You need to
explicitly `$clone()` the object beforehand if you want to keep the
object in its previous state.

#### Examples

    task = tsk("penguins")
    learner = lrn("classif.rpart")$train(task)
    learner$reset()

------------------------------------------------------------------------

### Method `base_learner()`

Extracts the base learner from nested learner objects like
`GraphLearner` in
[mlr3pipelines](https://CRAN.R-project.org/package=mlr3pipelines) or
`AutoTuner` in
[mlr3tuning](https://CRAN.R-project.org/package=mlr3tuning). Returns the
Learner itself for regular learners.

#### Usage

    Learner$base_learner(recursive = Inf)

#### Arguments

- `recursive`:

  (`integer(1)`)  
  Depth of recursion for multiple nested objects.

#### Returns

Learner

------------------------------------------------------------------------

### Method `encapsulate()`

Sets the encapsulation method and fallback learner for the train and
predict steps. There are currently four different methods implemented:

- `"none"`: Just runs the learner in the current session and measures
  the elapsed time. Does not keep a log, output is printed directly to
  the console. Works well together with
  [`traceback()`](https://rdrr.io/r/base/traceback.html).

- `"try"`: Similar to `"none"`, but catches error. Output is printed to
  the console and not logged.

- `"evaluate"`: Uses the package
  [evaluate](https://CRAN.R-project.org/package=evaluate) to call the
  learner, measure time and do the logging.

- `"callr"`: Uses the package
  [callr](https://CRAN.R-project.org/package=callr) to call the learner,
  measure time and do the logging. This encapsulation spawns a separate
  R session in which the learner is called. While this comes with a
  considerable overhead, it also guards your session from being torn
  down by segfaults.

- `"mirai"`: Uses the package
  [mirai](https://CRAN.R-project.org/package=mirai) to call the learner,
  measure time and do the logging. This encapsulation calls the function
  in a `mirai` on a `daemon`. The `daemon` can be pre-started via
  `daemons(1, .compute = "mlr3_encapsulation")`, otherwise a new R
  session will be created for each encapsulated call. If a `daemon` is
  already running with compute profile `"mlr3_encapsulation"`, it will
  be used to execute all calls. Using `mirai"` is similarly safe as
  `callr` but much faster if several learners are encapsulated one after
  the other on the same daemon.

The fallback learner is fitted to create valid predictions in case that
either the model fitting or the prediction of the original learner
fails. If the training step or the predict step of the original learner
fails, the fallback is used to make the predictions. If the original
learner only partially fails during predict step (usually in the form of
missing to predict some observations or producing some `NA`
predictions), these missing predictions are imputed by the fallback.
Note that the fallback is always trained, as we do not know in advance
whether prediction will fail. If the training step fails, the `$model`
field of the original learner is `NULL`. The results are reproducible
across the different encapsulation methods.

Note that for errors of class `Mlr3ErrorConfig`, the function always
errs and no fallback learner is trained.

Also see the section on error handling in the mlr3book:
<https://mlr3book.mlr-org.com/chapters/chapter10/advanced_technical_aspects_of_mlr3.html#sec-error-handling>

#### Usage

    Learner$encapsulate(method, fallback = NULL, when = NULL)

#### Arguments

- `method`:

  `character(1)`  
  One of `"none"`, `"try"`, `"evaluate"` or `"callr"`. See the
  description for details.

- `fallback`:

  Learner  
  The fallback learner for failed predictions.

- `when`:

  (`function(cond, stage)`)  
  Function that takes in the condition (`cond`) and the stage (`"train"`
  or `"predict"`) and returns `logical(1)` indicating whether to run the
  fallback learner.

  If `NULL` (default), the fallback is always used, except for errors of
  class `Mlr3ErrorConfig`.

#### Returns

`self` (invisibly).

#### Examples

    learner = lrn("classif.rpart")
    fallback = lrn("classif.featureless")
    learner$encapsulate("try", fallback = fallback)

------------------------------------------------------------------------

### Method `configure()`

Sets parameter values and fields of the learner. All arguments whose
names match the name of a parameter of the
[paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html)
are set as parameters. All remaining arguments are assumed to be regular
fields.

#### Usage

    Learner$configure(..., .values = list())

#### Arguments

- `...`:

  (named `any`)  
  Named arguments to set parameter values and fields.

- `.values`:

  (named `any`)  
  Named list of parameter values and fields.

#### Examples

    learner = lrn("classif.rpart")
    learner$configure(minsplit = 3, parallel_predict = FALSE)
    learner$configure(.values = list(cp = 0.005))

------------------------------------------------------------------------

### Method `selected_features()`

Returns the features selected by the model. The field
`selected_features_impute` controls the behavior if the learner does not
support feature selection. If set to `"error"`, an error is thrown,
otherwise all features are returned.

#### Usage

    Learner$selected_features()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Learner$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
## ------------------------------------------------
## Method `Learner$train`
## ------------------------------------------------

task   = tsk("penguins")
learner = lrn("classif.rpart")
learner$train(task)

## ------------------------------------------------
## Method `Learner$predict`
## ------------------------------------------------

task = tsk("penguins")
learner = lrn("classif.rpart")$train(task)
learner$predict(task)
#> 
#> ── <PredictionClassif> for 344 observations: ───────────────────────────────────
#>  row_ids     truth  response
#>        1    Adelie    Adelie
#>        2    Adelie    Adelie
#>        3    Adelie    Adelie
#>      ---       ---       ---
#>      342 Chinstrap Chinstrap
#>      343 Chinstrap Chinstrap
#>      344 Chinstrap Chinstrap

## ------------------------------------------------
## Method `Learner$predict_newdata`
## ------------------------------------------------

task = tsk("penguins")
learner = lrn("classif.rpart")$train(task)
learner$predict_newdata(task$data(rows = 1:5))
#> 
#> ── <PredictionClassif> for 5 observations: ─────────────────────────────────────
#>  row_ids  truth response
#>        1 Adelie   Adelie
#>        2 Adelie   Adelie
#>        3 Adelie   Adelie
#>        4 Adelie   Adelie
#>        5 Adelie   Adelie

## ------------------------------------------------
## Method `Learner$reset`
## ------------------------------------------------

task = tsk("penguins")
learner = lrn("classif.rpart")$train(task)
learner$reset()

## ------------------------------------------------
## Method `Learner$encapsulate`
## ------------------------------------------------

learner = lrn("classif.rpart")
fallback = lrn("classif.featureless")
learner$encapsulate("try", fallback = fallback)

## ------------------------------------------------
## Method `Learner$configure`
## ------------------------------------------------

learner = lrn("classif.rpart")
learner$configure(minsplit = 3, parallel_predict = FALSE)
learner$configure(.values = list(cp = 0.005))
```
