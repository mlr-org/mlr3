# Classification Learner

This Learner specializes
[Learner](https://mlr3.mlr-org.com/reference/Learner.md) for
classification problems:

- `task_type` is set to `"classif"`.

- Creates
  [Prediction](https://mlr3.mlr-org.com/reference/Prediction.md)s of
  class
  [PredictionClassif](https://mlr3.mlr-org.com/reference/PredictionClassif.md).

- Possible values for `predict_types` are:

  - `"response"`: Predicts a class label for each observation in the
    test set.

  - `"prob"`: Predicts the posterior probability for each class for each
    observation in the test set.

- Additional learner properties include:

  - `"twoclass"`: The learner works on binary classification problems.

  - `"multiclass"`: The learner works on multiclass classification
    problems.

Predefined learners can be found in the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr_learners](https://mlr3.mlr-org.com/reference/mlr_learners.md).
Essential classification learners can be found in this dictionary after
loading [mlr3learners](https://CRAN.R-project.org/package=mlr3learners).
Additional learners are implement in the Github package
<https://github.com/mlr-org/mlr3extralearners>.

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
  of [Learners](https://mlr3.mlr-org.com/reference/Learner.md):
  [mlr_learners](https://mlr3.mlr-org.com/reference/mlr_learners.md)

- `as.data.table(mlr_learners)` for a table of available
  [Learners](https://mlr3.mlr-org.com/reference/Learner.md) in the
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
[`Learner`](https://mlr3.mlr-org.com/reference/Learner.md),
[`LearnerRegr`](https://mlr3.mlr-org.com/reference/LearnerRegr.md),
[`mlr_learners`](https://mlr3.mlr-org.com/reference/mlr_learners.md),
[`mlr_learners_classif.debug`](https://mlr3.mlr-org.com/reference/mlr_learners_classif.debug.md),
[`mlr_learners_classif.featureless`](https://mlr3.mlr-org.com/reference/mlr_learners_classif.featureless.md),
[`mlr_learners_classif.rpart`](https://mlr3.mlr-org.com/reference/mlr_learners_classif.rpart.md),
[`mlr_learners_regr.debug`](https://mlr3.mlr-org.com/reference/mlr_learners_regr.debug.md),
[`mlr_learners_regr.featureless`](https://mlr3.mlr-org.com/reference/mlr_learners_regr.featureless.md),
[`mlr_learners_regr.rpart`](https://mlr3.mlr-org.com/reference/mlr_learners_regr.rpart.md)

## Super class

[`mlr3::Learner`](https://mlr3.mlr-org.com/reference/Learner.md) -\>
`LearnerClassif`

## Methods

### Public methods

- [`LearnerClassif$new()`](#method-LearnerClassif-new)

- [`LearnerClassif$predict_newdata_fast()`](#method-LearnerClassif-predict_newdata_fast)

- [`LearnerClassif$clone()`](#method-LearnerClassif-clone)

Inherited methods

- [`mlr3::Learner$base_learner()`](https://mlr3.mlr-org.com/reference/Learner.html#method-base_learner)
- [`mlr3::Learner$configure()`](https://mlr3.mlr-org.com/reference/Learner.html#method-configure)
- [`mlr3::Learner$encapsulate()`](https://mlr3.mlr-org.com/reference/Learner.html#method-encapsulate)
- [`mlr3::Learner$format()`](https://mlr3.mlr-org.com/reference/Learner.html#method-format)
- [`mlr3::Learner$help()`](https://mlr3.mlr-org.com/reference/Learner.html#method-help)
- [`mlr3::Learner$predict()`](https://mlr3.mlr-org.com/reference/Learner.html#method-predict)
- [`mlr3::Learner$predict_newdata()`](https://mlr3.mlr-org.com/reference/Learner.html#method-predict_newdata)
- [`mlr3::Learner$print()`](https://mlr3.mlr-org.com/reference/Learner.html#method-print)
- [`mlr3::Learner$reset()`](https://mlr3.mlr-org.com/reference/Learner.html#method-reset)
- [`mlr3::Learner$selected_features()`](https://mlr3.mlr-org.com/reference/Learner.html#method-selected_features)
- [`mlr3::Learner$train()`](https://mlr3.mlr-org.com/reference/Learner.html#method-train)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    LearnerClassif$new(
      id,
      param_set = ps(),
      predict_types = "response",
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

- `param_set`:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html))  
  Set of hyperparameters.

- `predict_types`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Supported predict types. Must be a subset of
  [`mlr_reflections$learner_predict_types`](https://mlr3.mlr-org.com/reference/mlr_reflections.md).

- `feature_types`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Feature types the learner operates on. Must be a subset of
  [`mlr_reflections$task_feature_types`](https://mlr3.mlr-org.com/reference/mlr_reflections.md).

- `properties`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Set of properties of the
  [Learner](https://mlr3.mlr-org.com/reference/Learner.md). Must be a
  subset of
  [`mlr_reflections$learner_properties`](https://mlr3.mlr-org.com/reference/mlr_reflections.md).
  The following properties are currently standardized and understood by
  learners in [mlr3](https://CRAN.R-project.org/package=mlr3):

  - `"missings"`: The learner can handle missing values in the data.

  - `"weights"`: The learner supports observation weights.

  - `"offset"`: The learner can incorporate offset values to adjust
    predictions.

  - `"importance"`: The learner supports extraction of importance
    scores, i.e. comes with an `$importance()` extractor function (see
    section on optional extractors in
    [Learner](https://mlr3.mlr-org.com/reference/Learner.md)).

  - `"selected_features"`: The learner supports extraction of the set of
    selected features, i.e. comes with a `$selected_features()`
    extractor function (see section on optional extractors in
    [Learner](https://mlr3.mlr-org.com/reference/Learner.md)).

  - `"oob_error"`: The learner supports extraction of estimated out of
    bag error, i.e. comes with a `oob_error()` extractor function (see
    section on optional extractors in
    [Learner](https://mlr3.mlr-org.com/reference/Learner.md)).

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

### Method `predict_newdata_fast()`

Predicts outcomes for new data in `newdata` using the model fitted
during `$train()`. This method is faster than `$predict_newdata()` as it
skips assertions, type conversions, encapsulation, and logging.

Unlike `$predict_newdata()`, this method does not return a
[Prediction](https://mlr3.mlr-org.com/reference/Prediction.md) object.
Instead, it returns a list with either a `"response"` or `"prob"`
element, depending on the prediction type.

Note that `state$predict_time` and `state$log` will remain empty after
using this method. Some learners may not support this method and may
fail when it is called. Prefer `$predict_newdata()` unless performance
is critical.

If the model was trained via
[`resample()`](https://mlr3.mlr-org.com/reference/resample.md) or
[`benchmark()`](https://mlr3.mlr-org.com/reference/benchmark.md), you
must pass the associated task object stored in the corresponding
[ResampleResult](https://mlr3.mlr-org.com/reference/ResampleResult.md)
or
[BenchmarkResult](https://mlr3.mlr-org.com/reference/BenchmarkResult.md).

#### Usage

    LearnerClassif$predict_newdata_fast(newdata, task = NULL)

#### Arguments

- `newdata`:

  [`data.table::data.table()`](https://rdatatable.gitlab.io/data.table/reference/data.table.html)  
  New data to predict on.

- `task`:

  ([Task](https://mlr3.mlr-org.com/reference/Task.md)).

#### Returns

[`list()`](https://rdrr.io/r/base/list.html) with elements `"response"`
or `"prob"` depending on the predict type.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    LearnerClassif$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# get all classification learners from mlr_learners:
lrns = mlr_learners$mget(mlr_learners$keys("^classif"))
names(lrns)
#> [1] "classif.debug"       "classif.featureless" "classif.rpart"      

# get a specific learner from mlr_learners:
lrn = lrn("classif.rpart")
print(lrn)
#> 
#> ── <LearnerClassifRpart> (classif.rpart): Classification Tree ──────────────────
#> • Model: -
#> • Parameters: xval=0
#> • Packages: mlr3 and rpart
#> • Predict Types: [response] and prob
#> • Feature Types: logical, integer, numeric, factor, and ordered
#> • Encapsulation: none (fallback: -)
#> • Properties: importance, missings, multiclass, selected_features, twoclass,
#> and weights
#> • Other settings: use_weights = 'use'

# train the learner:
task = tsk("penguins")
lrn$train(task, 1:200)

# predict on new observations:
lrn$predict(task, 201:344)$confusion
#>            truth
#> response    Adelie Chinstrap Gentoo
#>   Adelie         0        62      1
#>   Chinstrap      0         0      0
#>   Gentoo         0         6     75
```
