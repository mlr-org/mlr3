# Regression Learner for Debugging

A simple
[LearnerRegr](https://mlr3.mlr-org.com/reference/LearnerRegr.md) used
primarily in the unit tests and for debugging purposes. If no
hyperparameter is set, it simply constantly predicts the mean value of
the training data. The following hyperparameters trigger the following
actions:

- predict_missing::

  Ratio of predictions which will be NA.

- predict_missing_type::

  To to encode missingness. “na” will insert NA values, “omit” will just
  return fewer predictions than requested.

- save_tasks::

  Saves input task in `model` slot during training and prediction.

- threads::

  Number of threads to use. Has no effect.

- x::

  Numeric tuning parameter. Has no effect.

## Dictionary

This [Learner](https://mlr3.mlr-org.com/reference/Learner.md) can be
instantiated via the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr_learners](https://mlr3.mlr-org.com/reference/mlr_learners.md) or
with the associated sugar function
[`lrn()`](https://mlr3.mlr-org.com/reference/mlr_sugar.md):

    mlr_learners$get("regr.debug")
    lrn("regr.debug")

## Meta Information

- Task type: “regr”

- Predict Types: “response”, “se”, “quantiles”

- Feature Types: “logical”, “integer”, “numeric”, “character”, “factor”,
  “ordered”

- Required Packages: [mlr3](https://CRAN.R-project.org/package=mlr3),
  'stats'

## Parameters

|                      |           |         |             |                  |
|----------------------|-----------|---------|-------------|------------------|
| Id                   | Type      | Default | Levels      | Range            |
| error_predict        | numeric   | 0       |             | \\\[0, 1\]\\     |
| error_train          | numeric   | 0       |             | \\\[0, 1\]\\     |
| message_predict      | numeric   | 0       |             | \\\[0, 1\]\\     |
| message_train        | numeric   | 0       |             | \\\[0, 1\]\\     |
| predict_missing      | numeric   | 0       |             | \\\[0, 1\]\\     |
| predict_missing_type | character | na      | na, omit    | \-               |
| save_tasks           | logical   | FALSE   | TRUE, FALSE | \-               |
| segfault_predict     | numeric   | 0       |             | \\\[0, 1\]\\     |
| segfault_train       | numeric   | 0       |             | \\\[0, 1\]\\     |
| threads              | integer   | \-      |             | \\\[1, \infty)\\ |
| warning_predict      | numeric   | 0       |             | \\\[0, 1\]\\     |
| warning_train        | numeric   | 0       |             | \\\[0, 1\]\\     |
| x                    | numeric   | \-      |             | \\\[0, 1\]\\     |

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
[`LearnerClassif`](https://mlr3.mlr-org.com/reference/LearnerClassif.md),
[`LearnerRegr`](https://mlr3.mlr-org.com/reference/LearnerRegr.md),
[`mlr_learners`](https://mlr3.mlr-org.com/reference/mlr_learners.md),
[`mlr_learners_classif.debug`](https://mlr3.mlr-org.com/reference/mlr_learners_classif.debug.md),
[`mlr_learners_classif.featureless`](https://mlr3.mlr-org.com/reference/mlr_learners_classif.featureless.md),
[`mlr_learners_classif.rpart`](https://mlr3.mlr-org.com/reference/mlr_learners_classif.rpart.md),
[`mlr_learners_regr.featureless`](https://mlr3.mlr-org.com/reference/mlr_learners_regr.featureless.md),
[`mlr_learners_regr.rpart`](https://mlr3.mlr-org.com/reference/mlr_learners_regr.rpart.md)

## Super classes

[`mlr3::Learner`](https://mlr3.mlr-org.com/reference/Learner.md) -\>
[`mlr3::LearnerRegr`](https://mlr3.mlr-org.com/reference/LearnerRegr.md)
-\> `LearnerRegrDebug`

## Methods

### Public methods

- [`LearnerRegrDebug$new()`](#method-LearnerRegrDebug-new)

- [`LearnerRegrDebug$importance()`](#method-LearnerRegrDebug-importance)

- [`LearnerRegrDebug$selected_features()`](#method-LearnerRegrDebug-selected_features)

- [`LearnerRegrDebug$clone()`](#method-LearnerRegrDebug-clone)

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
- [`mlr3::Learner$train()`](https://mlr3.mlr-org.com/reference/Learner.html#method-train)
- [`mlr3::LearnerRegr$predict_newdata_fast()`](https://mlr3.mlr-org.com/reference/LearnerRegr.html#method-predict_newdata_fast)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    LearnerRegrDebug$new()

------------------------------------------------------------------------

### Method `importance()`

Returns 0 for each feature seen in training.

#### Usage

    LearnerRegrDebug$importance()

#### Returns

Named [`numeric()`](https://rdrr.io/r/base/numeric.html).

------------------------------------------------------------------------

### Method `selected_features()`

Always returns character(0).

#### Usage

    LearnerRegrDebug$selected_features()

#### Returns

[`character()`](https://rdrr.io/r/base/character.html).

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    LearnerRegrDebug$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
task = tsk("mtcars")
learner = lrn("regr.debug", save_tasks = TRUE)
learner$train(task, row_ids = 1:20)
prediction = learner$predict(task, row_ids = 21:32)

learner$model$task_train
#> 
#> ── <TaskRegr> (20x11): Motor Trends ────────────────────────────────────────────
#> • Target: mpg
#> • Properties: -
#> • Features (10):
#>   • dbl (10): am, carb, cyl, disp, drat, gear, hp, qsec, vs, wt
learner$model$task_predict
#> 
#> ── <TaskRegr> (12x11): Motor Trends ────────────────────────────────────────────
#> • Target: mpg
#> • Properties: -
#> • Features (10):
#>   • dbl (10): am, carb, cyl, disp, drat, gear, hp, qsec, vs, wt
```
