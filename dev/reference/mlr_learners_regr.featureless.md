# Featureless Regression Learner

A simple
[LearnerRegr](https://mlr3.mlr-org.com/dev/reference/LearnerRegr.md)
which only analyzes the response during train, ignoring all features. If
hyperparameter `robust` is `FALSE` (default), constantly predicts
`mean(y)` as response and `sd(y)` as standard error. If `robust` is
`TRUE`, [`median()`](https://rdrr.io/r/stats/median.html) and
[`mad()`](https://rdrr.io/r/stats/mad.html) are used instead of
[`mean()`](https://rdrr.io/r/base/mean.html) and
[`sd()`](https://rdrr.io/r/stats/sd.html), respectively.

For weighted data, the response is the weighted mean (weighted median
for robust regression). The predicted standard error is the square root
of the weighted variance estimator with bias correction based on
effective degrees of freedom:

    sd(y, weights) = sqrt(
      sum(weights * (y - weighted.mean(y, weights))^2) /
        (sum(weights) - sum(weights ^2) / sum(weights))
    )

If `robust` is `TRUE`, the weighted median absolute deviation is used,
adjusted by a factor of 1.4826 for consistency with
[`mad()`](https://rdrr.io/r/stats/mad.html).

## Dictionary

This [Learner](https://mlr3.mlr-org.com/dev/reference/Learner.md) can be
instantiated via the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr_learners](https://mlr3.mlr-org.com/dev/reference/mlr_learners.md)
or with the associated sugar function
[`lrn()`](https://mlr3.mlr-org.com/dev/reference/mlr_sugar.md):

    mlr_learners$get("regr.featureless")
    lrn("regr.featureless")

## Meta Information

- Task type: “regr”

- Predict Types: “response”, “se”, “quantiles”

- Feature Types: “logical”, “integer”, “numeric”, “character”, “factor”,
  “ordered”, “POSIXct”, “Date”

- Required Packages: [mlr3](https://CRAN.R-project.org/package=mlr3),
  'stats'

## Parameters

|        |         |         |             |
|--------|---------|---------|-------------|
| Id     | Type    | Default | Levels      |
| robust | logical | TRUE    | TRUE, FALSE |

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
  of [Learners](https://mlr3.mlr-org.com/dev/reference/Learner.md):
  [mlr_learners](https://mlr3.mlr-org.com/dev/reference/mlr_learners.md)

- `as.data.table(mlr_learners)` for a table of available
  [Learners](https://mlr3.mlr-org.com/dev/reference/Learner.md) in the
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
[`Learner`](https://mlr3.mlr-org.com/dev/reference/Learner.md),
[`LearnerClassif`](https://mlr3.mlr-org.com/dev/reference/LearnerClassif.md),
[`LearnerRegr`](https://mlr3.mlr-org.com/dev/reference/LearnerRegr.md),
[`mlr_learners`](https://mlr3.mlr-org.com/dev/reference/mlr_learners.md),
[`mlr_learners_classif.debug`](https://mlr3.mlr-org.com/dev/reference/mlr_learners_classif.debug.md),
[`mlr_learners_classif.featureless`](https://mlr3.mlr-org.com/dev/reference/mlr_learners_classif.featureless.md),
[`mlr_learners_classif.rpart`](https://mlr3.mlr-org.com/dev/reference/mlr_learners_classif.rpart.md),
[`mlr_learners_regr.debug`](https://mlr3.mlr-org.com/dev/reference/mlr_learners_regr.debug.md),
[`mlr_learners_regr.rpart`](https://mlr3.mlr-org.com/dev/reference/mlr_learners_regr.rpart.md)

## Super classes

[`Learner`](https://mlr3.mlr-org.com/dev/reference/Learner.md) -\>
[`LearnerRegr`](https://mlr3.mlr-org.com/dev/reference/LearnerRegr.md)
-\> `LearnerRegrFeatureless`

## Methods

### Public methods

- [`LearnerRegrFeatureless$new()`](#method-LearnerRegrFeatureless-initialize)

- [`LearnerRegrFeatureless$importance()`](#method-LearnerRegrFeatureless-importance)

- [`LearnerRegrFeatureless$selected_features()`](#method-LearnerRegrFeatureless-selected_features)

- [`LearnerRegrFeatureless$clone()`](#method-LearnerRegrFeatureless-clone)

Inherited methods

- [`Learner$base_learner()`](https://mlr3.mlr-org.com/dev/reference/Learner.html#method-base_learner)
- [`Learner$configure()`](https://mlr3.mlr-org.com/dev/reference/Learner.html#method-configure)
- [`Learner$encapsulate()`](https://mlr3.mlr-org.com/dev/reference/Learner.html#method-encapsulate)
- [`Learner$format()`](https://mlr3.mlr-org.com/dev/reference/Learner.html#method-format)
- [`Learner$help()`](https://mlr3.mlr-org.com/dev/reference/Learner.html#method-help)
- [`Learner$predict()`](https://mlr3.mlr-org.com/dev/reference/Learner.html#method-predict)
- [`Learner$predict_newdata()`](https://mlr3.mlr-org.com/dev/reference/Learner.html#method-predict_newdata)
- [`Learner$print()`](https://mlr3.mlr-org.com/dev/reference/Learner.html#method-print)
- [`Learner$reset()`](https://mlr3.mlr-org.com/dev/reference/Learner.html#method-reset)
- [`Learner$train()`](https://mlr3.mlr-org.com/dev/reference/Learner.html#method-train)
- [`LearnerRegr$predict_newdata_fast()`](https://mlr3.mlr-org.com/dev/reference/LearnerRegr.html#method-predict_newdata_fast)

------------------------------------------------------------------------

### `LearnerRegrFeatureless$new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    LearnerRegrFeatureless$new()

------------------------------------------------------------------------

### `LearnerRegrFeatureless$importance()`

All features have a score of `0` for this learner.

#### Usage

    LearnerRegrFeatureless$importance()

#### Returns

Named [`numeric()`](https://rdrr.io/r/base/numeric.html).

------------------------------------------------------------------------

### `LearnerRegrFeatureless$selected_features()`

Selected features are always the empty set for this learner.

#### Usage

    LearnerRegrFeatureless$selected_features()

#### Returns

`character(0)`.

------------------------------------------------------------------------

### `LearnerRegrFeatureless$clone()`

The objects of this class are cloneable with this method.

#### Usage

    LearnerRegrFeatureless$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
