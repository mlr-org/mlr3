# Featureless Classification Learner

A simple
[LearnerClassif](https://mlr3.mlr-org.com/reference/LearnerClassif.md)
which only analyzes the labels during train, ignoring all features.
Hyperparameter `method` determines the mode of operation during
prediction:

- mode::

  Predicts the most frequent label. If there are two or more labels
  tied, randomly selects one per prediction. Probabilities correspond to
  the relative frequency of the class labels in the training set. For
  weighted data, the label(s) with the highest weighted frequency are
  selected.

- sample::

  Randomly predict a label uniformly. Probabilities correspond to a
  uniform distribution of class labels, i.e. 1 divided by the number of
  classes. Weights are ignored, if present.

- weighted.sample::

  Randomly predict a label, with probability estimated from the training
  distribution. For consistency, probabilities are 1 for the sampled
  label and 0 for all other labels. For weighted data, sample weights
  are used to weight the class labels.

## Dictionary

This [Learner](https://mlr3.mlr-org.com/reference/Learner.md) can be
instantiated via the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr_learners](https://mlr3.mlr-org.com/reference/mlr_learners.md) or
with the associated sugar function
[`lrn()`](https://mlr3.mlr-org.com/reference/mlr_sugar.md):

    mlr_learners$get("classif.featureless")
    lrn("classif.featureless")

## Meta Information

- Task type: “classif”

- Predict Types: “response”, “prob”

- Feature Types: “logical”, “integer”, “numeric”, “character”, “factor”,
  “ordered”, “POSIXct”, “Date”

- Required Packages: [mlr3](https://CRAN.R-project.org/package=mlr3)

## Parameters

|        |           |         |                               |
|--------|-----------|---------|-------------------------------|
| Id     | Type      | Default | Levels                        |
| method | character | mode    | mode, sample, weighted.sample |

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
[`mlr_learners_classif.rpart`](https://mlr3.mlr-org.com/reference/mlr_learners_classif.rpart.md),
[`mlr_learners_regr.debug`](https://mlr3.mlr-org.com/reference/mlr_learners_regr.debug.md),
[`mlr_learners_regr.featureless`](https://mlr3.mlr-org.com/reference/mlr_learners_regr.featureless.md),
[`mlr_learners_regr.rpart`](https://mlr3.mlr-org.com/reference/mlr_learners_regr.rpart.md)

## Super classes

[`mlr3::Learner`](https://mlr3.mlr-org.com/reference/Learner.md) -\>
[`mlr3::LearnerClassif`](https://mlr3.mlr-org.com/reference/LearnerClassif.md)
-\> `LearnerClassifFeatureless`

## Methods

### Public methods

- [`LearnerClassifFeatureless$new()`](#method-LearnerClassifFeatureless-new)

- [`LearnerClassifFeatureless$importance()`](#method-LearnerClassifFeatureless-importance)

- [`LearnerClassifFeatureless$selected_features()`](#method-LearnerClassifFeatureless-selected_features)

- [`LearnerClassifFeatureless$clone()`](#method-LearnerClassifFeatureless-clone)

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
- [`mlr3::LearnerClassif$predict_newdata_fast()`](https://mlr3.mlr-org.com/reference/LearnerClassif.html#method-predict_newdata_fast)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    LearnerClassifFeatureless$new()

------------------------------------------------------------------------

### Method `importance()`

All features have a score of `0` for this learner.

#### Usage

    LearnerClassifFeatureless$importance()

#### Returns

Named [`numeric()`](https://rdrr.io/r/base/numeric.html).

------------------------------------------------------------------------

### Method `selected_features()`

Selected features are always the empty set for this learner.

#### Usage

    LearnerClassifFeatureless$selected_features()

#### Returns

`character(0)`.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    LearnerClassifFeatureless$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
