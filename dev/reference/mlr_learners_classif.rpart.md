# Classification Tree Learner

A
[LearnerClassif](https://mlr3.mlr-org.com/dev/reference/LearnerClassif.md)
for a classification tree implemented in
[`rpart::rpart()`](https://rdrr.io/pkg/rpart/man/rpart.html) in package
[rpart](https://CRAN.R-project.org/package=rpart).

## Initial parameter values

- Parameter `xval` is initialized to 0 in order to save some computation
  time.

## Custom mlr3 parameters

- Parameter `model` has been renamed to `keep_model`.

## Dictionary

This [Learner](https://mlr3.mlr-org.com/dev/reference/Learner.md) can be
instantiated via the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr_learners](https://mlr3.mlr-org.com/dev/reference/mlr_learners.md)
or with the associated sugar function
[`lrn()`](https://mlr3.mlr-org.com/dev/reference/mlr_sugar.md):

    mlr_learners$get("classif.rpart")
    lrn("classif.rpart")

## Meta Information

- Task type: “classif”

- Predict Types: “response”, “prob”

- Feature Types: “logical”, “integer”, “numeric”, “factor”, “ordered”

- Required Packages: [mlr3](https://CRAN.R-project.org/package=mlr3),
  [rpart](https://CRAN.R-project.org/package=rpart)

## Parameters

|                |         |         |             |                  |
|----------------|---------|---------|-------------|------------------|
| Id             | Type    | Default | Levels      | Range            |
| cp             | numeric | 0.01    |             | \\\[0, 1\]\\     |
| keep_model     | logical | FALSE   | TRUE, FALSE | \-               |
| maxcompete     | integer | 4       |             | \\\[0, \infty)\\ |
| maxdepth       | integer | 30      |             | \\\[1, 30\]\\    |
| maxsurrogate   | integer | 5       |             | \\\[0, \infty)\\ |
| minbucket      | integer | \-      |             | \\\[1, \infty)\\ |
| minsplit       | integer | 20      |             | \\\[1, \infty)\\ |
| surrogatestyle | integer | 0       |             | \\\[0, 1\]\\     |
| usesurrogate   | integer | 2       |             | \\\[0, 2\]\\     |
| xval           | integer | 10      |             | \\\[0, \infty)\\ |

## References

Breiman L, Friedman JH, Olshen RA, Stone CJ (1984). *Classification And
Regression Trees*. Routledge.
[doi:10.1201/9781315139470](https://doi.org/10.1201/9781315139470) .

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
[`mlr_learners_regr.debug`](https://mlr3.mlr-org.com/dev/reference/mlr_learners_regr.debug.md),
[`mlr_learners_regr.featureless`](https://mlr3.mlr-org.com/dev/reference/mlr_learners_regr.featureless.md),
[`mlr_learners_regr.rpart`](https://mlr3.mlr-org.com/dev/reference/mlr_learners_regr.rpart.md)

## Super classes

[`mlr3::Learner`](https://mlr3.mlr-org.com/dev/reference/Learner.md) -\>
[`mlr3::LearnerClassif`](https://mlr3.mlr-org.com/dev/reference/LearnerClassif.md)
-\> `LearnerClassifRpart`

## Methods

### Public methods

- [`LearnerClassifRpart$new()`](#method-LearnerClassifRpart-new)

- [`LearnerClassifRpart$importance()`](#method-LearnerClassifRpart-importance)

- [`LearnerClassifRpart$selected_features()`](#method-LearnerClassifRpart-selected_features)

- [`LearnerClassifRpart$clone()`](#method-LearnerClassifRpart-clone)

Inherited methods

- [`mlr3::Learner$base_learner()`](https://mlr3.mlr-org.com/dev/reference/Learner.html#method-base_learner)
- [`mlr3::Learner$configure()`](https://mlr3.mlr-org.com/dev/reference/Learner.html#method-configure)
- [`mlr3::Learner$encapsulate()`](https://mlr3.mlr-org.com/dev/reference/Learner.html#method-encapsulate)
- [`mlr3::Learner$format()`](https://mlr3.mlr-org.com/dev/reference/Learner.html#method-format)
- [`mlr3::Learner$help()`](https://mlr3.mlr-org.com/dev/reference/Learner.html#method-help)
- [`mlr3::Learner$predict()`](https://mlr3.mlr-org.com/dev/reference/Learner.html#method-predict)
- [`mlr3::Learner$predict_newdata()`](https://mlr3.mlr-org.com/dev/reference/Learner.html#method-predict_newdata)
- [`mlr3::Learner$print()`](https://mlr3.mlr-org.com/dev/reference/Learner.html#method-print)
- [`mlr3::Learner$reset()`](https://mlr3.mlr-org.com/dev/reference/Learner.html#method-reset)
- [`mlr3::Learner$train()`](https://mlr3.mlr-org.com/dev/reference/Learner.html#method-train)
- [`mlr3::LearnerClassif$predict_newdata_fast()`](https://mlr3.mlr-org.com/dev/reference/LearnerClassif.html#method-predict_newdata_fast)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    LearnerClassifRpart$new()

------------------------------------------------------------------------

### Method `importance()`

The importance scores are extracted from the model slot
`variable.importance`.

#### Usage

    LearnerClassifRpart$importance()

#### Returns

Named [`numeric()`](https://rdrr.io/r/base/numeric.html).

------------------------------------------------------------------------

### Method `selected_features()`

Selected features are extracted from the model slot `frame$var`.

#### Usage

    LearnerClassifRpart$selected_features()

#### Returns

[`character()`](https://rdrr.io/r/base/character.html).

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    LearnerClassifRpart$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
