# Prediction Object for Regression

This object wraps the predictions returned by a learner of class
[LearnerRegr](https://mlr3.mlr-org.com/reference/LearnerRegr.md), i.e.
the predicted response and standard error. Additionally, probability
distributions implemented in package `distr6` are supported.

## See also

- Chapter in the [mlr3book](https://mlr3book.mlr-org.com/):
  <https://mlr3book.mlr-org.com/chapters/chapter2/data_and_basic_modeling.html>

- Package [mlr3viz](https://CRAN.R-project.org/package=mlr3viz) for some
  generic visualizations.

- Extension packages for additional task types:

  - [mlr3proba](https://CRAN.R-project.org/package=mlr3proba) for
    probabilistic supervised regression and survival analysis.

  - [mlr3cluster](https://CRAN.R-project.org/package=mlr3cluster) for
    unsupervised clustering.

Other Prediction:
[`Prediction`](https://mlr3.mlr-org.com/reference/Prediction.md),
[`PredictionClassif`](https://mlr3.mlr-org.com/reference/PredictionClassif.md)

## Super class

[`mlr3::Prediction`](https://mlr3.mlr-org.com/reference/Prediction.md)
-\> `PredictionRegr`

## Active bindings

- `response`:

  ([`numeric()`](https://rdrr.io/r/base/numeric.html))  
  Access the stored predicted response.

- `se`:

  ([`numeric()`](https://rdrr.io/r/base/numeric.html))  
  Access the stored standard error.

- `quantiles`:

  ([`matrix()`](https://rdrr.io/r/base/matrix.html))  
  Matrix of predicted quantiles. Observations are in rows, quantile (in
  ascending order) in columns.

- `distr`:

  (`VectorDistribution`)  
  Access the stored vector distribution. Requires package `distr6`(in
  repository <https://raphaels1.r-universe.dev>) .

## Methods

### Public methods

- [`PredictionRegr$new()`](#method-PredictionRegr-new)

- [`PredictionRegr$clone()`](#method-PredictionRegr-clone)

Inherited methods

- [`mlr3::Prediction$filter()`](https://mlr3.mlr-org.com/reference/Prediction.html#method-filter)
- [`mlr3::Prediction$format()`](https://mlr3.mlr-org.com/reference/Prediction.html#method-format)
- [`mlr3::Prediction$help()`](https://mlr3.mlr-org.com/reference/Prediction.html#method-help)
- [`mlr3::Prediction$obs_loss()`](https://mlr3.mlr-org.com/reference/Prediction.html#method-obs_loss)
- [`mlr3::Prediction$print()`](https://mlr3.mlr-org.com/reference/Prediction.html#method-print)
- [`mlr3::Prediction$score()`](https://mlr3.mlr-org.com/reference/Prediction.html#method-score)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    PredictionRegr$new(
      task = NULL,
      row_ids = task$row_ids,
      truth = task$truth(),
      response = NULL,
      se = NULL,
      quantiles = NULL,
      distr = NULL,
      weights = NULL,
      check = TRUE,
      extra = NULL
    )

#### Arguments

- `task`:

  ([TaskRegr](https://mlr3.mlr-org.com/reference/TaskRegr.md))  
  Task, used to extract defaults for `row_ids` and `truth`.

- `row_ids`:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  Row ids of the predicted observations, i.e. the row ids of the test
  set.

- `truth`:

  ([`numeric()`](https://rdrr.io/r/base/numeric.html))  
  True (observed) response.

- `response`:

  ([`numeric()`](https://rdrr.io/r/base/numeric.html))  
  Vector of numeric response values. One element for each observation in
  the test set.

- `se`:

  ([`numeric()`](https://rdrr.io/r/base/numeric.html))  
  Numeric vector of predicted standard errors. One element for each
  observation in the test set.

- `quantiles`:

  ([`matrix()`](https://rdrr.io/r/base/matrix.html))  
  Numeric matrix of predicted quantiles. One row per observation, one
  column per quantile.

- `distr`:

  (`VectorDistribution`)  
  `VectorDistribution` from package distr6 (in repository
  <https://raphaels1.r-universe.dev>). Each individual distribution in
  the vector represents the random variable 'survival time' for an
  individual observation.

- `weights`:

  ([`numeric()`](https://rdrr.io/r/base/numeric.html))  
  Vector of measure weights for each observation. Should be constructed
  from the `Task`'s `weights_measure` column.

- `check`:

  (`logical(1)`)  
  If `TRUE`, performs some argument checks and predict type conversions.

- `extra`:

  ([`list()`](https://rdrr.io/r/base/list.html))  
  List of extra data to be stored in the prediction object.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    PredictionRegr$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
task = tsk("california_housing")
learner = lrn("regr.featureless", predict_type = "se")
p = learner$train(task)$predict(task)
p$predict_types
#> [1] "response" "se"      
head(as.data.table(p))
#>    row_ids  truth response       se
#>      <int>  <num>    <num>    <num>
#> 1:       1 452600 206855.8 115395.6
#> 2:       2 358500 206855.8 115395.6
#> 3:       3 352100 206855.8 115395.6
#> 4:       4 341300 206855.8 115395.6
#> 5:       5 342200 206855.8 115395.6
#> 6:       6 269700 206855.8 115395.6
```
