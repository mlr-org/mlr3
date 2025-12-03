# Akaike Information Criterion Measure

Calculates the Akaike Information Criterion (AIC) which is a trade-off
between goodness of fit (measured in terms of log-likelihood) and model
complexity (measured in terms of number of included features).
Internally, [`stats::AIC()`](https://rdrr.io/r/stats/AIC.html) is called
with parameter `k` (defaulting to 2). Requires the learner property
`"loglik"`, `NA` is returned for unsupported learners.

## Dictionary

This [Measure](https://mlr3.mlr-org.com/reference/Measure.md) can be
instantiated via the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr_measures](https://mlr3.mlr-org.com/reference/mlr_measures.md) or
with the associated sugar function
[`msr()`](https://mlr3.mlr-org.com/reference/mlr_sugar.md):

    mlr_measures$get("aic")
    msr("aic")

## Meta Information

- Task type: “NA”

- Range: \\(-\infty, \infty)\\

- Minimize: TRUE

- Average: macro

- Required Prediction: “NA”

- Required Packages: [mlr3](https://CRAN.R-project.org/package=mlr3)

## Parameters

|     |         |         |                  |
|-----|---------|---------|------------------|
| Id  | Type    | Default | Range            |
| k   | integer | \-      | \\\[0, \infty)\\ |

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
[`MeasureClassif`](https://mlr3.mlr-org.com/reference/MeasureClassif.md),
[`MeasureRegr`](https://mlr3.mlr-org.com/reference/MeasureRegr.md),
[`MeasureSimilarity`](https://mlr3.mlr-org.com/reference/MeasureSimilarity.md),
[`mlr_measures`](https://mlr3.mlr-org.com/reference/mlr_measures.md),
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
`MeasureAIC`

## Methods

### Public methods

- [`MeasureAIC$new()`](#method-MeasureAIC-new)

- [`MeasureAIC$clone()`](#method-MeasureAIC-clone)

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

    MeasureAIC$new()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    MeasureAIC$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
