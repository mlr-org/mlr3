# Cost-sensitive Classification Measure

Uses a cost matrix to create a classification measure. True labels must
be arranged in columns, predicted labels must be arranged in rows. The
cost matrix is stored as slot `$costs`.

For calculation of the score, the confusion matrix is multiplied
element-wise with the cost matrix. The costs are then summed up (and
potentially divided by the number of observations if `normalize` is set
to `TRUE` (default)).

## Dictionary

This [Measure](https://mlr3.mlr-org.com/dev/reference/Measure.md) can be
instantiated via the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr_measures](https://mlr3.mlr-org.com/dev/reference/mlr_measures.md)
or with the associated sugar function
[`msr()`](https://mlr3.mlr-org.com/dev/reference/mlr_sugar.md):

    mlr_measures$get("classif.costs")
    msr("classif.costs")

## Meta Information

- Task type: “classif”

- Range: \\(-\infty, \infty)\\

- Minimize: TRUE

- Average: macro

- Required Prediction: “response”

- Required Packages: [mlr3](https://CRAN.R-project.org/package=mlr3)

## Parameters

|           |         |         |             |
|-----------|---------|---------|-------------|
| Id        | Type    | Default | Levels      |
| normalize | logical | \-      | TRUE, FALSE |

## See also

- Chapter in the [mlr3book](https://mlr3book.mlr-org.com/):
  <https://mlr3book.mlr-org.com/chapters/chapter2/data_and_basic_modeling.html#sec-eval>

- Package
  [mlr3measures](https://CRAN.R-project.org/package=mlr3measures) for
  the scoring functions.
  [Dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
  of [Measures](https://mlr3.mlr-org.com/dev/reference/Measure.md):
  [mlr_measures](https://mlr3.mlr-org.com/dev/reference/mlr_measures.md)
  `as.data.table(mlr_measures)` for a table of available
  [Measures](https://mlr3.mlr-org.com/dev/reference/Measure.md) in the
  running session (depending on the loaded packages).

- Extension packages for additional task types:

  - [mlr3proba](https://CRAN.R-project.org/package=mlr3proba) for
    probabilistic supervised regression and survival analysis.

  - [mlr3cluster](https://CRAN.R-project.org/package=mlr3cluster) for
    unsupervised clustering.

Other Measure:
[`Measure`](https://mlr3.mlr-org.com/dev/reference/Measure.md),
[`MeasureClassif`](https://mlr3.mlr-org.com/dev/reference/MeasureClassif.md),
[`MeasureRegr`](https://mlr3.mlr-org.com/dev/reference/MeasureRegr.md),
[`MeasureSimilarity`](https://mlr3.mlr-org.com/dev/reference/MeasureSimilarity.md),
[`mlr_measures`](https://mlr3.mlr-org.com/dev/reference/mlr_measures.md),
[`mlr_measures_aic`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_aic.md),
[`mlr_measures_bic`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_bic.md),
[`mlr_measures_debug_classif`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_debug_classif.md),
[`mlr_measures_elapsed_time`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_elapsed_time.md),
[`mlr_measures_internal_valid_score`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_internal_valid_score.md),
[`mlr_measures_oob_error`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_oob_error.md),
[`mlr_measures_regr.pinball`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_regr.pinball.md),
[`mlr_measures_regr.rqr`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_regr.rqr.md),
[`mlr_measures_regr.rsq`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_regr.rsq.md),
[`mlr_measures_selected_features`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_selected_features.md)

Other classification measures:
[`mlr_measures_classif.acc`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.acc.md),
[`mlr_measures_classif.auc`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.auc.md),
[`mlr_measures_classif.bacc`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.bacc.md),
[`mlr_measures_classif.bbrier`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.bbrier.md),
[`mlr_measures_classif.ce`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.ce.md),
[`mlr_measures_classif.dor`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.dor.md),
[`mlr_measures_classif.fbeta`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.fbeta.md),
[`mlr_measures_classif.fdr`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.fdr.md),
[`mlr_measures_classif.fn`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.fn.md),
[`mlr_measures_classif.fnr`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.fnr.md),
[`mlr_measures_classif.fomr`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.fomr.md),
[`mlr_measures_classif.fp`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.fp.md),
[`mlr_measures_classif.fpr`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.fpr.md),
[`mlr_measures_classif.logloss`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.logloss.md),
[`mlr_measures_classif.mauc_au1p`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.mauc_au1p.md),
[`mlr_measures_classif.mauc_au1u`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.mauc_au1u.md),
[`mlr_measures_classif.mauc_aunp`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.mauc_aunp.md),
[`mlr_measures_classif.mauc_aunu`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.mauc_aunu.md),
[`mlr_measures_classif.mauc_mu`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.mauc_mu.md),
[`mlr_measures_classif.mbrier`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.mbrier.md),
[`mlr_measures_classif.mcc`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.mcc.md),
[`mlr_measures_classif.npv`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.npv.md),
[`mlr_measures_classif.ppv`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.ppv.md),
[`mlr_measures_classif.prauc`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.prauc.md),
[`mlr_measures_classif.precision`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.precision.md),
[`mlr_measures_classif.recall`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.recall.md),
[`mlr_measures_classif.sensitivity`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.sensitivity.md),
[`mlr_measures_classif.specificity`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.specificity.md),
[`mlr_measures_classif.tn`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.tn.md),
[`mlr_measures_classif.tnr`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.tnr.md),
[`mlr_measures_classif.tp`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.tp.md),
[`mlr_measures_classif.tpr`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.tpr.md)

Other multiclass classification measures:
[`mlr_measures_classif.acc`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.acc.md),
[`mlr_measures_classif.bacc`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.bacc.md),
[`mlr_measures_classif.ce`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.ce.md),
[`mlr_measures_classif.logloss`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.logloss.md),
[`mlr_measures_classif.mauc_au1p`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.mauc_au1p.md),
[`mlr_measures_classif.mauc_au1u`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.mauc_au1u.md),
[`mlr_measures_classif.mauc_aunp`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.mauc_aunp.md),
[`mlr_measures_classif.mauc_aunu`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.mauc_aunu.md),
[`mlr_measures_classif.mauc_mu`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.mauc_mu.md),
[`mlr_measures_classif.mbrier`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.mbrier.md),
[`mlr_measures_classif.mcc`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.mcc.md)

## Super classes

[`mlr3::Measure`](https://mlr3.mlr-org.com/dev/reference/Measure.md) -\>
[`mlr3::MeasureClassif`](https://mlr3.mlr-org.com/dev/reference/MeasureClassif.md)
-\> `MeasureClassifCosts`

## Active bindings

- `costs`:

  (numeric [`matrix()`](https://rdrr.io/r/base/matrix.html))  
  Matrix of costs (truth in columns, predicted response in rows).

## Methods

### Public methods

- [`MeasureClassifCosts$new()`](#method-MeasureClassifCosts-new)

- [`MeasureClassifCosts$clone()`](#method-MeasureClassifCosts-clone)

Inherited methods

- [`mlr3::Measure$aggregate()`](https://mlr3.mlr-org.com/dev/reference/Measure.html#method-aggregate)
- [`mlr3::Measure$format()`](https://mlr3.mlr-org.com/dev/reference/Measure.html#method-format)
- [`mlr3::Measure$help()`](https://mlr3.mlr-org.com/dev/reference/Measure.html#method-help)
- [`mlr3::Measure$print()`](https://mlr3.mlr-org.com/dev/reference/Measure.html#method-print)
- [`mlr3::Measure$score()`](https://mlr3.mlr-org.com/dev/reference/Measure.html#method-score)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    MeasureClassifCosts$new()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    MeasureClassifCosts$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# get a cost sensitive task
task = tsk("german_credit")

# cost matrix as given on the UCI page of the german credit data set
# https://archive.ics.uci.edu/ml/datasets/statlog+(german+credit+data)
costs = matrix(c(0, 5, 1, 0), nrow = 2)
dimnames(costs) = list(truth = task$class_names, predicted = task$class_names)
print(costs)
#>       predicted
#> truth  good bad
#>   good    0   1
#>   bad     5   0

# mlr3 needs truth in columns, predictions in rows
costs = t(costs)

# create a cost measure which calculates the absolute costs
m = msr("classif.costs", id = "german_credit_costs", costs = costs, normalize = FALSE)

# fit models and evaluate with the cost measure
learner = lrn("classif.rpart")
rr = resample(task, learner, rsmp("cv", folds = 3))
rr$aggregate(m)
#> german_credit_costs 
#>                 334 
```
