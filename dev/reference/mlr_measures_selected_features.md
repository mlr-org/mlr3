# Selected Features Measure

Measures the number of selected features by extracting it from learners
with property `"selected_features"`. If parameter `normalize` is set to
`TRUE`, the relative number of features instead of the absolute number
of features is returned. Note that the models must be stored to be able
to extract this information. If the learner does not support the
extraction of used features, `NA` is returned.

This measure requires the
[Task](https://mlr3.mlr-org.com/dev/reference/Task.md) and the
[Learner](https://mlr3.mlr-org.com/dev/reference/Learner.md) for
scoring.

## Dictionary

This [Measure](https://mlr3.mlr-org.com/dev/reference/Measure.md) can be
instantiated via the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr_measures](https://mlr3.mlr-org.com/dev/reference/mlr_measures.md)
or with the associated sugar function
[`msr()`](https://mlr3.mlr-org.com/dev/reference/mlr_sugar.md):

    mlr_measures$get("selected_features")
    msr("selected_features")

## Meta Information

- Task type: “NA”

- Range: \\\[0, \infty)\\

- Minimize: TRUE

- Average: macro

- Required Prediction: “NA”

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
[`mlr_measures_classif.costs`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_classif.costs.md),
[`mlr_measures_debug_classif`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_debug_classif.md),
[`mlr_measures_elapsed_time`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_elapsed_time.md),
[`mlr_measures_internal_valid_score`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_internal_valid_score.md),
[`mlr_measures_oob_error`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_oob_error.md),
[`mlr_measures_regr.pinball`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_regr.pinball.md),
[`mlr_measures_regr.rqr`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_regr.rqr.md),
[`mlr_measures_regr.rsq`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_regr.rsq.md)

## Super class

[`mlr3::Measure`](https://mlr3.mlr-org.com/dev/reference/Measure.md) -\>
`MeasureSelectedFeatures`

## Methods

### Public methods

- [`MeasureSelectedFeatures$new()`](#method-MeasureSelectedFeatures-new)

- [`MeasureSelectedFeatures$clone()`](#method-MeasureSelectedFeatures-clone)

Inherited methods

- [`mlr3::Measure$aggregate()`](https://mlr3.mlr-org.com/dev/reference/Measure.html#method-aggregate)
- [`mlr3::Measure$format()`](https://mlr3.mlr-org.com/dev/reference/Measure.html#method-format)
- [`mlr3::Measure$help()`](https://mlr3.mlr-org.com/dev/reference/Measure.html#method-help)
- [`mlr3::Measure$obs_loss()`](https://mlr3.mlr-org.com/dev/reference/Measure.html#method-obs_loss)
- [`mlr3::Measure$print()`](https://mlr3.mlr-org.com/dev/reference/Measure.html#method-print)
- [`mlr3::Measure$score()`](https://mlr3.mlr-org.com/dev/reference/Measure.html#method-score)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    MeasureSelectedFeatures$new()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    MeasureSelectedFeatures$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
task = tsk("german_credit")
learner = lrn("classif.rpart")
rr = resample(task, learner, rsmp("cv", folds = 3), store_models = TRUE)

scores = rr$score(msr("selected_features"))
scores[, c("iteration", "selected_features")]
#>    iteration selected_features
#>        <int>             <num>
#> 1:         1                12
#> 2:         2                 7
#> 3:         3                 7
```
