# R-Squared for Quantile Regression

Measure to compare true observed response with predicted quantiles in
regression tasks.

## Details

\\R^1(\alpha)\\ is defined as \$\$ 1 - \frac{\sum\_{i=1}^n \rho\_\alpha
\left( t_i - r_i(\alpha) \right)}{\sum\_{i=1}^n \rho\_\alpha \left(
t_i - q\_{\alpha} \right)}, \$\$ where for a quantile \\\alpha\\,
\\\rho\_\alpha\\ is the pinball function, \\r_i(\alpha)\\ are the
predictions for the quantile and \\q\_{\alpha}\\ is the empirical
\\\alpha\\-quantile of the test or training data.

\\R^1(\alpha)\\ is analogous to \\R^2\\ for regression tasks. It
compares the pinball function of the predictions relative to a naive
model predicting the empirical quantile.

This measure is undefined for constant \\t\\.

## Dictionary

This [Measure](https://mlr3.mlr-org.com/dev/reference/Measure.md) can be
instantiated via the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr_measures](https://mlr3.mlr-org.com/dev/reference/mlr_measures.md)
or with the associated sugar function
[`msr()`](https://mlr3.mlr-org.com/dev/reference/mlr_sugar.md):

    mlr_measures$get("regr.rqr")
    msr("regr.rqr")

## Meta Information

- Task type: “regr”

- Range: \\(-\infty, 1\]\\

- Minimize: FALSE

- Average: macro

- Required Prediction: “quantiles”

- Required Packages: [mlr3](https://CRAN.R-project.org/package=mlr3)

## Parameters

|       |         |         |              |
|-------|---------|---------|--------------|
| Id    | Type    | Default | Range        |
| alpha | numeric | \-      | \\\[0, 1\]\\ |

## References

Koenker, Roger, Machado, F. JA (1999). “Goodness of Fit and Related
Inference Processes for Quantile Regression.” *Journal of the American
Statistical Association*, **94**(448), 1296–1310.
[doi:10.1080/01621459.1999.10473882](https://doi.org/10.1080/01621459.1999.10473882)
.

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
[`mlr_measures_regr.rsq`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_regr.rsq.md),
[`mlr_measures_selected_features`](https://mlr3.mlr-org.com/dev/reference/mlr_measures_selected_features.md)

## Super classes

[`mlr3::Measure`](https://mlr3.mlr-org.com/dev/reference/Measure.md) -\>
[`mlr3::MeasureRegr`](https://mlr3.mlr-org.com/dev/reference/MeasureRegr.md)
-\> `MeasureRQR`

## Methods

### Public methods

- [`MeasureRegrRQR$new()`](#method-MeasureRQR-new)

- [`MeasureRegrRQR$clone()`](#method-MeasureRQR-clone)

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

    MeasureRegrRQR$new(alpha = 0.5, pred_set_mean = TRUE)

#### Arguments

- `alpha`:

  `numeric(1)`  
  The quantile for which to compute the measure. Must be one of the
  quantiles that the Learner was trained on.

- `pred_set_mean`:

  `logical(1)`  
  If `TRUE`, the mean of the true values is calculated on the prediction
  set. If `FALSE`, the mean of the true values is calculated on the
  training set.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    MeasureRegrRQR$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
