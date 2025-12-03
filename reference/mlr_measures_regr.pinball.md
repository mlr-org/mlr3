# Average Pinball Loss

Measure to compare true observed response with predicted response in
regression tasks.

## Details

The pinball loss for quantile regression is defined as \$\$
\text{Average Pinball Loss} = \frac{1}{n} \sum\_{i=1}^{n} w\_{i}
\begin{cases} q \cdot (t_i - r_i) & \text{if } t_i \geq r_i \\ (1 - q)
\cdot (r_i - t_i) & \text{if } t_i \< r_i \end{cases} \$\$ where \\q\\
is the quantile and \\w_i\\ are normalized sample weights.

## Dictionary

This [Measure](https://mlr3.mlr-org.com/reference/Measure.md) can be
instantiated via the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr_measures](https://mlr3.mlr-org.com/reference/mlr_measures.md) or
with the associated sugar function
[`msr()`](https://mlr3.mlr-org.com/reference/mlr_sugar.md):

    mlr_measures$get("regr.pinball")
    msr("regr.pinball")

## Meta Information

- Task type: “regr”

- Range: \\(-\infty, \infty)\\

- Minimize: TRUE

- Average: macro

- Required Prediction: “quantiles”

- Required Packages: [mlr3](https://CRAN.R-project.org/package=mlr3)

## Parameters

|       |         |         |              |
|-------|---------|---------|--------------|
| Id    | Type    | Default | Range        |
| alpha | numeric | \-      | \\\[0, 1\]\\ |

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
[`mlr_measures_aic`](https://mlr3.mlr-org.com/reference/mlr_measures_aic.md),
[`mlr_measures_bic`](https://mlr3.mlr-org.com/reference/mlr_measures_bic.md),
[`mlr_measures_classif.costs`](https://mlr3.mlr-org.com/reference/mlr_measures_classif.costs.md),
[`mlr_measures_debug_classif`](https://mlr3.mlr-org.com/reference/mlr_measures_debug_classif.md),
[`mlr_measures_elapsed_time`](https://mlr3.mlr-org.com/reference/mlr_measures_elapsed_time.md),
[`mlr_measures_internal_valid_score`](https://mlr3.mlr-org.com/reference/mlr_measures_internal_valid_score.md),
[`mlr_measures_oob_error`](https://mlr3.mlr-org.com/reference/mlr_measures_oob_error.md),
[`mlr_measures_regr.rqr`](https://mlr3.mlr-org.com/reference/mlr_measures_regr.rqr.md),
[`mlr_measures_regr.rsq`](https://mlr3.mlr-org.com/reference/mlr_measures_regr.rsq.md),
[`mlr_measures_selected_features`](https://mlr3.mlr-org.com/reference/mlr_measures_selected_features.md)

## Super classes

[`mlr3::Measure`](https://mlr3.mlr-org.com/reference/Measure.md) -\>
[`mlr3::MeasureRegr`](https://mlr3.mlr-org.com/reference/MeasureRegr.md)
-\> `MeasureRegrPinball`

## Methods

### Public methods

- [`MeasureRegrPinball$new()`](#method-MeasureRegrPinball-new)

- [`MeasureRegrPinball$clone()`](#method-MeasureRegrPinball-clone)

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

    MeasureRegrPinball$new(alpha = 0.5)

#### Arguments

- `alpha`:

  `numeric(1)`  
  The quantile to compute the pinball loss. Must be one of the quantiles
  that the Learner was trained on.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    MeasureRegrPinball$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
