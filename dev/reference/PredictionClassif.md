# Prediction Object for Classification

This object wraps the predictions returned by a learner of class
[LearnerClassif](https://mlr3.mlr-org.com/dev/reference/LearnerClassif.md),
i.e. the predicted response and class probabilities.

If the response is not provided during construction, but class
probabilities are, the response is calculated from the probabilities:
the class label with the highest probability is chosen. In case of ties,
a label is selected randomly.

## Note

If this object is constructed manually, make sure that the factor levels
for `truth` have the same levels as the task, in the same order. In case
of binary classification tasks, the positive class label must be the
first level.

## Thresholding

If probabilities are stored, it is possible to change the threshold
which determines the predicted class label. Usually, the label of the
class with the highest predicted probability is selected. For binary
classification problems, such an threshold defaults to 0.5. For
cost-sensitive or imbalanced classification problems, manually adjusting
the threshold can increase the predictive performance.

- For binary problems only a single threshold value can be set. If the
  probability exceeds the threshold, the positive class is predicted. If
  the probability equals the threshold, the label is selected randomly.

- For binary and multi-class problems, a named numeric vector of
  thresholds can be set. The length and names must correspond to the
  number of classes and class names, respectively. To determine the
  class label, the probabilities are divided by the threshold. This
  results in a ratio \> 1 if the probability exceeds the threshold, and
  a ratio \< 1 otherwise. Note that it is possible that either none or
  multiple ratios are greater than 1 at the same time. Anyway, the class
  label with maximum ratio is selected. In case of ties in the ratio,
  one of the tied class labels is selected randomly.

  Note that there are the following edge cases for threshold equal to
  `0` which are handled specially:

  1.  With threshold 0 the resulting ratio gets `Inf` and thus gets
      always selected. If there are multiple ratios with value `Inf`,
      one is selected according to `ties_method` (randomly per default).

  2.  If additionally the predicted probability is also 0, the ratio
      `0/0` results in `NaN` values. These are simply replaced by `0`
      and thus will never get selected.

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
[`Prediction`](https://mlr3.mlr-org.com/dev/reference/Prediction.md),
[`PredictionRegr`](https://mlr3.mlr-org.com/dev/reference/PredictionRegr.md)

## Super class

[`mlr3::Prediction`](https://mlr3.mlr-org.com/dev/reference/Prediction.md)
-\> `PredictionClassif`

## Active bindings

- `response`:

  ([`factor()`](https://rdrr.io/r/base/factor.html))  
  Access to the stored predicted class labels.

- `prob`:

  ([`matrix()`](https://rdrr.io/r/base/matrix.html))  
  Access to the stored probabilities.

- `confusion`:

  ([`matrix()`](https://rdrr.io/r/base/matrix.html))  
  Confusion matrix, as resulting from the comparison of truth and
  response. Truth is in columns, predicted response is in rows.

## Methods

### Public methods

- [`PredictionClassif$new()`](#method-PredictionClassif-new)

- [`PredictionClassif$set_threshold()`](#method-PredictionClassif-set_threshold)

- [`PredictionClassif$clone()`](#method-PredictionClassif-clone)

Inherited methods

- [`mlr3::Prediction$filter()`](https://mlr3.mlr-org.com/dev/reference/Prediction.html#method-filter)
- [`mlr3::Prediction$format()`](https://mlr3.mlr-org.com/dev/reference/Prediction.html#method-format)
- [`mlr3::Prediction$help()`](https://mlr3.mlr-org.com/dev/reference/Prediction.html#method-help)
- [`mlr3::Prediction$obs_loss()`](https://mlr3.mlr-org.com/dev/reference/Prediction.html#method-obs_loss)
- [`mlr3::Prediction$print()`](https://mlr3.mlr-org.com/dev/reference/Prediction.html#method-print)
- [`mlr3::Prediction$score()`](https://mlr3.mlr-org.com/dev/reference/Prediction.html#method-score)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    PredictionClassif$new(
      task = NULL,
      row_ids = task$row_ids,
      truth = task$truth(),
      response = NULL,
      prob = NULL,
      weights = NULL,
      check = TRUE,
      extra = NULL
    )

#### Arguments

- `task`:

  ([TaskClassif](https://mlr3.mlr-org.com/dev/reference/TaskClassif.md))  
  Task, used to extract defaults for `row_ids` and `truth`.

- `row_ids`:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  Row ids of the predicted observations, i.e. the row ids of the test
  set.

- `truth`:

  ([`factor()`](https://rdrr.io/r/base/factor.html))  
  True (observed) labels. See the note on manual construction.

- `response`:

  ([`character()`](https://rdrr.io/r/base/character.html) \|
  [`factor()`](https://rdrr.io/r/base/factor.html))  
  Vector of predicted class labels. One element for each observation in
  the test set. Character vectors are automatically converted to
  factors. See the note on manual construction.

- `prob`:

  ([`matrix()`](https://rdrr.io/r/base/matrix.html))  
  Numeric matrix of posterior class probabilities with one column for
  each class and one row for each observation in the test set. Columns
  must be named with class labels, row names are automatically removed.
  If `prob` is provided, but `response` is not, the class labels are
  calculated from the probabilities using
  [`max.col()`](https://rdrr.io/r/base/maxCol.html) with `ties.method`
  set to `"random"`.

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

### Method `set_threshold()`

Sets the prediction response based on the provided threshold. See the
section on thresholding for more information.

#### Usage

    PredictionClassif$set_threshold(threshold, ties_method = "random")

#### Arguments

- `threshold`:

  ([`numeric()`](https://rdrr.io/r/base/numeric.html)).

- `ties_method`:

  (`character(1)`)  
  One of `"random"`, `"first"` or `"last"` (c.f.
  [`max.col()`](https://rdrr.io/r/base/maxCol.html)) to determine how to
  deal with tied probabilities.

#### Returns

Returns the object itself, but modified **by reference**. You need to
explicitly `$clone()` the object beforehand if you want to keep the
object in its previous state.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    PredictionClassif$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
task = tsk("penguins")
learner = lrn("classif.rpart", predict_type = "prob")
learner$train(task)
p = learner$predict(task)
p$predict_types
#> [1] "response" "prob"    
head(as.data.table(p))
#>    row_ids  truth response prob.Adelie prob.Chinstrap prob.Gentoo
#>      <int> <fctr>   <fctr>       <num>          <num>       <num>
#> 1:       1 Adelie   Adelie   0.9668874     0.03311258           0
#> 2:       2 Adelie   Adelie   0.9668874     0.03311258           0
#> 3:       3 Adelie   Adelie   0.9668874     0.03311258           0
#> 4:       4 Adelie   Adelie   0.9668874     0.03311258           0
#> 5:       5 Adelie   Adelie   0.9668874     0.03311258           0
#> 6:       6 Adelie   Adelie   0.9668874     0.03311258           0

# confusion matrix
p$confusion
#>            truth
#> response    Adelie Chinstrap Gentoo
#>   Adelie       146         5      0
#>   Chinstrap      6        63      1
#>   Gentoo         0         0    123

# change threshold
th = c(0.05, 0.9, 0.05)
names(th) = task$class_names

# new predictions
p$set_threshold(th)$response
#>   [1] Adelie Adelie Adelie Adelie Adelie Adelie Adelie Adelie Adelie Adelie
#>  [11] Adelie Adelie Adelie Adelie Adelie Adelie Adelie Adelie Adelie Adelie
#>  [21] Adelie Adelie Adelie Adelie Adelie Adelie Adelie Adelie Adelie Adelie
#>  [31] Adelie Adelie Adelie Adelie Adelie Adelie Adelie Adelie Adelie Adelie
#>  [41] Adelie Adelie Adelie Adelie Adelie Adelie Adelie Adelie Adelie Adelie
#>  [51] Adelie Adelie Adelie Adelie Adelie Adelie Adelie Adelie Adelie Adelie
#>  [61] Adelie Adelie Adelie Adelie Adelie Adelie Adelie Adelie Adelie Adelie
#>  [71] Adelie Adelie Adelie Adelie Adelie Adelie Adelie Adelie Adelie Adelie
#>  [81] Adelie Adelie Adelie Adelie Adelie Adelie Adelie Adelie Adelie Adelie
#>  [91] Adelie Adelie Adelie Adelie Adelie Adelie Adelie Adelie Adelie Adelie
#> [101] Adelie Adelie Adelie Adelie Adelie Adelie Adelie Adelie Adelie Adelie
#> [111] Adelie Adelie Adelie Adelie Adelie Adelie Adelie Adelie Adelie Adelie
#> [121] Adelie Adelie Adelie Adelie Adelie Adelie Adelie Adelie Adelie Adelie
#> [131] Adelie Adelie Adelie Adelie Adelie Adelie Adelie Adelie Adelie Adelie
#> [141] Adelie Adelie Adelie Adelie Adelie Adelie Adelie Adelie Adelie Adelie
#> [151] Adelie Adelie Gentoo Gentoo Gentoo Gentoo Gentoo Gentoo Gentoo Gentoo
#> [161] Gentoo Gentoo Gentoo Gentoo Gentoo Gentoo Gentoo Gentoo Gentoo Gentoo
#> [171] Gentoo Gentoo Gentoo Gentoo Gentoo Gentoo Gentoo Gentoo Gentoo Gentoo
#> [181] Gentoo Gentoo Gentoo Gentoo Gentoo Gentoo Gentoo Gentoo Gentoo Gentoo
#> [191] Gentoo Gentoo Gentoo Gentoo Gentoo Gentoo Gentoo Gentoo Gentoo Gentoo
#> [201] Gentoo Gentoo Gentoo Gentoo Gentoo Gentoo Gentoo Gentoo Gentoo Gentoo
#> [211] Gentoo Gentoo Gentoo Gentoo Gentoo Gentoo Gentoo Gentoo Gentoo Gentoo
#> [221] Gentoo Gentoo Gentoo Gentoo Gentoo Gentoo Gentoo Gentoo Gentoo Gentoo
#> [231] Gentoo Gentoo Gentoo Gentoo Gentoo Gentoo Gentoo Gentoo Gentoo Gentoo
#> [241] Gentoo Gentoo Gentoo Gentoo Gentoo Gentoo Gentoo Gentoo Gentoo Gentoo
#> [251] Adelie Gentoo Gentoo Gentoo Gentoo Gentoo Gentoo Gentoo Gentoo Gentoo
#> [261] Gentoo Gentoo Gentoo Gentoo Gentoo Gentoo Gentoo Gentoo Gentoo Gentoo
#> [271] Gentoo Gentoo Gentoo Gentoo Gentoo Gentoo Adelie Adelie Adelie Adelie
#> [281] Adelie Adelie Adelie Adelie Adelie Adelie Adelie Adelie Adelie Adelie
#> [291] Adelie Adelie Adelie Adelie Adelie Adelie Adelie Adelie Adelie Adelie
#> [301] Adelie Adelie Adelie Adelie Adelie Adelie Adelie Adelie Adelie Adelie
#> [311] Adelie Adelie Adelie Adelie Adelie Adelie Adelie Adelie Adelie Adelie
#> [321] Adelie Adelie Adelie Adelie Adelie Adelie Adelie Adelie Adelie Adelie
#> [331] Adelie Adelie Adelie Adelie Adelie Adelie Adelie Adelie Adelie Adelie
#> [341] Adelie Adelie Adelie Adelie
#> Levels: Adelie Chinstrap Gentoo
p$score(measures = msr("classif.ce"))
#> classif.ce 
#>  0.2005814 
```
