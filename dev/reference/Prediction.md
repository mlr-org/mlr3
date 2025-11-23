# Abstract Prediction Object

This is the abstract base class for task objects like
[PredictionClassif](https://mlr3.mlr-org.com/dev/reference/PredictionClassif.md)
or
[PredictionRegr](https://mlr3.mlr-org.com/dev/reference/PredictionRegr.md).

Prediction objects store the following information:

1.  The row ids of the test set

2.  The corresponding true (observed) response.

3.  The corresponding predicted response.

4.  Additional predictions based on the class and `predict_type`. E.g.,
    the class probabilities for classification or the estimated standard
    error for regression.

Note that this object is usually constructed via a derived classes, e.g.
[PredictionClassif](https://mlr3.mlr-org.com/dev/reference/PredictionClassif.md)
or
[PredictionRegr](https://mlr3.mlr-org.com/dev/reference/PredictionRegr.md).

## S3 Methods

- `as.data.table(rr)`  
  Prediction -\>
  [`data.table::data.table()`](https://rdatatable.gitlab.io/data.table/reference/data.table.html)  
  Converts the data to a
  [`data.table::data.table()`](https://rdatatable.gitlab.io/data.table/reference/data.table.html).

- `c(..., keep_duplicates = TRUE)`  
  (Prediction, Prediction, ...) -\> Prediction  
  Combines multiple `Prediction`s to a single `Prediction`. If
  `keep_duplicates` is `FALSE` and there are duplicated row ids, the
  data of the former passed objects get overwritten by the data of the
  later passed objects.

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
[`PredictionClassif`](https://mlr3.mlr-org.com/dev/reference/PredictionClassif.md),
[`PredictionRegr`](https://mlr3.mlr-org.com/dev/reference/PredictionRegr.md)

## Public fields

- `data`:

  (named [`list()`](https://rdrr.io/r/base/list.html))  
  Internal data structure.

- `task_type`:

  (`character(1)`)  
  Required type of the
  [Task](https://mlr3.mlr-org.com/dev/reference/Task.md).

- `task_properties`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Required properties of the
  [Task](https://mlr3.mlr-org.com/dev/reference/Task.md).

- `predict_types`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Set of predict types this object stores.

- `man`:

  (`character(1)`)  
  String in the format `[pkg]::[topic]` pointing to a manual page for
  this object. Defaults to `NA`, but can be set by child classes.

## Active bindings

- `row_ids`:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  Vector of row ids for which predictions are stored.

- `truth`:

  (any)  
  True (observed) outcome.

- `missing`:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  Returns `row_ids` for which the predictions are missing or incomplete.

- `weights`:

  ([`numeric()`](https://rdrr.io/r/base/numeric.html))  
  Vector of measure weights, obtained from the `weights_measure` column
  of the [Task](https://mlr3.mlr-org.com/dev/reference/Task.md) if
  present. This is `NULL` if no weights are present.

- `extra`:

  ([`list()`](https://rdrr.io/r/base/list.html))  
  List of extra data stored in the prediction object.

## Methods

### Public methods

- [`Prediction$format()`](#method-Prediction-format)

- [`Prediction$print()`](#method-Prediction-print)

- [`Prediction$help()`](#method-Prediction-help)

- [`Prediction$score()`](#method-Prediction-score)

- [`Prediction$obs_loss()`](#method-Prediction-obs_loss)

- [`Prediction$filter()`](#method-Prediction-filter)

- [`Prediction$clone()`](#method-Prediction-clone)

------------------------------------------------------------------------

### Method [`format()`](https://rdrr.io/r/base/format.html)

Helper for print outputs.

#### Usage

    Prediction$format(...)

#### Arguments

- `...`:

  (ignored).

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Printer.

#### Usage

    Prediction$print(...)

#### Arguments

- `...`:

  (ignored).

------------------------------------------------------------------------

### Method [`help()`](https://rdrr.io/r/utils/help.html)

Opens the corresponding help page referenced by field `$man`.

#### Usage

    Prediction$help()

------------------------------------------------------------------------

### Method `score()`

Calculates the performance for all provided measures
[Task](https://mlr3.mlr-org.com/dev/reference/Task.md) and
[Learner](https://mlr3.mlr-org.com/dev/reference/Learner.md) may be
`NULL` for most measures, but some measures need to extract information
from these objects. Note that the `predict_sets` of the `measures` are
ignored by this method, instead all predictions are used.

#### Usage

    Prediction$score(
      measures = NULL,
      task = NULL,
      learner = NULL,
      train_set = NULL
    )

#### Arguments

- `measures`:

  ([Measure](https://mlr3.mlr-org.com/dev/reference/Measure.md) \| list
  of [Measure](https://mlr3.mlr-org.com/dev/reference/Measure.md))  
  Measure(s) to calculate.

- `task`:

  ([Task](https://mlr3.mlr-org.com/dev/reference/Task.md)).

- `learner`:

  ([Learner](https://mlr3.mlr-org.com/dev/reference/Learner.md)).

- `train_set`:

  ([`integer()`](https://rdrr.io/r/base/integer.html)).

#### Returns

Prediction.

------------------------------------------------------------------------

### Method `obs_loss()`

Calculates the observation-wise loss via the
[Measure](https://mlr3.mlr-org.com/dev/reference/Measure.md)'s
`obs_loss` method. Returns a
[`data.table()`](https://rdatatable.gitlab.io/data.table/reference/data.table.html)
with the columns of the matching Prediction object plus one additional
numeric column for each measure, named with the respective measure id.
If there is no observation-wise loss function for the measure, the
column is filled with `NA_real_` values. Note that some measures such as
RMSE, do have an `$obs_loss`, but they require an additional
transformation after aggregation, in this example taking the
square-root.

#### Usage

    Prediction$obs_loss(measures = NULL)

#### Arguments

- `measures`:

  ([Measure](https://mlr3.mlr-org.com/dev/reference/Measure.md) \| list
  of [Measure](https://mlr3.mlr-org.com/dev/reference/Measure.md))  
  Measure(s) to calculate.

------------------------------------------------------------------------

### Method [`filter()`](https://rdrr.io/r/stats/filter.html)

Filters the Prediction, keeping only predictions for the provided
row_ids. This changes the object in-place, you need to create a clone to
preserve the original Prediction.

#### Usage

    Prediction$filter(row_ids)

#### Arguments

- `row_ids`:

  [`integer()`](https://rdrr.io/r/base/integer.html)  
  Row indices.

#### Returns

`self`, modified.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Prediction$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
