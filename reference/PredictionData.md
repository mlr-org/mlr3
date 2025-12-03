# Convert to PredictionData

Objects of type `PredictionData` serve as a intermediate representation
for objects of type
[Prediction](https://mlr3.mlr-org.com/reference/Prediction.md). It is an
internal data structure, implemented to optimize runtime and solve some
issues emerging while serializing R6 objects. End-users typically do not
need to worry about the details, package developers are advised to
continue reading for some technical information.

Unlike most other [mlr3](https://CRAN.R-project.org/package=mlr3)
objects, `PredictionData` relies on the S3 class system. The following
operations must be supported to extend mlr3 for new task types:

- [`as_prediction_data()`](https://mlr3.mlr-org.com/reference/as_prediction_data.md)
  converts objects to class `PredictionData`, e.g. objects of type
  [Prediction](https://mlr3.mlr-org.com/reference/Prediction.md).

- [`as_prediction()`](https://mlr3.mlr-org.com/reference/as_prediction.md)
  converts objects to class
  [Prediction](https://mlr3.mlr-org.com/reference/Prediction.md), e.g.
  objects of type `PredictionData`.

- `check_prediction_data()` is called on the return value of the predict
  method of a [Learner](https://mlr3.mlr-org.com/reference/Learner.md)
  to perform assertions and type conversions. Returns an update object
  of class `PredictionData`.

- `is_missing_prediction_data()` is used for the fallback learner (see
  [Learner](https://mlr3.mlr-org.com/reference/Learner.md)) to impute
  missing predictions. Returns vector with row ids which need
  imputation.

## Usage

``` r
create_empty_prediction_data(task, learner)

check_prediction_data(pdata, ...)

is_missing_prediction_data(pdata, ...)

filter_prediction_data(pdata, row_ids, ...)

# S3 method for class 'PredictionDataClassif'
check_prediction_data(pdata, train_task, ...)

# S3 method for class 'PredictionDataClassif'
is_missing_prediction_data(pdata, ...)

# S3 method for class 'PredictionDataClassif'
c(..., keep_duplicates = TRUE)

# S3 method for class 'PredictionDataRegr'
check_prediction_data(pdata, ...)

# S3 method for class 'PredictionDataRegr'
is_missing_prediction_data(pdata, ...)

# S3 method for class 'PredictionDataRegr'
c(..., keep_duplicates = TRUE)
```

## Arguments

- task:

  ([Task](https://mlr3.mlr-org.com/reference/Task.md)).

- learner:

  ([Learner](https://mlr3.mlr-org.com/reference/Learner.md)).

- pdata:

  (PredictionData)  
  Named list inheriting from `"PredictionData"`.

- ...:

  (one or more PredictionData objects).

- row_ids:

  [`integer()`](https://rdrr.io/r/base/integer.html)  
  Row indices.

- train_task:

  ([Task](https://mlr3.mlr-org.com/reference/Task.md))  
  Task used for training the learner.

- keep_duplicates:

  (`logical(1)`) If `TRUE`, the combined PredictionData object is
  filtered for duplicated row ids (starting from last).
