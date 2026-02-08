# Container for Results of `resample()`

This is the result container object returned by
[`resample()`](https://mlr3.mlr-org.com/dev/reference/resample.md).

Note that all stored objects are accessed by reference. Do not modify
any object without cloning it first.

ResampleResults can be visualized via
[mlr3viz](https://CRAN.R-project.org/package=mlr3viz)'s `autoplot()`
function.

## S3 Methods

- `as.data.table(rr, reassemble_learners = TRUE, convert_predictions = TRUE, predict_sets = "test")`  
  ResampleResult -\>
  [`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html)  
  Returns a tabular view of the internal data.

- `c(...)`  
  (ResampleResult, ...) -\>
  [BenchmarkResult](https://mlr3.mlr-org.com/dev/reference/BenchmarkResult.md)  
  Combines multiple objects convertible to
  [BenchmarkResult](https://mlr3.mlr-org.com/dev/reference/BenchmarkResult.md)
  into a new
  [BenchmarkResult](https://mlr3.mlr-org.com/dev/reference/BenchmarkResult.md).

## See also

- [`as_benchmark_result()`](https://mlr3.mlr-org.com/dev/reference/as_benchmark_result.md)
  to convert to a
  [BenchmarkResult](https://mlr3.mlr-org.com/dev/reference/BenchmarkResult.md).

- Chapter in the [mlr3book](https://mlr3book.mlr-org.com/):
  <https://mlr3book.mlr-org.com/chapters/chapter3/evaluation_and_benchmarking.html#sec-resampling>

- Package [mlr3viz](https://CRAN.R-project.org/package=mlr3viz) for some
  generic visualizations.

Other resample:
[`resample()`](https://mlr3.mlr-org.com/dev/reference/resample.md)

## Active bindings

- `task_type`:

  (`character(1)`)  
  Task type of objects in the `ResampleResult`, e.g. `"classif"` or
  `"regr"`. This is `NA` for empty ResampleResults.

- `uhash`:

  (`character(1)`)  
  Unique hash for this object.

- `iters`:

  (`integer(1)`)  
  Number of resampling iterations stored in the `ResampleResult`.

- `task`:

  ([Task](https://mlr3.mlr-org.com/dev/reference/Task.md))  
  The task
  [`resample()`](https://mlr3.mlr-org.com/dev/reference/resample.md)
  operated on.

- `learner`:

  ([Learner](https://mlr3.mlr-org.com/dev/reference/Learner.md))  
  Learner prototype
  [`resample()`](https://mlr3.mlr-org.com/dev/reference/resample.md)
  operated on. For a list of **trained** learners, see methods
  `$learners()`.

- `resampling`:

  ([Resampling](https://mlr3.mlr-org.com/dev/reference/Resampling.md))  
  Instantiated
  [Resampling](https://mlr3.mlr-org.com/dev/reference/Resampling.md)
  object which stores the splits into training and test.

- `learners`:

  (list of
  [Learner](https://mlr3.mlr-org.com/dev/reference/Learner.md))  
  List of trained learners, sorted by resampling iteration.

- `data_extra`:

  (list())  
  Additional data stored in the ResampleResult.

- `warnings`:

  ([`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html))  
  A table with all warning messages. Column names are `"iteration"` and
  `"msg"`. Note that there can be multiple rows per resampling iteration
  if multiple warnings have been recorded.

- `errors`:

  ([`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html))  
  A table with all error messages. Column names are `"iteration"` and
  `"msg"`. Note that there can be multiple rows per resampling iteration
  if multiple errors have been recorded.

## Methods

### Public methods

- [`ResampleResult$new()`](#method-ResampleResult-new)

- [`ResampleResult$format()`](#method-ResampleResult-format)

- [`ResampleResult$print()`](#method-ResampleResult-print)

- [`ResampleResult$help()`](#method-ResampleResult-help)

- [`ResampleResult$prediction()`](#method-ResampleResult-prediction)

- [`ResampleResult$predictions()`](#method-ResampleResult-predictions)

- [`ResampleResult$score()`](#method-ResampleResult-score)

- [`ResampleResult$obs_loss()`](#method-ResampleResult-obs_loss)

- [`ResampleResult$aggregate()`](#method-ResampleResult-aggregate)

- [`ResampleResult$filter()`](#method-ResampleResult-filter)

- [`ResampleResult$discard()`](#method-ResampleResult-discard)

- [`ResampleResult$marshal()`](#method-ResampleResult-marshal)

- [`ResampleResult$unmarshal()`](#method-ResampleResult-unmarshal)

- [`ResampleResult$set_threshold()`](#method-ResampleResult-set_threshold)

- [`ResampleResult$clone()`](#method-ResampleResult-clone)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class. An alternative
construction method is provided by
[`as_resample_result()`](https://mlr3.mlr-org.com/dev/reference/as_resample_result.md).

#### Usage

    ResampleResult$new(data = ResultData$new(), view = NULL)

#### Arguments

- `data`:

  ([ResultData](https://mlr3.mlr-org.com/dev/reference/ResultData.md) \|
  [`data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html))  
  An object of type
  [ResultData](https://mlr3.mlr-org.com/dev/reference/ResultData.md),
  either extracted from another ResampleResult, another
  [BenchmarkResult](https://mlr3.mlr-org.com/dev/reference/BenchmarkResult.md),
  or manually constructed with
  [`as_result_data()`](https://mlr3.mlr-org.com/dev/reference/as_result_data.md).

- `view`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Single `uhash` of the
  [ResultData](https://mlr3.mlr-org.com/dev/reference/ResultData.md) to
  operate on. Used internally for optimizations.

------------------------------------------------------------------------

### Method [`format()`](https://rdrr.io/r/base/format.html)

Helper for print outputs.

#### Usage

    ResampleResult$format(...)

#### Arguments

- `...`:

  (ignored).

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Printer.

#### Usage

    ResampleResult$print(...)

#### Arguments

- `...`:

  (ignored).

------------------------------------------------------------------------

### Method [`help()`](https://rdrr.io/r/utils/help.html)

Opens the corresponding help page referenced by field `$man`.

#### Usage

    ResampleResult$help()

------------------------------------------------------------------------

### Method `prediction()`

Combined
[Prediction](https://mlr3.mlr-org.com/dev/reference/Prediction.md) of
all individual resampling iterations, and all provided predict sets.
Note that, per default, most performance measures do not operate on this
object directly, but instead on the prediction objects from the
resampling iterations separately, and then combine the performance
scores with the aggregate function of the respective
[Measure](https://mlr3.mlr-org.com/dev/reference/Measure.md) (macro
averaging).

If you calculate the performance on this prediction object directly,
this is called micro averaging.

#### Usage

    ResampleResult$prediction(predict_sets = "test")

#### Arguments

- `predict_sets`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Subset of `{"train", "test"}`.

#### Returns

[Prediction](https://mlr3.mlr-org.com/dev/reference/Prediction.md) or
empty [`list()`](https://rdrr.io/r/base/list.html) if no predictions are
available.

#### Examples

    rr$prediction()

------------------------------------------------------------------------

### Method `predictions()`

List of prediction objects, sorted by resampling iteration. If multiple
sets are given, these are combined to a single one for each iteration.

If you evaluate the performance on all of the returned prediction
objects and then average them, this is called macro averaging. For micro
averaging, operate on the combined prediction object as returned by
`$prediction()`.

#### Usage

    ResampleResult$predictions(predict_sets = "test")

#### Arguments

- `predict_sets`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Subset of `{"train", "test", "internal_valid"}`.

#### Returns

List of
[Prediction](https://mlr3.mlr-org.com/dev/reference/Prediction.md)
objects, one per element in `predict_sets`. Or list of empty
[`list()`](https://rdrr.io/r/base/list.html)s if no predictions are
available.

#### Examples

    rr$predictions()

------------------------------------------------------------------------

### Method `score()`

Returns a table with one row for each resampling iteration, including
all involved objects:
[Task](https://mlr3.mlr-org.com/dev/reference/Task.md),
[Learner](https://mlr3.mlr-org.com/dev/reference/Learner.md),
[Resampling](https://mlr3.mlr-org.com/dev/reference/Resampling.md),
iteration number (`integer(1)`), and (if enabled) one
[Prediction](https://mlr3.mlr-org.com/dev/reference/Prediction.md) for
each predict set of the
[Learner](https://mlr3.mlr-org.com/dev/reference/Learner.md).
Additionally, a column with the individual (per resampling iteration)
performance is added for each
[Measure](https://mlr3.mlr-org.com/dev/reference/Measure.md) in
`measures`, named with the id of the respective measure id. If
`measures` is `NULL`, `measures` defaults to the return value of
[`default_measures()`](https://mlr3.mlr-org.com/dev/reference/default_measures.md).

#### Usage

    ResampleResult$score(
      measures = NULL,
      ids = TRUE,
      conditions = FALSE,
      predictions = TRUE
    )

#### Arguments

- `measures`:

  ([Measure](https://mlr3.mlr-org.com/dev/reference/Measure.md) \| list
  of [Measure](https://mlr3.mlr-org.com/dev/reference/Measure.md))  
  Measure(s) to calculate.

- `ids`:

  (`logical(1)`)  
  If `ids` is `TRUE`, extra columns with the ids of objects
  (`"task_id"`, `"learner_id"`, `"resampling_id"`) are added to the
  returned table. These allow to subset more conveniently.

- `conditions`:

  (`logical(1)`)  
  Adds condition messages (`"warnings"`, `"errors"`) as extra list
  columns of character vectors to the returned table

- `predictions`:

  (`logical(1)`)  
  Additionally return prediction objects, one column for each
  `predict_set` of the learner. Columns are named `"prediction_train"`,
  `"prediction_test"` and `"prediction_internal_valid"`, if present.

#### Returns

[`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html).

#### Examples

    rr$score(msr("classif.acc"))

------------------------------------------------------------------------

### Method `obs_loss()`

Calculates the observation-wise loss via the
[Measure](https://mlr3.mlr-org.com/dev/reference/Measure.md)'s
`obs_loss` method. Returns a
[`data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html)
with an `iteration` column plus one numeric column for each measure,
named with the respective measure id. If there is no observation-wise
loss function for the measure, the column is filled with `NA_real_`
values. Note that some measures such as RMSE, do have an `$obs_loss`,
but they require an additional transformation after aggregation, in this
example taking the square-root.

#### Usage

    ResampleResult$obs_loss(measures = NULL, predict_sets = "test")

#### Arguments

- `measures`:

  ([Measure](https://mlr3.mlr-org.com/dev/reference/Measure.md) \| list
  of [Measure](https://mlr3.mlr-org.com/dev/reference/Measure.md))  
  Measure(s) to calculate.

- `predict_sets`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  The predict sets.

#### Examples

    rr$obs_loss(msr("classif.acc"))

------------------------------------------------------------------------

### Method [`aggregate()`](https://rdrr.io/r/stats/aggregate.html)

Calculates and aggregates performance values for all provided measures,
according to the respective aggregation function in
[Measure](https://mlr3.mlr-org.com/dev/reference/Measure.md). If
`measures` is `NULL`, `measures` defaults to the return value of
[`default_measures()`](https://mlr3.mlr-org.com/dev/reference/default_measures.md).

#### Usage

    ResampleResult$aggregate(measures = NULL)

#### Arguments

- `measures`:

  ([Measure](https://mlr3.mlr-org.com/dev/reference/Measure.md) \| list
  of [Measure](https://mlr3.mlr-org.com/dev/reference/Measure.md))  
  Measure(s) to calculate.

#### Returns

Named [`numeric()`](https://rdrr.io/r/base/numeric.html).

#### Examples

    rr$aggregate(msr("classif.acc"))

------------------------------------------------------------------------

### Method [`filter()`](https://rdrr.io/r/stats/filter.html)

Subsets the ResampleResult, reducing it to only keep the iterations
specified in `iters`.

#### Usage

    ResampleResult$filter(iters)

#### Arguments

- `iters`:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  Resampling iterations to keep.

#### Returns

Returns the object itself, but modified **by reference**. You need to
explicitly `$clone()` the object beforehand if you want to keeps the
object in its previous state.

#### Examples

    rr$filter(1L)

------------------------------------------------------------------------

### Method `discard()`

Shrinks the ResampleResult by discarding parts of the internally stored
data. Note that certain operations might stop work, e.g. extracting
importance values from learners or calculating measures requiring the
task's data.

#### Usage

    ResampleResult$discard(backends = FALSE, models = FALSE)

#### Arguments

- `backends`:

  (`logical(1)`)  
  If `TRUE`, the
  [DataBackend](https://mlr3.mlr-org.com/dev/reference/DataBackend.md)
  is removed from all stored
  [Task](https://mlr3.mlr-org.com/dev/reference/Task.md)s.

- `models`:

  (`logical(1)`)  
  If `TRUE`, the stored model is removed from all
  [Learner](https://mlr3.mlr-org.com/dev/reference/Learner.md)s.

#### Returns

Returns the object itself, but modified **by reference**. You need to
explicitly `$clone()` the object beforehand if you want to keeps the
object in its previous state.

------------------------------------------------------------------------

### Method `marshal()`

Marshals all stored models.

#### Usage

    ResampleResult$marshal(...)

#### Arguments

- `...`:

  (any)  
  Additional arguments passed to
  [`marshal_model()`](https://mlr3.mlr-org.com/dev/reference/marshaling.md).

#### Examples

    rr$marshal()

------------------------------------------------------------------------

### Method `unmarshal()`

Unmarshals all stored models.

#### Usage

    ResampleResult$unmarshal(...)

#### Arguments

- `...`:

  (any)  
  Additional arguments passed to
  [`unmarshal_model()`](https://mlr3.mlr-org.com/dev/reference/marshaling.md).

#### Examples

    rr$unmarshal()

------------------------------------------------------------------------

### Method `set_threshold()`

Sets the threshold for the response prediction of classification
learners, given they have output a probability prediction for a binary
classification task. This modifies the object in-place.

#### Usage

    ResampleResult$set_threshold(threshold, ties_method = "random")

#### Arguments

- `threshold`:

  (`numeric(1)`)  
  Threshold value.

- `ties_method`:

  (`character(1)`)  
  Method to handle ties in probabilities when selecting a class label.
  Must be one of `"random"`, `"first"` or `"last"` (corresponding to the
  same options in [`max.col()`](https://rdrr.io/r/base/maxCol.html)).

  - `"random"`: Randomly select one of the tied class labels (default).

  - `"first"`: Select the first class label among tied values.

  - `"last"`: Select the last class label among tied values.

#### Examples

    learner = lrn("classif.rpart", predict_type = "prob")
    rr = resample(tsk("sonar"), learner, rsmp("cv", folds = 3))
    rr$set_threshold(0.6)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ResampleResult$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
task = tsk("penguins")
learner = lrn("classif.rpart")
resampling = rsmp("cv", folds = 3)
rr = resample(task, learner, resampling)
print(rr)
#> 
#> ── <ResampleResult> with 3 resampling iterations ───────────────────────────────
#>   task_id    learner_id resampling_id iteration     prediction_test warnings
#>  penguins classif.rpart            cv         1 <PredictionClassif>        0
#>  penguins classif.rpart            cv         2 <PredictionClassif>        0
#>  penguins classif.rpart            cv         3 <PredictionClassif>        0
#>  errors
#>       0
#>       0
#>       0

# combined predictions and predictions for each fold separately
rr$prediction()
#> 
#> ── <PredictionClassif> for 344 observations: ───────────────────────────────────
#>  row_ids     truth  response
#>        1    Adelie    Adelie
#>        2    Adelie    Adelie
#>        3    Adelie    Adelie
#>      ---       ---       ---
#>      339 Chinstrap Chinstrap
#>      340 Chinstrap Chinstrap
#>      341 Chinstrap    Adelie
rr$predictions()
#> [[1]]
#> 
#> ── <PredictionClassif> for 115 observations: ───────────────────────────────────
#>  row_ids     truth  response
#>        1    Adelie    Adelie
#>        2    Adelie    Adelie
#>        3    Adelie    Adelie
#>      ---       ---       ---
#>      337 Chinstrap Chinstrap
#>      338 Chinstrap Chinstrap
#>      343 Chinstrap    Gentoo
#> 
#> [[2]]
#> 
#> ── <PredictionClassif> for 115 observations: ───────────────────────────────────
#>  row_ids     truth  response
#>        4    Adelie    Adelie
#>        8    Adelie    Adelie
#>        9    Adelie    Adelie
#>      ---       ---       ---
#>      336 Chinstrap Chinstrap
#>      342 Chinstrap Chinstrap
#>      344 Chinstrap Chinstrap
#> 
#> [[3]]
#> 
#> ── <PredictionClassif> for 114 observations: ───────────────────────────────────
#>  row_ids     truth  response
#>        6    Adelie    Adelie
#>       13    Adelie    Adelie
#>       18    Adelie    Adelie
#>      ---       ---       ---
#>      339 Chinstrap Chinstrap
#>      340 Chinstrap Chinstrap
#>      341 Chinstrap    Adelie
#> 

# folds scored separately, then aggregated (macro)
rr$aggregate(msr("classif.acc"))
#> classif.acc 
#>   0.9390033 

# predictions first combined, then scored (micro)
rr$prediction()$score(msr("classif.acc"))
#> classif.acc 
#>   0.9389535 

# check for warnings and errors
rr$warnings
#> Empty data.table (0 rows and 2 cols): iteration,msg
rr$errors
#> Empty data.table (0 rows and 2 cols): iteration,msg

## ------------------------------------------------
## Method `ResampleResult$prediction`
## ------------------------------------------------

rr$prediction()
#> 
#> ── <PredictionClassif> for 344 observations: ───────────────────────────────────
#>  row_ids     truth  response
#>        1    Adelie    Adelie
#>        2    Adelie    Adelie
#>        3    Adelie    Adelie
#>      ---       ---       ---
#>      339 Chinstrap Chinstrap
#>      340 Chinstrap Chinstrap
#>      341 Chinstrap    Adelie

## ------------------------------------------------
## Method `ResampleResult$predictions`
## ------------------------------------------------

rr$predictions()
#> [[1]]
#> 
#> ── <PredictionClassif> for 115 observations: ───────────────────────────────────
#>  row_ids     truth  response
#>        1    Adelie    Adelie
#>        2    Adelie    Adelie
#>        3    Adelie    Adelie
#>      ---       ---       ---
#>      337 Chinstrap Chinstrap
#>      338 Chinstrap Chinstrap
#>      343 Chinstrap    Gentoo
#> 
#> [[2]]
#> 
#> ── <PredictionClassif> for 115 observations: ───────────────────────────────────
#>  row_ids     truth  response
#>        4    Adelie    Adelie
#>        8    Adelie    Adelie
#>        9    Adelie    Adelie
#>      ---       ---       ---
#>      336 Chinstrap Chinstrap
#>      342 Chinstrap Chinstrap
#>      344 Chinstrap Chinstrap
#> 
#> [[3]]
#> 
#> ── <PredictionClassif> for 114 observations: ───────────────────────────────────
#>  row_ids     truth  response
#>        6    Adelie    Adelie
#>       13    Adelie    Adelie
#>       18    Adelie    Adelie
#>      ---       ---       ---
#>      339 Chinstrap Chinstrap
#>      340 Chinstrap Chinstrap
#>      341 Chinstrap    Adelie
#> 

## ------------------------------------------------
## Method `ResampleResult$score`
## ------------------------------------------------

rr$score(msr("classif.acc"))
#>     task_id    learner_id resampling_id iteration classif.acc
#>      <char>        <char>        <char>     <int>       <num>
#> 1: penguins classif.rpart            cv         1   0.9391304
#> 2: penguins classif.rpart            cv         2   0.9217391
#> 3: penguins classif.rpart            cv         3   0.9561404
#> Hidden columns: task, learner, resampling, prediction_test

## ------------------------------------------------
## Method `ResampleResult$obs_loss`
## ------------------------------------------------

rr$obs_loss(msr("classif.acc"))
#>      iteration row_ids     truth  response classif.acc
#>          <int>   <int>    <fctr>    <fctr>       <int>
#>   1:         1       1    Adelie    Adelie           1
#>   2:         1       2    Adelie    Adelie           1
#>   3:         1       3    Adelie    Adelie           1
#>   4:         1       5    Adelie    Adelie           1
#>   5:         1       7    Adelie    Adelie           1
#>  ---                                                  
#> 340:         3     332 Chinstrap Chinstrap           1
#> 341:         3     334 Chinstrap Chinstrap           1
#> 342:         3     339 Chinstrap Chinstrap           1
#> 343:         3     340 Chinstrap Chinstrap           1
#> 344:         3     341 Chinstrap    Adelie           0

## ------------------------------------------------
## Method `ResampleResult$aggregate`
## ------------------------------------------------

rr$aggregate(msr("classif.acc"))
#> classif.acc 
#>   0.9390033 

## ------------------------------------------------
## Method `ResampleResult$filter`
## ------------------------------------------------

rr$filter(1L)

## ------------------------------------------------
## Method `ResampleResult$marshal`
## ------------------------------------------------

rr$marshal()

## ------------------------------------------------
## Method `ResampleResult$unmarshal`
## ------------------------------------------------

rr$unmarshal()

## ------------------------------------------------
## Method `ResampleResult$set_threshold`
## ------------------------------------------------

learner = lrn("classif.rpart", predict_type = "prob")
rr = resample(tsk("sonar"), learner, rsmp("cv", folds = 3))
rr$set_threshold(0.6)
#> Key: <uhash, iteration>
#>                                   uhash iteration      learner_state prediction
#>                                  <char>     <int>             <list>     <list>
#> 1: 74dcf7f4-ecad-4077-bc99-8a8f13394c8d         1 <learner_state[8]>  <list[1]>
#> 2: 74dcf7f4-ecad-4077-bc99-8a8f13394c8d         2 <learner_state[8]>  <list[1]>
#> 3: 74dcf7f4-ecad-4077-bc99-8a8f13394c8d         3 <learner_state[8]>  <list[1]>
#>        learner_hash        task_hash    learner_phash  resampling_hash
#>              <char>           <char>           <char>           <char>
#> 1: 667ca7804cbe810f 062d5c9e3bac138b a2121f0bf1b9c2f6 fb4fd3525746a553
#> 2: 667ca7804cbe810f 062d5c9e3bac138b a2121f0bf1b9c2f6 fb4fd3525746a553
#> 3: 667ca7804cbe810f 062d5c9e3bac138b a2121f0bf1b9c2f6 fb4fd3525746a553
```
