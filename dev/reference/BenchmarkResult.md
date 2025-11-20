# Container for Benchmarking Results

This is the result container object returned by
[`benchmark()`](https://mlr3.mlr-org.com/dev/reference/benchmark.md). A
BenchmarkResult consists of the data of multiple
[ResampleResult](https://mlr3.mlr-org.com/dev/reference/ResampleResult.md)s.
The contents of a `BenchmarkResult` and
[ResampleResult](https://mlr3.mlr-org.com/dev/reference/ResampleResult.md)
are almost identical and the stored
[ResampleResult](https://mlr3.mlr-org.com/dev/reference/ResampleResult.md)s
can be extracted via the `$resample_result(i)` method, where i is the
index of the performed resample experiment. This allows us to
investigate the extracted
[ResampleResult](https://mlr3.mlr-org.com/dev/reference/ResampleResult.md)
and individual resampling iterations, as well as the predictions and
models from each fold.

BenchmarkResults can be visualized via
[mlr3viz](https://CRAN.R-project.org/package=mlr3viz)'s `autoplot()`
function.

For statistical analysis of benchmark results and more advanced plots,
see [mlr3benchmark](https://CRAN.R-project.org/package=mlr3benchmark).

## Note

All stored objects are accessed by reference. Do not modify any
extracted object without cloning it first.

## S3 Methods

- `as.data.table(rr, ..., reassemble_learners = TRUE, convert_predictions = TRUE, predict_sets = "test", task_characteristics = FALSE)`  
  BenchmarkResult -\>
  [`data.table::data.table()`](https://rdatatable.gitlab.io/data.table/reference/data.table.html)  
  Returns a tabular view of the internal data.

- `c(...)`  
  (BenchmarkResult, ...) -\> BenchmarkResult  
  Combines multiple objects convertible to BenchmarkResult into a new
  BenchmarkResult.

## See also

- Chapter in the [mlr3book](https://mlr3book.mlr-org.com/):
  <https://mlr3book.mlr-org.com/chapters/chapter3/evaluation_and_benchmarking.html#sec-benchmarking>

- Package [mlr3viz](https://CRAN.R-project.org/package=mlr3viz) for some
  generic visualizations.

- [mlr3benchmark](https://CRAN.R-project.org/package=mlr3benchmark) for
  post-hoc analysis of benchmark results.

Other benchmark:
[`benchmark()`](https://mlr3.mlr-org.com/dev/reference/benchmark.md),
[`benchmark_grid()`](https://mlr3.mlr-org.com/dev/reference/benchmark_grid.md)

## Active bindings

- `task_type`:

  (`character(1)`)  
  Task type of objects in the `BenchmarkResult`. All stored objects
  ([Task](https://mlr3.mlr-org.com/dev/reference/Task.md),
  [Learner](https://mlr3.mlr-org.com/dev/reference/Learner.md),
  [Prediction](https://mlr3.mlr-org.com/dev/reference/Prediction.md)) in
  a single `BenchmarkResult` are required to have the same task type,
  e.g., `"classif"` or `"regr"`. This is `NA` for empty
  BenchmarkResults.

- `tasks`:

  ([`data.table::data.table()`](https://rdatatable.gitlab.io/data.table/reference/data.table.html))  
  Table of included
  [Task](https://mlr3.mlr-org.com/dev/reference/Task.md)s with three
  columns:

  - `"task_hash"` (`character(1)`),

  - `"task_id"` (`character(1)`), and

  - `"task"` ([Task](https://mlr3.mlr-org.com/dev/reference/Task.md)).

- `learners`:

  ([`data.table::data.table()`](https://rdatatable.gitlab.io/data.table/reference/data.table.html))  
  Table of included
  [Learner](https://mlr3.mlr-org.com/dev/reference/Learner.md)s with
  three columns:

  - `"learner_hash"` (`character(1)`),

  - `"learner_id"` (`character(1)`), and

  - `"learner"`
    ([Learner](https://mlr3.mlr-org.com/dev/reference/Learner.md)).

  Note that it is not feasible to access learned models via this field,
  as the training task would be ambiguous. For this reason the returned
  learner are reset before they are returned. Instead, select a row from
  the table returned by `$score()`.

- `resamplings`:

  ([`data.table::data.table()`](https://rdatatable.gitlab.io/data.table/reference/data.table.html))  
  Table of included
  [Resampling](https://mlr3.mlr-org.com/dev/reference/Resampling.md)s
  with three columns:

  - `"resampling_hash"` (`character(1)`),

  - `"resampling_id"` (`character(1)`), and

  - `"resampling"`
    ([Resampling](https://mlr3.mlr-org.com/dev/reference/Resampling.md)).

- `resample_results`:

  ([`data.table::data.table()`](https://rdatatable.gitlab.io/data.table/reference/data.table.html))  
  Returns a table with three columns:

  - `uhash` ([`character()`](https://rdrr.io/r/base/character.html)).

  - `resample_result`
    ([ResampleResult](https://mlr3.mlr-org.com/dev/reference/ResampleResult.md)).

- `n_resample_results`:

  (`integer(1)`)  
  Returns the total number of stored
  [ResampleResult](https://mlr3.mlr-org.com/dev/reference/ResampleResult.md)s.

- `uhashes`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Set of (unique) hashes of all included
  [ResampleResult](https://mlr3.mlr-org.com/dev/reference/ResampleResult.md)s.

- `uhash_table`:

  ([data.table::data.table](https://rdatatable.gitlab.io/data.table/reference/data.table.html))  
  Table with columns `uhash`, `learner_id`, `task_id` and
  `resampling_id`.

## Methods

### Public methods

- [`BenchmarkResult$new()`](#method-BenchmarkResult-new)

- [`BenchmarkResult$help()`](#method-BenchmarkResult-help)

- [`BenchmarkResult$format()`](#method-BenchmarkResult-format)

- [`BenchmarkResult$print()`](#method-BenchmarkResult-print)

- [`BenchmarkResult$combine()`](#method-BenchmarkResult-combine)

- [`BenchmarkResult$marshal()`](#method-BenchmarkResult-marshal)

- [`BenchmarkResult$unmarshal()`](#method-BenchmarkResult-unmarshal)

- [`BenchmarkResult$score()`](#method-BenchmarkResult-score)

- [`BenchmarkResult$obs_loss()`](#method-BenchmarkResult-obs_loss)

- [`BenchmarkResult$aggregate()`](#method-BenchmarkResult-aggregate)

- [`BenchmarkResult$filter()`](#method-BenchmarkResult-filter)

- [`BenchmarkResult$resample_result()`](#method-BenchmarkResult-resample_result)

- [`BenchmarkResult$discard()`](#method-BenchmarkResult-discard)

- [`BenchmarkResult$set_threshold()`](#method-BenchmarkResult-set_threshold)

- [`BenchmarkResult$clone()`](#method-BenchmarkResult-clone)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    BenchmarkResult$new(data = NULL)

#### Arguments

- `data`:

  (`ResultData`)  
  An object of type `ResultData`, either extracted from another
  [ResampleResult](https://mlr3.mlr-org.com/dev/reference/ResampleResult.md),
  another BenchmarkResult, or manually constructed with
  [`as_result_data()`](https://mlr3.mlr-org.com/dev/reference/as_result_data.md).

------------------------------------------------------------------------

### Method [`help()`](https://rdrr.io/r/utils/help.html)

Opens the help page for this object.

#### Usage

    BenchmarkResult$help()

------------------------------------------------------------------------

### Method [`format()`](https://rdrr.io/r/base/format.html)

Helper for print outputs.

#### Usage

    BenchmarkResult$format(...)

#### Arguments

- `...`:

  (ignored).

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Printer.

#### Usage

    BenchmarkResult$print()

------------------------------------------------------------------------

### Method `combine()`

Fuses a second BenchmarkResult into itself, mutating the BenchmarkResult
in-place. If the second BenchmarkResult `bmr` is `NULL`, simply returns
`self`. Note that you can alternatively use the combine function
[`c()`](https://rdrr.io/r/base/c.html) which calls this method
internally.

#### Usage

    BenchmarkResult$combine(bmr)

#### Arguments

- `bmr`:

  (BenchmarkResult)  
  A second BenchmarkResult object.

#### Returns

Returns the object itself, but modified **by reference**. You need to
explicitly `$clone()` the object beforehand if you want to keep the
object in its previous state.

------------------------------------------------------------------------

### Method `marshal()`

Marshals all stored models.

#### Usage

    BenchmarkResult$marshal(...)

#### Arguments

- `...`:

  (any)  
  Additional arguments passed to
  [`marshal_model()`](https://mlr3.mlr-org.com/dev/reference/marshaling.md).

#### Examples

    bmr$marshal()

------------------------------------------------------------------------

### Method `unmarshal()`

Unmarshals all stored models.

#### Usage

    BenchmarkResult$unmarshal(...)

#### Arguments

- `...`:

  (any)  
  Additional arguments passed to
  [`unmarshal_model()`](https://mlr3.mlr-org.com/dev/reference/marshaling.md).

#### Examples

    bmr$unmarshal()

------------------------------------------------------------------------

### Method `score()`

Returns a table with one row for each resampling iteration, including
all involved objects:
[Task](https://mlr3.mlr-org.com/dev/reference/Task.md),
[Learner](https://mlr3.mlr-org.com/dev/reference/Learner.md),
[Resampling](https://mlr3.mlr-org.com/dev/reference/Resampling.md),
iteration number (`integer(1)`), and
[Prediction](https://mlr3.mlr-org.com/dev/reference/Prediction.md). If
`ids` is set to `TRUE`, character column of extracted ids are added to
the table for convenient filtering: `"task_id"`, `"learner_id"`, and
`"resampling_id"`.

Additionally calculates the provided performance measures and binds the
performance scores as extra columns. These columns are named using the
id of the respective
[Measure](https://mlr3.mlr-org.com/dev/reference/Measure.md).

#### Usage

    BenchmarkResult$score(
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
  Adds object ids (`"task_id"`, `"learner_id"`, `"resampling_id"`) as
  extra character columns to the returned table.

- `conditions`:

  (`logical(1)`)  
  Adds condition messages (`"warnings"`, `"errors"`) as extra list
  columns of character vectors to the returned table

- `predictions`:

  (`logical(1)`)  
  Additionally return prediction objects, one column for each
  `predict_set` of all learners combined. Columns are named
  `"prediction_train"`, `"prediction_test"` and
  `"prediction_internal_valid"`, if present.

#### Returns

[`data.table::data.table()`](https://rdatatable.gitlab.io/data.table/reference/data.table.html).

#### Examples

    bmr$score(msr("classif.acc"))

------------------------------------------------------------------------

### Method `obs_loss()`

Calculates the observation-wise loss via the loss function set in the
[Measure](https://mlr3.mlr-org.com/dev/reference/Measure.md)'s field
`obs_loss`. Returns a
[`data.table()`](https://rdatatable.gitlab.io/data.table/reference/data.table.html)
with the columns `row_ids`, `truth`, `response` and one additional
numeric column for each measure, named with the respective measure id.
If there is no observation-wise loss function for the measure, the
column is filled with `NA` values. Note that some measures such as RMSE,
do have an `$obs_loss`, but they require an additional transformation
after aggregation, in this example taking the square-root.

#### Usage

    BenchmarkResult$obs_loss(measures = NULL, predict_sets = "test")

#### Arguments

- `measures`:

  ([Measure](https://mlr3.mlr-org.com/dev/reference/Measure.md) \| list
  of [Measure](https://mlr3.mlr-org.com/dev/reference/Measure.md))  
  Measure(s) to calculate.

- `predict_sets`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  The predict sets.

#### Examples

    bmr$obs_loss(msr("classif.acc"))

------------------------------------------------------------------------

### Method [`aggregate()`](https://rdrr.io/r/stats/aggregate.html)

Returns a result table where resampling iterations are combined into
[ResampleResult](https://mlr3.mlr-org.com/dev/reference/ResampleResult.md)s.
A column with the aggregated performance score is added for each
[Measure](https://mlr3.mlr-org.com/dev/reference/Measure.md), named with
the id of the respective measure.

The method for aggregation is controlled by the
[Measure](https://mlr3.mlr-org.com/dev/reference/Measure.md), e.g. micro
aggregation, macro aggregation or custom aggregation. Most measures
default to macro aggregation.

Note that the aggregated performances just give a quick impression which
approaches work well and which approaches are probably underperforming.
However, the aggregates do not account for variance and cannot replace a
statistical test. See
[mlr3viz](https://CRAN.R-project.org/package=mlr3viz) to get a better
impression via boxplots or
[mlr3benchmark](https://CRAN.R-project.org/package=mlr3benchmark) for
critical difference plots and significance tests.

For convenience, different flags can be set to extract more information
from the returned
[ResampleResult](https://mlr3.mlr-org.com/dev/reference/ResampleResult.md).

#### Usage

    BenchmarkResult$aggregate(
      measures = NULL,
      ids = TRUE,
      uhashes = FALSE,
      params = FALSE,
      conditions = FALSE
    )

#### Arguments

- `measures`:

  ([Measure](https://mlr3.mlr-org.com/dev/reference/Measure.md) \| list
  of [Measure](https://mlr3.mlr-org.com/dev/reference/Measure.md))  
  Measure(s) to calculate.

- `ids`:

  (`logical(1)`)  
  Adds object ids (`"task_id"`, `"learner_id"`, `"resampling_id"`) as
  extra character columns for convenient subsetting.

- `uhashes`:

  (`logical(1)`)  
  Adds the uhash values of the
  [ResampleResult](https://mlr3.mlr-org.com/dev/reference/ResampleResult.md)
  as extra character column `"uhash"`.

- `params`:

  (`logical(1)`)  
  Adds the hyperparameter values as extra list column `"params"`. You
  can unnest them with
  [`mlr3misc::unnest()`](https://mlr3misc.mlr-org.com/reference/unnest.html).

- `conditions`:

  (`logical(1)`)  
  Adds the number of resampling iterations with at least one warning as
  extra integer column `"warnings"`, and the number of resampling
  iterations with errors as extra integer column `"errors"`.

#### Returns

[`data.table::data.table()`](https://rdatatable.gitlab.io/data.table/reference/data.table.html).

#### Examples

    bmr$aggregate()

------------------------------------------------------------------------

### Method [`filter()`](https://rdrr.io/r/stats/filter.html)

Subsets the benchmark result. You can either directly provide the row
IDs or the uhashes of the resample results to keep, or use the
`learner_ids`, `task_ids` and `resampling_ids` arguments to filter for
learner, task and resampling IDs. The three options are mutually
exclusive.

#### Usage

    BenchmarkResult$filter(
      i = NULL,
      uhashes = NULL,
      learner_ids = NULL,
      task_ids = NULL,
      resampling_ids = NULL
    )

#### Arguments

- `i`:

  ([`integer()`](https://rdrr.io/r/base/integer.html) \| `NULL`)  
  The iteration values to filter for.

- `uhashes`:

  ([`character()`](https://rdrr.io/r/base/character.html) \| `NULL`)  
  The uhashes of the resample results to filter for.

- `learner_ids`:

  ([`character()`](https://rdrr.io/r/base/character.html) \| `NULL`)  
  The learner IDs to filter for.

- `task_ids`:

  ([`character()`](https://rdrr.io/r/base/character.html) \| `NULL`)  
  The task IDs to filter for.

- `resampling_ids`:

  ([`character()`](https://rdrr.io/r/base/character.html) \| `NULL`)  
  The resampling IDs to filter for.

#### Returns

Returns the object itself, but modified **by reference**. You need to
explicitly `$clone()` the object beforehand if you want to keeps the
object in its previous state.

#### Examples

    design = benchmark_grid(
      tsks(c("iris", "sonar")),
      lrns(c("classif.debug", "classif.featureless")),
      rsmp("holdout")
    )
    bmr = benchmark(design)
    bmr
    bmr2 = bmr$clone(deep = TRUE)
    bmr2$filter(learner_ids = "classif.featureless")
    bmr2

------------------------------------------------------------------------

### Method `resample_result()`

Retrieve the i-th
[ResampleResult](https://mlr3.mlr-org.com/dev/reference/ResampleResult.md),
by position, by unique hash `uhash` or by learner, task and resampling
IDs. All three options are mutually exclusive.

#### Usage

    BenchmarkResult$resample_result(
      i = NULL,
      uhash = NULL,
      task_id = NULL,
      learner_id = NULL,
      resampling_id = NULL
    )

#### Arguments

- `i`:

  (`integer(1)` \| `NULL`)  
  The iteration value to filter for.

- `uhash`:

  (`character(1)` \| `NULL`)  
  The unique identifier to filter for.

- `task_id`:

  (`character(1)` \| `NULL`)  
  The task ID to filter for.

- `learner_id`:

  (`character(1)` \| `NULL`)  
  The learner ID to filter for.

- `resampling_id`:

  (`character(1)` \| `NULL`)  
  The resampling ID to filter for.

#### Returns

[ResampleResult](https://mlr3.mlr-org.com/dev/reference/ResampleResult.md).

#### Examples

    design = benchmark_grid(
      tsk("iris"),
      lrns(c("classif.debug", "classif.featureless")),
      rsmp("holdout")
    )
    bmr = benchmark(design)
    bmr$resample_result(learner_id = "classif.featureless")
    bmr$resample_result(i = 1)
    bmr$resample_result(uhash = uhashes(bmr, learner_id = "classif.debug"))

------------------------------------------------------------------------

### Method `discard()`

Shrinks the BenchmarkResult by discarding parts of the internally stored
data. Note that certain operations might stop work, e.g. extracting
importance values from learners or calculating measures requiring the
task's data.

#### Usage

    BenchmarkResult$discard(backends = FALSE, models = FALSE)

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

#### Examples

    bmr$discard(models = TRUE)

------------------------------------------------------------------------

### Method `set_threshold()`

Sets the threshold for the response prediction of classification
learners, given they have output a probability prediction for a binary
classification task.

The resample results for which to change the threshold can either be
specified directly via `uhashes`, by selecting the specific iterations
(`i`) or by filtering according to learner, task and resampling IDs.

If none of the three options is specified, the threshold is set for all
resample results.

#### Usage

    BenchmarkResult$set_threshold(
      threshold,
      i = NULL,
      uhashes = NULL,
      learner_ids = NULL,
      task_ids = NULL,
      resampling_ids = NULL,
      ties_method = "random"
    )

#### Arguments

- `threshold`:

  (`numeric(1)`)  
  Threshold value.

- `i`:

  ([`integer()`](https://rdrr.io/r/base/integer.html) \| `NULL`)  
  The iteration values to filter for.

- `uhashes`:

  ([`character()`](https://rdrr.io/r/base/character.html) \| `NULL`)  
  The unique identifiers of the
  [ResampleResult](https://mlr3.mlr-org.com/dev/reference/ResampleResult.md)s
  for which the threshold should be set.

- `learner_ids`:

  ([`character()`](https://rdrr.io/r/base/character.html) \| `NULL`)  
  The learner IDs for which the threshold should be set.

- `task_ids`:

  ([`character()`](https://rdrr.io/r/base/character.html) \| `NULL`)  
  The task IDs for which the threshold should be set.

- `resampling_ids`:

  ([`character()`](https://rdrr.io/r/base/character.html) \| `NULL`)  
  The resampling IDs for which the threshold should be set.

- `ties_method`:

  (`character(1)`)  
  Method to handle ties in probabilities when selecting a class label.
  Must be one of `"random"`, `"first"` or `"last"` (corresponding to the
  same options in [`max.col()`](https://rdrr.io/r/base/maxCol.html)).

  - `"random"`: Randomly select one of the tied class labels (default).

  - `"first"`: Select the first class label among tied values.

  - `"last"`: Select the last class label among tied values.

#### Examples

    design = benchmark_grid(
      tsk("sonar"),
      lrns(c("classif.debug", "classif.featureless"), predict_type = "prob"),
      rsmp("holdout")
    )
    bmr = benchmark(design)
    bmr$set_threshold(0.8, learner_ids = "classif.featureless")
    bmr$set_threshold(0.3, i = 2)
    bmr$set_threshold(0.7, uhashes = uhashes(bmr, learner_ids = "classif.featureless"))

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    BenchmarkResult$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
set.seed(123)
learners = list(
  lrn("classif.featureless", predict_type = "prob"),
  lrn("classif.rpart", predict_type = "prob")
)

design = benchmark_grid(
  tasks = list(tsk("sonar"), tsk("penguins")),
  learners = learners,
  resamplings = rsmp("cv", folds = 3)
)
print(design)
#>        task             learner resampling
#>      <char>              <char>     <char>
#> 1:    sonar classif.featureless         cv
#> 2:    sonar       classif.rpart         cv
#> 3: penguins classif.featureless         cv
#> 4: penguins       classif.rpart         cv

bmr = benchmark(design)
print(bmr)
#> 
#> ── <BenchmarkResult> of 12 rows with 4 resampling run ──────────────────────────
#>  nr  task_id          learner_id resampling_id iters warnings errors
#>   1    sonar classif.featureless            cv     3        0      0
#>   2    sonar       classif.rpart            cv     3        0      0
#>   3 penguins classif.featureless            cv     3        0      0
#>   4 penguins       classif.rpart            cv     3        0      0

bmr$tasks
#> Key: <task_hash>
#>           task_hash  task_id                   task
#>              <char>   <char>                 <list>
#> 1: 40bc78f17c7a5f3d penguins <TaskClassif:penguins>
#> 2: f9791e97f9813150    sonar    <TaskClassif:sonar>
bmr$learners
#> Key: <learner_hash>
#>        learner_hash          learner_id
#>              <char>              <char>
#> 1: 667ca7804cbe810f       classif.rpart
#> 2: 918bbfdfae9c988e classif.featureless
#>                                            learner
#>                                             <list>
#> 1:             <LearnerClassifRpart:classif.rpart>
#> 2: <LearnerClassifFeatureless:classif.featureless>

# first 5 resampling iterations
head(as.data.table(bmr, measures = c("classif.acc", "classif.auc")), 5)
#>                                   uhash                task
#>                                  <char>              <list>
#> 1: 6a8fe46a-900b-4000-b85e-c0f29c6400f4 <TaskClassif:sonar>
#> 2: 6a8fe46a-900b-4000-b85e-c0f29c6400f4 <TaskClassif:sonar>
#> 3: 6a8fe46a-900b-4000-b85e-c0f29c6400f4 <TaskClassif:sonar>
#> 4: 4eb76bee-7c6b-486f-bad3-dfe2818e1515 <TaskClassif:sonar>
#> 5: 4eb76bee-7c6b-486f-bad3-dfe2818e1515 <TaskClassif:sonar>
#>                                            learner     resampling iteration
#>                                             <list>         <list>     <int>
#> 1: <LearnerClassifFeatureless:classif.featureless> <ResamplingCV>         1
#> 2: <LearnerClassifFeatureless:classif.featureless> <ResamplingCV>         2
#> 3: <LearnerClassifFeatureless:classif.featureless> <ResamplingCV>         3
#> 4:             <LearnerClassifRpart:classif.rpart> <ResamplingCV>         1
#> 5:             <LearnerClassifRpart:classif.rpart> <ResamplingCV>         2
#>             prediction task_id          learner_id resampling_id
#>                 <list>  <char>              <char>        <char>
#> 1: <PredictionClassif>   sonar classif.featureless            cv
#> 2: <PredictionClassif>   sonar classif.featureless            cv
#> 3: <PredictionClassif>   sonar classif.featureless            cv
#> 4: <PredictionClassif>   sonar       classif.rpart            cv
#> 5: <PredictionClassif>   sonar       classif.rpart            cv

# aggregate results
bmr$aggregate()
#>       nr  task_id          learner_id resampling_id iters classif.ce
#>    <int>   <char>              <char>        <char> <int>      <num>
#> 1:     1    sonar classif.featureless            cv     3 0.46604555
#> 2:     2    sonar       classif.rpart            cv     3 0.27391304
#> 3:     3 penguins classif.featureless            cv     3 0.55814900
#> 4:     4 penguins       classif.rpart            cv     3 0.05812357
#> Hidden columns: resample_result

# aggregate results with hyperparameters as separate columns
mlr3misc::unnest(bmr$aggregate(params = TRUE), "params")
#>       nr  task_id          learner_id resampling_id iters classif.ce method
#>    <int>   <char>              <char>        <char> <int>      <num> <char>
#> 1:     1    sonar classif.featureless            cv     3 0.46604555   mode
#> 2:     2    sonar       classif.rpart            cv     3 0.27391304   <NA>
#> 3:     3 penguins classif.featureless            cv     3 0.55814900   mode
#> 4:     4 penguins       classif.rpart            cv     3 0.05812357   <NA>
#>     xval
#>    <int>
#> 1:    NA
#> 2:     0
#> 3:    NA
#> 4:     0
#> Hidden columns: resample_result

# extract resample result for classif.rpart
rr = bmr$aggregate()[learner_id == "classif.rpart", resample_result][[1]]
print(rr)
#> 
#> ── <ResampleResult> with 3 resampling iterations ───────────────────────────────
#>  task_id    learner_id resampling_id iteration     prediction_test warnings
#>    sonar classif.rpart            cv         1 <PredictionClassif>        0
#>    sonar classif.rpart            cv         2 <PredictionClassif>        0
#>    sonar classif.rpart            cv         3 <PredictionClassif>        0
#>  errors
#>       0
#>       0
#>       0

# access the confusion matrix of the first resampling iteration
rr$predictions()[[1]]$confusion
#>         truth
#> response  M  R
#>        M 30 18
#>        R  3 19

# reduce to subset with task id "sonar"
bmr$filter(task_ids = "sonar")
print(bmr)
#> 
#> ── <BenchmarkResult> of 6 rows with 2 resampling run ───────────────────────────
#>  nr task_id          learner_id resampling_id iters warnings errors
#>   1   sonar classif.featureless            cv     3        0      0
#>   2   sonar       classif.rpart            cv     3        0      0

## ------------------------------------------------
## Method `BenchmarkResult$marshal`
## ------------------------------------------------

bmr$marshal()

## ------------------------------------------------
## Method `BenchmarkResult$unmarshal`
## ------------------------------------------------

bmr$unmarshal()

## ------------------------------------------------
## Method `BenchmarkResult$score`
## ------------------------------------------------

bmr$score(msr("classif.acc"))
#>       nr task_id          learner_id resampling_id iteration
#>    <int>  <char>              <char>        <char>     <int>
#> 1:     1   sonar classif.featureless            cv         1
#> 2:     1   sonar classif.featureless            cv         2
#> 3:     1   sonar classif.featureless            cv         3
#> 4:     2   sonar       classif.rpart            cv         1
#> 5:     2   sonar       classif.rpart            cv         2
#> 6:     2   sonar       classif.rpart            cv         3
#>        prediction_test classif.acc
#>                 <list>       <num>
#> 1: <PredictionClassif>   0.4714286
#> 2: <PredictionClassif>   0.5652174
#> 3: <PredictionClassif>   0.5652174
#> 4: <PredictionClassif>   0.7000000
#> 5: <PredictionClassif>   0.7536232
#> 6: <PredictionClassif>   0.7246377
#> Hidden columns: uhash, task, learner, resampling

## ------------------------------------------------
## Method `BenchmarkResult$obs_loss`
## ------------------------------------------------

bmr$obs_loss(msr("classif.acc"))
#>      resample_result iteration row_ids  truth response    prob.M    prob.R
#>                <int>     <int>   <int> <fctr>   <fctr>     <num>     <num>
#>   1:               1         1       8      R        M 0.5652174 0.4347826
#>   2:               1         1       9      R        M 0.5652174 0.4347826
#>   3:               1         1      10      R        M 0.5652174 0.4347826
#>   4:               1         1      14      R        M 0.5652174 0.4347826
#>   5:               1         1      22      R        M 0.5652174 0.4347826
#>  ---                                                                      
#> 412:               2         3     198      M        M 0.9500000 0.0500000
#> 413:               2         3     199      M        M 0.9500000 0.0500000
#> 414:               2         3     203      M        M 0.9500000 0.0500000
#> 415:               2         3     204      M        M 0.9500000 0.0500000
#> 416:               2         3     208      M        M 0.9500000 0.0500000
#>      classif.acc
#>            <int>
#>   1:           0
#>   2:           0
#>   3:           0
#>   4:           0
#>   5:           0
#>  ---            
#> 412:           1
#> 413:           1
#> 414:           1
#> 415:           1
#> 416:           1

## ------------------------------------------------
## Method `BenchmarkResult$aggregate`
## ------------------------------------------------

bmr$aggregate()
#>       nr task_id          learner_id resampling_id iters classif.ce
#>    <int>  <char>              <char>        <char> <int>      <num>
#> 1:     1   sonar classif.featureless            cv     3  0.4660455
#> 2:     2   sonar       classif.rpart            cv     3  0.2739130
#> Hidden columns: resample_result

## ------------------------------------------------
## Method `BenchmarkResult$filter`
## ------------------------------------------------

design = benchmark_grid(
  tsks(c("iris", "sonar")),
  lrns(c("classif.debug", "classif.featureless")),
  rsmp("holdout")
)
bmr = benchmark(design)
bmr
#> 
#> ── <BenchmarkResult> of 4 rows with 4 resampling run ───────────────────────────
#>  nr task_id          learner_id resampling_id iters warnings errors
#>   1    iris       classif.debug       holdout     1        0      0
#>   2    iris classif.featureless       holdout     1        0      0
#>   3   sonar       classif.debug       holdout     1        0      0
#>   4   sonar classif.featureless       holdout     1        0      0
bmr2 = bmr$clone(deep = TRUE)
bmr2$filter(learner_ids = "classif.featureless")
bmr2
#> 
#> ── <BenchmarkResult> of 2 rows with 2 resampling run ───────────────────────────
#>  nr task_id          learner_id resampling_id iters warnings errors
#>   1    iris classif.featureless       holdout     1        0      0
#>   2   sonar classif.featureless       holdout     1        0      0

## ------------------------------------------------
## Method `BenchmarkResult$resample_result`
## ------------------------------------------------

design = benchmark_grid(
  tsk("iris"),
  lrns(c("classif.debug", "classif.featureless")),
  rsmp("holdout")
)
bmr = benchmark(design)
bmr$resample_result(learner_id = "classif.featureless")
#> 
#> ── <ResampleResult> with 1 resampling iterations ───────────────────────────────
#>  task_id          learner_id resampling_id iteration     prediction_test
#>     iris classif.featureless       holdout         1 <PredictionClassif>
#>  warnings errors
#>         0      0
bmr$resample_result(i = 1)
#> 
#> ── <ResampleResult> with 1 resampling iterations ───────────────────────────────
#>  task_id    learner_id resampling_id iteration     prediction_test warnings
#>     iris classif.debug       holdout         1 <PredictionClassif>        0
#>  errors
#>       0
bmr$resample_result(uhash = uhashes(bmr, learner_id = "classif.debug"))
#> 
#> ── <ResampleResult> with 1 resampling iterations ───────────────────────────────
#>  task_id    learner_id resampling_id iteration     prediction_test warnings
#>     iris classif.debug       holdout         1 <PredictionClassif>        0
#>  errors
#>       0

## ------------------------------------------------
## Method `BenchmarkResult$discard`
## ------------------------------------------------

bmr$discard(models = TRUE)

## ------------------------------------------------
## Method `BenchmarkResult$set_threshold`
## ------------------------------------------------

design = benchmark_grid(
  tsk("sonar"),
  lrns(c("classif.debug", "classif.featureless"), predict_type = "prob"),
  rsmp("holdout")
)
bmr = benchmark(design)
bmr$set_threshold(0.8, learner_ids = "classif.featureless")
#> Key: <uhash, iteration>
#>                                   uhash iteration      learner_state prediction
#>                                  <char>     <int>             <list>     <list>
#> 1: 01f069d4-e936-489b-bf2a-c2817f5fc103         1 <learner_state[9]>  <list[1]>
#> 2: 6830f4c0-f8f8-4a73-8251-76c3b2af2a27         1 <learner_state[8]>  <list[1]>
#>        learner_hash        task_hash    learner_phash  resampling_hash
#>              <char>           <char>           <char>           <char>
#> 1: c7c9e936a7ad47c2 f9791e97f9813150 02fee2b4eac6c687 35db3d2bb507d357
#> 2: 918bbfdfae9c988e f9791e97f9813150 53c7d4b30df61d77 35db3d2bb507d357
bmr$set_threshold(0.3, i = 2)
#> Key: <uhash, iteration>
#>                                   uhash iteration      learner_state prediction
#>                                  <char>     <int>             <list>     <list>
#> 1: 01f069d4-e936-489b-bf2a-c2817f5fc103         1 <learner_state[9]>  <list[1]>
#> 2: 6830f4c0-f8f8-4a73-8251-76c3b2af2a27         1 <learner_state[8]>  <list[1]>
#>        learner_hash        task_hash    learner_phash  resampling_hash
#>              <char>           <char>           <char>           <char>
#> 1: c7c9e936a7ad47c2 f9791e97f9813150 02fee2b4eac6c687 35db3d2bb507d357
#> 2: 918bbfdfae9c988e f9791e97f9813150 53c7d4b30df61d77 35db3d2bb507d357
bmr$set_threshold(0.7, uhashes = uhashes(bmr, learner_ids = "classif.featureless"))
#> Key: <uhash, iteration>
#>                                   uhash iteration      learner_state prediction
#>                                  <char>     <int>             <list>     <list>
#> 1: 01f069d4-e936-489b-bf2a-c2817f5fc103         1 <learner_state[9]>  <list[1]>
#> 2: 6830f4c0-f8f8-4a73-8251-76c3b2af2a27         1 <learner_state[8]>  <list[1]>
#>        learner_hash        task_hash    learner_phash  resampling_hash
#>              <char>           <char>           <char>           <char>
#> 1: c7c9e936a7ad47c2 f9791e97f9813150 02fee2b4eac6c687 35db3d2bb507d357
#> 2: 918bbfdfae9c988e f9791e97f9813150 53c7d4b30df61d77 35db3d2bb507d357
```
