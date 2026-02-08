# ResultData

Internal object to store results in list of data.tables, arranged in a
star schema. It is advised to not directly work on this data structure
as it may be changed in the future without further warnings.

The main motivation of this data structure is the necessity to avoid
storing duplicated [R6](https://r6.r-lib.org/reference/R6Class.html)
objects. While this is usually no problem in a single R session,
serialization via [`serialize()`](https://rdrr.io/r/base/serialize.html)
(which is used in
[`save()`](https://rdrr.io/r/base/save.html)/[`saveRDS()`](https://rdrr.io/r/base/readRDS.html)
or during parallelization) leads to objects with unreasonable memory
requirements.

## Public fields

- `data`:

  ([`list()`](https://rdrr.io/r/base/list.html))  
  List of
  [`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html),
  arranged in a star schema. Do not operate directly on this list.

## Active bindings

- `task_type`:

  (`character(1)`)  
  Returns the task type of stored objects, e.g. `"classif"` or `"regr"`.
  Returns `NULL` if the ResultData is empty.

## Methods

### Public methods

- [`ResultData$new()`](#method-ResultData-new)

- [`ResultData$uhashes()`](#method-ResultData-uhashes)

- [`ResultData$uhash_table()`](#method-ResultData-uhash_table)

- [`ResultData$iterations()`](#method-ResultData-iterations)

- [`ResultData$tasks()`](#method-ResultData-tasks)

- [`ResultData$learners()`](#method-ResultData-learners)

- [`ResultData$learner_states()`](#method-ResultData-learner_states)

- [`ResultData$resamplings()`](#method-ResultData-resamplings)

- [`ResultData$predictions()`](#method-ResultData-predictions)

- [`ResultData$prediction()`](#method-ResultData-prediction)

- [`ResultData$data_extra()`](#method-ResultData-data_extra)

- [`ResultData$combine()`](#method-ResultData-combine)

- [`ResultData$sweep()`](#method-ResultData-sweep)

- [`ResultData$marshal()`](#method-ResultData-marshal)

- [`ResultData$unmarshal()`](#method-ResultData-unmarshal)

- [`ResultData$discard()`](#method-ResultData-discard)

- [`ResultData$as_data_table()`](#method-ResultData-as_data_table)

- [`ResultData$logs()`](#method-ResultData-logs)

- [`ResultData$set_threshold()`](#method-ResultData-set_threshold)

- [`ResultData$clone()`](#method-ResultData-clone)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class. An alternative
construction method is provided by
[`as_result_data()`](https://mlr3.mlr-org.com/dev/reference/as_result_data.md).

#### Usage

    ResultData$new(data = NULL, data_extra = NULL, store_backends = TRUE)

#### Arguments

- `data`:

  ([`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html))
  \| `NULL`)  
  Do not initialize this object yourself, use
  [`as_result_data()`](https://mlr3.mlr-org.com/dev/reference/as_result_data.md)
  instead.

- `data_extra`:

  ([`list()`](https://rdrr.io/r/base/list.html))  
  Additional data to store. This can be used to store additional
  information for each iteration.

- `store_backends`:

  (`logical(1)`)  
  If set to `FALSE`, the backends of the
  [Task](https://mlr3.mlr-org.com/dev/reference/Task.md)s provided in
  `data` are removed.

------------------------------------------------------------------------

### Method [`uhashes()`](https://mlr3.mlr-org.com/dev/reference/uhash.md)

Returns all unique hashes (`uhash` values) of all included
[ResampleResult](https://mlr3.mlr-org.com/dev/reference/ResampleResult.md)s.

#### Usage

    ResultData$uhashes(view = NULL)

#### Arguments

- `view`:

  (`character(1)` \| `NULL`)  
  Single `uhash` to restrict the results to.

- `view`:

  (`character(1)` \| `NULL`)  
  Single `uhash` to restrict the results to.

#### Returns

[`character()`](https://rdrr.io/r/base/character.html).

------------------------------------------------------------------------

### Method `uhash_table()`

Returns a
[`data.table`](https://rdrr.io/pkg/data.table/man/data.table.html) with
columns `uhash`, `learner_id`, `task_id` and `resampling_id` for the
given view. The `uhash` uniquely identifies an individual
[`ResampleResult`](https://mlr3.mlr-org.com/dev/reference/ResampleResult.md).

#### Usage

    ResultData$uhash_table(view = NULL)

#### Arguments

- `view`:

  (`character(1)` \| `NULL`)  
  Single `uhash` to restrict the results to.

- `view`:

  (`character(1)` \| `NULL`)  
  Single `uhash` to restrict the results to.

#### Returns

[`data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html)

------------------------------------------------------------------------

### Method `iterations()`

Returns the number of recorded iterations / experiments.

#### Usage

    ResultData$iterations(view = NULL)

#### Arguments

- `view`:

  (`character(1)` \| `NULL`)  
  Single `uhash` to restrict the results to.

- `view`:

  (`character(1)` \| `NULL`)  
  Single `uhash` to restrict the results to.

#### Returns

`integer(1)`.

------------------------------------------------------------------------

### Method `tasks()`

Returns a table of included
[Task](https://mlr3.mlr-org.com/dev/reference/Task.md)s.

#### Usage

    ResultData$tasks(view = NULL)

#### Arguments

- `view`:

  (`character(1)` \| `NULL`)  
  Single `uhash` to restrict the results to.

- `view`:

  (`character(1)` \| `NULL`)  
  Single `uhash` to restrict the results to.

#### Returns

[`data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html)
with columns `"task_hash"`
([`character()`](https://rdrr.io/r/base/character.html)) and `"task"`
([Task](https://mlr3.mlr-org.com/dev/reference/Task.md)).

------------------------------------------------------------------------

### Method `learners()`

Returns a table of included
[Learner](https://mlr3.mlr-org.com/dev/reference/Learner.md)s.

#### Usage

    ResultData$learners(view = NULL, states = TRUE, reassemble = TRUE)

#### Arguments

- `view`:

  (`character(1)` \| `NULL`)  
  Single `uhash` to restrict the results to.

- `view`:

  (`character(1)` \| `NULL`)  
  Single `uhash` to restrict the results to.

- `states`:

  (`logical(1)`)  
  If `TRUE`, returns a learner for each iteration/experiment in the
  ResultData object. If `FALSE`, returns an exemplary learner (without
  state) for each
  [ResampleResult](https://mlr3.mlr-org.com/dev/reference/ResampleResult.md).

- `reassemble`:

  (`logical(1)`)  
  Reassemble the learners, i.e. re-set the `state` and the
  hyperparameters which are stored separately before returning the
  learners.

#### Returns

[`data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html)
with columns `"learner_hash"`
([`character()`](https://rdrr.io/r/base/character.html)) and `"learner"`
([Learner](https://mlr3.mlr-org.com/dev/reference/Learner.md)).

------------------------------------------------------------------------

### Method `learner_states()`

Returns a list of states of included
[Learner](https://mlr3.mlr-org.com/dev/reference/Learner.md)s without
reassembling the learners.

@return list of [`list()`](https://rdrr.io/r/base/list.html)

#### Usage

    ResultData$learner_states(view = NULL)

#### Arguments

- `view`:

  (`character(1)` \| `NULL`)  
  Single `uhash` to restrict the results to.

- `view`:

  (`character(1)` \| `NULL`)  
  Single `uhash` to restrict the results to.

------------------------------------------------------------------------

### Method `resamplings()`

Returns a table of included
[Resampling](https://mlr3.mlr-org.com/dev/reference/Resampling.md)s.

#### Usage

    ResultData$resamplings(view = NULL)

#### Arguments

- `view`:

  (`character(1)` \| `NULL`)  
  Single `uhash` to restrict the results to.

- `view`:

  (`character(1)` \| `NULL`)  
  Single `uhash` to restrict the results to.

#### Returns

[`data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html)
with columns `"resampling_hash"`
([`character()`](https://rdrr.io/r/base/character.html)) and
`"resampling"`
([Resampling](https://mlr3.mlr-org.com/dev/reference/Resampling.md)).

------------------------------------------------------------------------

### Method `predictions()`

Returns a list of
[Prediction](https://mlr3.mlr-org.com/dev/reference/Prediction.md)
objects.

#### Usage

    ResultData$predictions(view = NULL, predict_sets = "test")

#### Arguments

- `view`:

  (`character(1)` \| `NULL`)  
  Single `uhash` to restrict the results to.

- `view`:

  (`character(1)` \| `NULL`)  
  Single `uhash` to restrict the results to.

- `predict_sets`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Prediction sets to operate on, used in
  [`aggregate()`](https://rdrr.io/r/stats/aggregate.html) to extract the
  matching `predict_sets` from the
  [ResampleResult](https://mlr3.mlr-org.com/dev/reference/ResampleResult.md).
  Multiple predict sets are calculated by the respective
  [Learner](https://mlr3.mlr-org.com/dev/reference/Learner.md) during
  [`resample()`](https://mlr3.mlr-org.com/dev/reference/resample.md)/[`benchmark()`](https://mlr3.mlr-org.com/dev/reference/benchmark.md).
  Must be a non-empty subset of `{"train", "test", "internal_valid"}`.
  If multiple sets are provided, these are first combined to a single
  prediction object. Default is `"test"`.

- `predict_sets`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Prediction sets to operate on, used in
  [`aggregate()`](https://rdrr.io/r/stats/aggregate.html) to extract the
  matching `predict_sets` from the
  [ResampleResult](https://mlr3.mlr-org.com/dev/reference/ResampleResult.md).
  Multiple predict sets are calculated by the respective
  [Learner](https://mlr3.mlr-org.com/dev/reference/Learner.md) during
  [`resample()`](https://mlr3.mlr-org.com/dev/reference/resample.md)/[`benchmark()`](https://mlr3.mlr-org.com/dev/reference/benchmark.md).
  Must be a non-empty subset of `{"train", "test", "internal_valid"}`.
  If multiple sets are provided, these are first combined to a single
  prediction object. Default is `"test"`.

- `predict_sets`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Prediction sets to operate on, used in
  [`aggregate()`](https://rdrr.io/r/stats/aggregate.html) to extract the
  matching `predict_sets` from the
  [ResampleResult](https://mlr3.mlr-org.com/dev/reference/ResampleResult.md).
  Multiple predict sets are calculated by the respective
  [Learner](https://mlr3.mlr-org.com/dev/reference/Learner.md) during
  [`resample()`](https://mlr3.mlr-org.com/dev/reference/resample.md)/[`benchmark()`](https://mlr3.mlr-org.com/dev/reference/benchmark.md).
  Must be a non-empty subset of `{"train", "test", "internal_valid"}`.
  If multiple sets are provided, these are first combined to a single
  prediction object. Default is `"test"`.

#### Returns

[`list()`](https://rdrr.io/r/base/list.html) of
[Prediction](https://mlr3.mlr-org.com/dev/reference/Prediction.md).

------------------------------------------------------------------------

### Method `prediction()`

Returns a combined
[Prediction](https://mlr3.mlr-org.com/dev/reference/Prediction.md)
objects.

#### Usage

    ResultData$prediction(view = NULL, predict_sets = "test")

#### Arguments

- `view`:

  (`character(1)` \| `NULL`)  
  Single `uhash` to restrict the results to.

- `view`:

  (`character(1)` \| `NULL`)  
  Single `uhash` to restrict the results to.

- `predict_sets`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Prediction sets to operate on, used in
  [`aggregate()`](https://rdrr.io/r/stats/aggregate.html) to extract the
  matching `predict_sets` from the
  [ResampleResult](https://mlr3.mlr-org.com/dev/reference/ResampleResult.md).
  Multiple predict sets are calculated by the respective
  [Learner](https://mlr3.mlr-org.com/dev/reference/Learner.md) during
  [`resample()`](https://mlr3.mlr-org.com/dev/reference/resample.md)/[`benchmark()`](https://mlr3.mlr-org.com/dev/reference/benchmark.md).
  Must be a non-empty subset of `{"train", "test", "internal_valid"}`.
  If multiple sets are provided, these are first combined to a single
  prediction object. Default is `"test"`.

- `predict_sets`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Prediction sets to operate on, used in
  [`aggregate()`](https://rdrr.io/r/stats/aggregate.html) to extract the
  matching `predict_sets` from the
  [ResampleResult](https://mlr3.mlr-org.com/dev/reference/ResampleResult.md).
  Multiple predict sets are calculated by the respective
  [Learner](https://mlr3.mlr-org.com/dev/reference/Learner.md) during
  [`resample()`](https://mlr3.mlr-org.com/dev/reference/resample.md)/[`benchmark()`](https://mlr3.mlr-org.com/dev/reference/benchmark.md).
  Must be a non-empty subset of `{"train", "test", "internal_valid"}`.
  If multiple sets are provided, these are first combined to a single
  prediction object. Default is `"test"`.

- `predict_sets`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Prediction sets to operate on, used in
  [`aggregate()`](https://rdrr.io/r/stats/aggregate.html) to extract the
  matching `predict_sets` from the
  [ResampleResult](https://mlr3.mlr-org.com/dev/reference/ResampleResult.md).
  Multiple predict sets are calculated by the respective
  [Learner](https://mlr3.mlr-org.com/dev/reference/Learner.md) during
  [`resample()`](https://mlr3.mlr-org.com/dev/reference/resample.md)/[`benchmark()`](https://mlr3.mlr-org.com/dev/reference/benchmark.md).
  Must be a non-empty subset of `{"train", "test", "internal_valid"}`.
  If multiple sets are provided, these are first combined to a single
  prediction object. Default is `"test"`.

#### Returns

[Prediction](https://mlr3.mlr-org.com/dev/reference/Prediction.md).

------------------------------------------------------------------------

### Method `data_extra()`

Returns additional data stored.

#### Usage

    ResultData$data_extra(view = NULL)

#### Arguments

- `view`:

  (`character(1)` \| `NULL`)  
  Single `uhash` to restrict the results to.

- `view`:

  (`character(1)` \| `NULL`)  
  Single `uhash` to restrict the results to.

#### Returns

[`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html).

------------------------------------------------------------------------

### Method `combine()`

Combines multiple ResultData objects, modifying `self` in-place.

#### Usage

    ResultData$combine(rdata)

#### Arguments

- `rdata`:

  (ResultData).

#### Returns

`self` (invisibly).

------------------------------------------------------------------------

### Method [`sweep()`](https://rdrr.io/r/base/sweep.html)

Updates the ResultData object, removing rows from all tables which are
not referenced by the fact table anymore. E.g., can be called after
filtering/subsetting the fact table.

#### Usage

    ResultData$sweep()

#### Returns

Modified `self` (invisibly).

------------------------------------------------------------------------

### Method `marshal()`

Marshals all stored learner models. This will do nothing to models that
are already marshaled.

#### Usage

    ResultData$marshal(...)

#### Arguments

- `...`:

  (any)  
  Additional arguments passed to
  [`marshal_model()`](https://mlr3.mlr-org.com/dev/reference/marshaling.md).

------------------------------------------------------------------------

### Method `unmarshal()`

Unmarshals all stored learner models. This will do nothing to models
which are not marshaled.

#### Usage

    ResultData$unmarshal(...)

#### Arguments

- `...`:

  (any)  
  Additional arguments passed to
  [`unmarshal_model()`](https://mlr3.mlr-org.com/dev/reference/marshaling.md).

------------------------------------------------------------------------

### Method `discard()`

Shrinks the object by discarding parts of the stored data.

#### Usage

    ResultData$discard(backends = FALSE, models = FALSE)

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

Modified `self` (invisibly).

------------------------------------------------------------------------

### Method `as_data_table()`

Combines internal tables into a single flat
[`data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html).

#### Usage

    ResultData$as_data_table(
      view = NULL,
      reassemble_learners = TRUE,
      convert_predictions = TRUE,
      predict_sets = "test"
    )

#### Arguments

- `view`:

  (`character(1)` \| `NULL`)  
  Single `uhash` to restrict the results to.

- `view`:

  (`character(1)` \| `NULL`)  
  Single `uhash` to restrict the results to.

- `reassemble_learners`:

  (`logical(1)`)  
  Reassemble the tasks?

- `convert_predictions`:

  (`logical(1)`)  
  Convert
  [PredictionData](https://mlr3.mlr-org.com/dev/reference/PredictionData.md)
  to [Prediction](https://mlr3.mlr-org.com/dev/reference/Prediction.md)?

- `predict_sets`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Prediction sets to operate on, used in
  [`aggregate()`](https://rdrr.io/r/stats/aggregate.html) to extract the
  matching `predict_sets` from the
  [ResampleResult](https://mlr3.mlr-org.com/dev/reference/ResampleResult.md).
  Multiple predict sets are calculated by the respective
  [Learner](https://mlr3.mlr-org.com/dev/reference/Learner.md) during
  [`resample()`](https://mlr3.mlr-org.com/dev/reference/resample.md)/[`benchmark()`](https://mlr3.mlr-org.com/dev/reference/benchmark.md).
  Must be a non-empty subset of `{"train", "test", "internal_valid"}`.
  If multiple sets are provided, these are first combined to a single
  prediction object. Default is `"test"`.

- `predict_sets`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Prediction sets to operate on, used in
  [`aggregate()`](https://rdrr.io/r/stats/aggregate.html) to extract the
  matching `predict_sets` from the
  [ResampleResult](https://mlr3.mlr-org.com/dev/reference/ResampleResult.md).
  Multiple predict sets are calculated by the respective
  [Learner](https://mlr3.mlr-org.com/dev/reference/Learner.md) during
  [`resample()`](https://mlr3.mlr-org.com/dev/reference/resample.md)/[`benchmark()`](https://mlr3.mlr-org.com/dev/reference/benchmark.md).
  Must be a non-empty subset of `{"train", "test", "internal_valid"}`.
  If multiple sets are provided, these are first combined to a single
  prediction object. Default is `"test"`.

- `predict_sets`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Prediction sets to operate on, used in
  [`aggregate()`](https://rdrr.io/r/stats/aggregate.html) to extract the
  matching `predict_sets` from the
  [ResampleResult](https://mlr3.mlr-org.com/dev/reference/ResampleResult.md).
  Multiple predict sets are calculated by the respective
  [Learner](https://mlr3.mlr-org.com/dev/reference/Learner.md) during
  [`resample()`](https://mlr3.mlr-org.com/dev/reference/resample.md)/[`benchmark()`](https://mlr3.mlr-org.com/dev/reference/benchmark.md).
  Must be a non-empty subset of `{"train", "test", "internal_valid"}`.
  If multiple sets are provided, these are first combined to a single
  prediction object. Default is `"test"`.

------------------------------------------------------------------------

### Method `logs()`

Get a table of recorded learner logs.

#### Usage

    ResultData$logs(view = NULL, condition)

#### Arguments

- `view`:

  (`character(1)` \| `NULL`)  
  Single `uhash` to restrict the results to.

- `view`:

  (`character(1)` \| `NULL`)  
  Single `uhash` to restrict the results to.

- `condition`:

  (`character(1)`) The condition to extract. One of `"message"`,
  `"warning"` or `"error"`.

#### Returns

[`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html).

------------------------------------------------------------------------

### Method `set_threshold()`

Sets the threshold for the response prediction of classification
learners, given they have output a probability prediction.

#### Usage

    ResultData$set_threshold(view = NULL, threshold, ties_method = "random")

#### Arguments

- `view`:

  (`character(1)` \| `NULL`)  
  Single `uhash` to restrict the results to.

- `view`:

  (`character(1)` \| `NULL`)  
  Single `uhash` to restrict the results to.

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

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ResultData$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# table overview
print(ResultData$new()$data)
#> $fact
#> Key: <uhash, iteration>
#> Empty data.table (0 rows and 8 cols): uhash,iteration,learner_state,prediction,learner_hash,task_hash...
#> 
#> $uhashes
#> Empty data.table (0 rows and 1 cols): uhash
#> 
#> $tasks
#> Key: <task_hash>
#> Empty data.table (0 rows and 2 cols): task_hash,task
#> 
#> $learners
#> Key: <learner_phash>
#> Empty data.table (0 rows and 2 cols): learner_phash,learner
#> 
#> $resamplings
#> Key: <resampling_hash>
#> Empty data.table (0 rows and 2 cols): resampling_hash,resampling
#> 
#> $learner_components
#> Key: <learner_hash>
#> Empty data.table (0 rows and 2 cols): learner_hash,learner_param_vals
#> 
#> $data_extras
#> Key: <uhash, iteration>
#> Empty data.table (0 rows and 3 cols): uhash,iteration,data_extra
#> 
```
