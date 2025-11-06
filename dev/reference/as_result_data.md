# Convert to ResultData

This function allows to construct or convert to a
[ResultData](https://mlr3.mlr-org.com/dev/reference/ResultData.md)
object, the result container used by
[ResampleResult](https://mlr3.mlr-org.com/dev/reference/ResampleResult.md)
and
[BenchmarkResult](https://mlr3.mlr-org.com/dev/reference/BenchmarkResult.md).
A
[ResampleResult](https://mlr3.mlr-org.com/dev/reference/ResampleResult.md)
or
[BenchmarkResult](https://mlr3.mlr-org.com/dev/reference/BenchmarkResult.md)
can be initialized with the returned object. Note that
[ResampleResult](https://mlr3.mlr-org.com/dev/reference/ResampleResult.md)s
can be converted to a
[BenchmarkResult](https://mlr3.mlr-org.com/dev/reference/BenchmarkResult.md)
with
[`as_benchmark_result()`](https://mlr3.mlr-org.com/dev/reference/as_benchmark_result.md)
and multiple
[BenchmarkResult](https://mlr3.mlr-org.com/dev/reference/BenchmarkResult.md)s
can be combined to a larger
[BenchmarkResult](https://mlr3.mlr-org.com/dev/reference/BenchmarkResult.md)
with the `$combine()` method of
[BenchmarkResult](https://mlr3.mlr-org.com/dev/reference/BenchmarkResult.md).

## Usage

``` r
as_result_data(
  task,
  learners,
  resampling,
  iterations,
  predictions,
  learner_states = NULL,
  data_extra = NULL,
  store_backends = TRUE
)
```

## Arguments

- task:

  ([Task](https://mlr3.mlr-org.com/dev/reference/Task.md)).

- learners:

  (list of trained
  [Learner](https://mlr3.mlr-org.com/dev/reference/Learner.md)s).

- resampling:

  ([Resampling](https://mlr3.mlr-org.com/dev/reference/Resampling.md)).

- iterations:

  ([`integer()`](https://rdrr.io/r/base/integer.html)).

- predictions:

  (list of list of
  [Prediction](https://mlr3.mlr-org.com/dev/reference/Prediction.md)s).

- learner_states:

  ([`list()`](https://rdrr.io/r/base/list.html))  
  Learner states. If not provided, the states of `learners` are
  automatically extracted.

- data_extra:

  ([`list()`](https://rdrr.io/r/base/list.html))  
  Additional data for each iteration.

- store_backends:

  (`logical(1)`)  
  If set to `FALSE`, the backends of the
  [Task](https://mlr3.mlr-org.com/dev/reference/Task.md)s provided in
  `data` are removed.

## Value

`ResultData` object which can be passed to the constructor of
[ResampleResult](https://mlr3.mlr-org.com/dev/reference/ResampleResult.md).

## Examples

``` r
task = tsk("penguins")
learner = lrn("classif.rpart")
resampling = rsmp("cv", folds = 2)$instantiate(task)
iterations = seq_len(resampling$iters)

# manually train two learners.
# store learners and predictions
learners = list()
predictions = list()
for (i in iterations) {
  l = learner$clone(deep = TRUE)
  learners[[i]] = l$train(task, row_ids = resampling$train_set(i))
  predictions[[i]] = list(test = l$predict(task, row_ids = resampling$test_set(i)))
}

rdata = as_result_data(task, learners, resampling, iterations, predictions)
ResampleResult$new(rdata)
#> 
#> ── <ResampleResult> with 2 resampling iterations ───────────────────────────────
#>   task_id    learner_id resampling_id iteration     prediction_test warnings
#>  penguins classif.rpart            cv         1 <PredictionClassif>        0
#>  penguins classif.rpart            cv         2 <PredictionClassif>        0
#>  errors
#>       0
#>       0
```
