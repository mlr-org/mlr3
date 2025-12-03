# Assertion for mlr3 Objects

Functions intended to be used in packages extending mlr3. Most assertion
functions ensure the right class attribute, and optionally additional
properties. Additionally, the following compound assertions are
implemented:

- `assert_learnable(task, learner)`  
  ([Task](https://mlr3.mlr-org.com/reference/Task.md),
  [Learner](https://mlr3.mlr-org.com/reference/Learner.md)) -\> `NULL`  
  Checks if the learner is applicable to the task. This includes type
  checks on the type, the feature types, and properties.

If an assertion fails, an exception is raised. Otherwise, the input
object is returned invisibly.

Asserts whether the input is a valid value for the `$validate` field of
a [`Learner`](https://mlr3.mlr-org.com/reference/Learner.md).

## Usage

``` r
assert_backend(b, .var.name = vname(b))

assert_task(
  task,
  task_type = NULL,
  feature_types = NULL,
  task_properties = NULL,
  .var.name = vname(task)
)

assert_tasks(
  tasks,
  task_type = NULL,
  feature_types = NULL,
  task_properties = NULL,
  .var.name = vname(tasks)
)

assert_learner(
  learner,
  task = NULL,
  task_type = NULL,
  properties = character(),
  .var.name = vname(learner)
)

assert_learners(
  learners,
  task = NULL,
  task_type = NULL,
  properties = character(),
  unique_ids = FALSE,
  .var.name = vname(learners)
)

assert_learnable(task, learner, param_values = NULL)

assert_predictable(task, learner)

assert_measure(
  measure,
  task = NULL,
  learner = NULL,
  prediction = NULL,
  .var.name = vname(measure)
)

assert_scorable(
  measure,
  task,
  learner,
  prediction = NULL,
  .var.name = vname(measure)
)

assert_measures(
  measures,
  task = NULL,
  learner = NULL,
  .var.name = vname(measures)
)

assert_resampling(
  resampling,
  instantiated = NULL,
  .var.name = vname(resampling)
)

assert_resamplings(
  resamplings,
  instantiated = NULL,
  .var.name = vname(resamplings)
)

assert_prediction(prediction, .var.name = vname(prediction), null.ok = FALSE)

assert_resample_result(rr, .var.name = vname(rr))

assert_benchmark_result(bmr, .var.name = vname(bmr))

assert_row_ids(
  row_ids,
  task = NULL,
  null.ok = FALSE,
  .var.name = vname(row_ids)
)

assert_has_backend(task)

assert_quantiles(learner, quantile_response = FALSE)

assert_validate(x)
```

## Arguments

- b:

  ([DataBackend](https://mlr3.mlr-org.com/reference/DataBackend.md)).

- task:

  ([Task](https://mlr3.mlr-org.com/reference/Task.md)).

- task_type:

  (`character(1)`).

- feature_types:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Feature types the learner operates on. Must be a subset of
  [`mlr_reflections$task_feature_types`](https://mlr3.mlr-org.com/reference/mlr_reflections.md).

- task_properties:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Set of required task properties.

- tasks:

  (list of [Task](https://mlr3.mlr-org.com/reference/Task.md)).

- learner:

  ([Learner](https://mlr3.mlr-org.com/reference/Learner.md)).

- learners:

  (list of [Learner](https://mlr3.mlr-org.com/reference/Learner.md)).

- param_values:

  ([`list()`](https://rdrr.io/r/base/list.html))  
  TuneToken are not allowed in the parameter set of the learner. If the
  `param_values` overwrite the TuneToken, the assertion will pass.

- measure:

  ([Measure](https://mlr3.mlr-org.com/reference/Measure.md)).

- prediction:

  ([Prediction](https://mlr3.mlr-org.com/reference/Prediction.md)).

- measures:

  (list of [Measure](https://mlr3.mlr-org.com/reference/Measure.md)).

- resampling:

  ([Resampling](https://mlr3.mlr-org.com/reference/Resampling.md)).

- resamplings:

  (list of
  [Resampling](https://mlr3.mlr-org.com/reference/Resampling.md)).

- rr:

  ([ResampleResult](https://mlr3.mlr-org.com/reference/ResampleResult.md)).

- bmr:

  ([BenchmarkResult](https://mlr3.mlr-org.com/reference/BenchmarkResult.md)).

- row_ids:

  [`integer()`](https://rdrr.io/r/base/integer.html)  
  Row indices.

- quantile_response:

  (`logical(1)`)  
  Whether to check if the quantile response is set. If `TRUE`, the
  learner must have the `$quantile_response` field set.

- x:

  (any)  
  The input to check.
