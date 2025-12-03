# Convert a Task from One Type to Another

The task's target is replaced by a different column from the data.

## Usage

``` r
convert_task(
  intask,
  target = NULL,
  new_type = NULL,
  drop_original_target = FALSE,
  drop_levels = TRUE
)
```

## Arguments

- intask:

  ([Task](https://mlr3.mlr-org.com/reference/Task.md))  
  A [Task](https://mlr3.mlr-org.com/reference/Task.md) to be converted.

- target:

  (`character(1)`)  
  New target to be set, must be a column in the `intask` data. If
  `NULL`, no new target is set, and task is converted as-is.

- new_type:

  (`character(1)`)  
  The new task type. Must be in
  [`mlr_reflections$task_types`](https://mlr3.mlr-org.com/reference/mlr_reflections.md)\].
  If `NULL` (default), a new task with the same task_type is created.

- drop_original_target:

  (`logical(1)`)  
  If `FALSE` (default), the original target is added as a feature.
  Otherwise the original target is dropped.

- drop_levels:

  (`logical(1)`)  
  If `TRUE` (default), unused levels of the new target variable are
  dropped.

## Value

[Task](https://mlr3.mlr-org.com/reference/Task.md) of requested type.
