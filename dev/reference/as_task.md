# Convert to a Task

Convert object to a
[Task](https://mlr3.mlr-org.com/dev/reference/Task.md) or a list of
[Task](https://mlr3.mlr-org.com/dev/reference/Task.md).

The function supports:

- Converting existing
  [Task](https://mlr3.mlr-org.com/dev/reference/Task.md) objects (with
  optional cloning)

- Converting objects from other packages (e.g., `OMLTask` from
  [mlr3oml](https://CRAN.R-project.org/package=mlr3oml))

- Converting lists of objects to lists of tasks

For constructing tasks from data frames, use the dedicated converters:

- [`as_task_classif()`](https://mlr3.mlr-org.com/dev/reference/as_task_classif.md)
  for classification tasks

- [`as_task_regr()`](https://mlr3.mlr-org.com/dev/reference/as_task_regr.md)
  for regression tasks

- [`as_task_unsupervised()`](https://mlr3.mlr-org.com/dev/reference/as_task_unsupervised.md)
  for unsupervised tasks

## Usage

``` r
as_task(x, ...)

# S3 method for class 'Task'
as_task(x, clone = FALSE, ...)

as_tasks(x, ...)

# Default S3 method
as_tasks(x, ...)

# S3 method for class 'list'
as_tasks(x, ...)
```

## Arguments

- x:

  (any)  
  Object to convert.

- ...:

  (any)  
  Additional arguments.

- clone:

  (`logical(1)`)  
  If `TRUE`, ensures that the returned object is not the same as the
  input `x`.
