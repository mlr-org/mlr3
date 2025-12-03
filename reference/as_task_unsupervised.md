# Convert to an Unsupervised Task

Convert object to a
[TaskUnsupervised](https://mlr3.mlr-org.com/reference/TaskUnsupervised.md)
or a list of
[TaskUnsupervised](https://mlr3.mlr-org.com/reference/TaskUnsupervised.md).

## Usage

``` r
as_task_unsupervised(x, ...)

# S3 method for class 'Task'
as_task_unsupervised(x, clone = FALSE, ...)

# S3 method for class 'data.frame'
as_task_unsupervised(
  x,
  id = deparse1(substitute(x)),
  label = NA_character_,
  ...
)

# S3 method for class 'DataBackend'
as_task_unsupervised(
  x,
  id = deparse1(substitute(x)),
  label = NA_character_,
  ...
)

as_tasks_unsupervised(x, ...)

# S3 method for class 'list'
as_tasks_unsupervised(x, clone = FALSE, ...)

# S3 method for class 'Task'
as_tasks_unsupervised(x, clone = FALSE, ...)
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

- id:

  (`character(1)`)  
  Id for the new task. Defaults to the (deparsed and substituted) name
  of the data argument.

- label:

  (`character(1)`)  
  Label for the new instance.
