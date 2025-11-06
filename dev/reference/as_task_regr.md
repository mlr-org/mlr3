# Convert to a Regression Task

Convert object to a
[TaskRegr](https://mlr3.mlr-org.com/dev/reference/TaskRegr.md). This is
a S3 generic. mlr3 ships with methods for the following objects:

1.  [TaskRegr](https://mlr3.mlr-org.com/dev/reference/TaskRegr.md):
    returns the object as-is, possibly cloned.

2.  [`formula`](https://rdrr.io/r/stats/formula.html),
    [`data.frame()`](https://rdrr.io/r/base/data.frame.html),
    [`matrix()`](https://rdrr.io/r/base/matrix.html), and
    [DataBackend](https://mlr3.mlr-org.com/dev/reference/DataBackend.md):
    provides an alternative to the constructor of
    [TaskRegr](https://mlr3.mlr-org.com/dev/reference/TaskRegr.md).

3.  [TaskClassif](https://mlr3.mlr-org.com/dev/reference/TaskClassif.md):
    Calls
    [`convert_task()`](https://mlr3.mlr-org.com/dev/reference/convert_task.md).

## Usage

``` r
as_task_regr(x, ...)

# S3 method for class 'TaskRegr'
as_task_regr(x, clone = FALSE, ...)

# S3 method for class 'data.frame'
as_task_regr(
  x,
  target,
  id = deparse1(substitute(x)),
  label = NA_character_,
  ...
)

# S3 method for class 'matrix'
as_task_regr(
  x,
  target,
  id = deparse1(substitute(x)),
  label = NA_character_,
  ...
)

# S3 method for class 'DataBackend'
as_task_regr(
  x,
  target,
  id = deparse1(substitute(x)),
  label = NA_character_,
  ...
)

# S3 method for class 'TaskClassif'
as_task_regr(x, target, drop_original_target = FALSE, drop_levels = TRUE, ...)

# S3 method for class 'formula'
as_task_regr(
  x,
  data,
  id = deparse1(substitute(data)),
  label = NA_character_,
  ...
)
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

- target:

  (`character(1)`)  
  Name of the target column.

- id:

  (`character(1)`)  
  Id for the new task. Defaults to the (deparsed and substituted) name
  of the data argument.

- label:

  (`character(1)`)  
  Label for the new instance.

- drop_original_target:

  (`logical(1)`)  
  If `FALSE` (default), the original target is added as a feature.
  Otherwise the original target is dropped.

- drop_levels:

  (`logical(1)`)  
  If `TRUE` (default), unused levels of the new target variable are
  dropped.

- data:

  ([`data.frame()`](https://rdrr.io/r/base/data.frame.html))  
  Data frame containing all columns referenced in formula `x`.

## Value

[TaskRegr](https://mlr3.mlr-org.com/dev/reference/TaskRegr.md).

## Examples

``` r
as_task_regr(datasets::mtcars, target = "mpg")
#> 
#> ── <TaskRegr> (32x11) ──────────────────────────────────────────────────────────
#> • Target: mpg
#> • Properties: -
#> • Features (10):
#>   • dbl (10): am, carb, cyl, disp, drat, gear, hp, qsec, vs, wt
```
