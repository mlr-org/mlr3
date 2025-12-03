# Convert to a Classification Task

Convert object to a
[TaskClassif](https://mlr3.mlr-org.com/reference/TaskClassif.md). This
is a S3 generic. mlr3 ships with methods for the following objects:

1.  [TaskClassif](https://mlr3.mlr-org.com/reference/TaskClassif.md):
    returns the object as-is, possibly cloned.

2.  [`formula`](https://rdrr.io/r/stats/formula.html),
    [`data.frame()`](https://rdrr.io/r/base/data.frame.html),
    [`matrix()`](https://rdrr.io/r/base/matrix.html), and
    [DataBackend](https://mlr3.mlr-org.com/reference/DataBackend.md):
    provides an alternative to the constructor of
    [TaskClassif](https://mlr3.mlr-org.com/reference/TaskClassif.md).

3.  [TaskRegr](https://mlr3.mlr-org.com/reference/TaskRegr.md): Calls
    [`convert_task()`](https://mlr3.mlr-org.com/reference/convert_task.md).

Note that the target column will be converted to a
[`factor()`](https://rdrr.io/r/base/factor.html), if possible.

## Usage

``` r
as_task_classif(x, ...)

# S3 method for class 'TaskClassif'
as_task_classif(x, clone = FALSE, ...)

# S3 method for class 'data.frame'
as_task_classif(
  x,
  target,
  id = deparse1(substitute(x)),
  positive = NULL,
  label = NA_character_,
  ...
)

# S3 method for class 'matrix'
as_task_classif(
  x,
  target,
  id = deparse1(substitute(x)),
  label = NA_character_,
  ...
)

# S3 method for class 'DataBackend'
as_task_classif(
  x,
  target,
  id = deparse1(substitute(x)),
  positive = NULL,
  label = NA_character_,
  ...
)

# S3 method for class 'TaskRegr'
as_task_classif(
  x,
  target,
  drop_original_target = FALSE,
  drop_levels = TRUE,
  ...
)

# S3 method for class 'formula'
as_task_classif(
  x,
  data,
  id = deparse1(substitute(data)),
  positive = NULL,
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

- positive:

  (`character(1)`)  
  Level of the positive class. See
  [TaskClassif](https://mlr3.mlr-org.com/reference/TaskClassif.md).

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

[TaskClassif](https://mlr3.mlr-org.com/reference/TaskClassif.md).

## Examples

``` r
as_task_classif(palmerpenguins::penguins, target = "species")
#> 
#> ── <TaskClassif> (344x8) ───────────────────────────────────────────────────────
#> • Target: species
#> • Target classes: Adelie (44%), Gentoo (36%), Chinstrap (20%)
#> • Properties: multiclass
#> • Features (7):
#>   • int (3): body_mass_g, flipper_length_mm, year
#>   • dbl (2): bill_depth_mm, bill_length_mm
#>   • fct (2): island, sex
```
