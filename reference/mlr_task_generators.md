# Dictionary of Task Generators

A simple
[mlr3misc::Dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
storing objects of class
[TaskGenerator](https://mlr3.mlr-org.com/reference/TaskGenerator.md).
Each task generator has an associated help page, see
`mlr_task_generators_[id]`.

This dictionary can get populated with additional task generators by
add-on packages.

For a more convenient way to retrieve and construct task generators, see
[`tgen()`](https://mlr3.mlr-org.com/reference/mlr_sugar.md)/[`tgens()`](https://mlr3.mlr-org.com/reference/mlr_sugar.md).

## Format

[R6::R6Class](https://r6.r-lib.org/reference/R6Class.html) object
inheriting from
[mlr3misc::Dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html).

## Methods

See
[mlr3misc::Dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html).

## S3 methods

- `as.data.table(dict, ..., objects = FALSE)`  
  [mlr3misc::Dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
  -\>
  [`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html)  
  Returns a
  [`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html)
  with fields "key", "label", "task_type", "params", and "packages" as
  columns. If `objects` is set to `TRUE`, the constructed objects are
  returned in the list column named `object`.

## See also

Sugar functions:
[`tgen()`](https://mlr3.mlr-org.com/reference/mlr_sugar.md),
[`tgens()`](https://mlr3.mlr-org.com/reference/mlr_sugar.md)

Other Dictionary:
[`mlr_learners`](https://mlr3.mlr-org.com/reference/mlr_learners.md),
[`mlr_measures`](https://mlr3.mlr-org.com/reference/mlr_measures.md),
[`mlr_resamplings`](https://mlr3.mlr-org.com/reference/mlr_resamplings.md),
[`mlr_tasks`](https://mlr3.mlr-org.com/reference/mlr_tasks.md)

Other TaskGenerator:
[`TaskGenerator`](https://mlr3.mlr-org.com/reference/TaskGenerator.md),
[`mlr_task_generators_2dnormals`](https://mlr3.mlr-org.com/reference/mlr_task_generators_2dnormals.md),
[`mlr_task_generators_cassini`](https://mlr3.mlr-org.com/reference/mlr_task_generators_cassini.md),
[`mlr_task_generators_circle`](https://mlr3.mlr-org.com/reference/mlr_task_generators_circle.md),
[`mlr_task_generators_friedman1`](https://mlr3.mlr-org.com/reference/mlr_task_generators_friedman1.md),
[`mlr_task_generators_moons`](https://mlr3.mlr-org.com/reference/mlr_task_generators_moons.md),
[`mlr_task_generators_peak`](https://mlr3.mlr-org.com/reference/mlr_task_generators_peak.md),
[`mlr_task_generators_simplex`](https://mlr3.mlr-org.com/reference/mlr_task_generators_simplex.md),
[`mlr_task_generators_smiley`](https://mlr3.mlr-org.com/reference/mlr_task_generators_smiley.md),
[`mlr_task_generators_spirals`](https://mlr3.mlr-org.com/reference/mlr_task_generators_spirals.md),
[`mlr_task_generators_xor`](https://mlr3.mlr-org.com/reference/mlr_task_generators_xor.md)

## Examples

``` r
mlr_task_generators$get("smiley")
#> 
#> ── <TaskGeneratorSmiley> (smiley): Smiley Classification ───────────────────────
#> • Task type: classif
#> • Packages: mlr3 and mlbench
#> • Parameters: list()
#> • Manual: `?mlr3::mlr_task_generators_smiley()`
tgen("2dnormals")
#> 
#> ── <TaskGenerator2DNormals> (2dnormals): 2D Normals Classification ─────────────
#> • Task type: classif
#> • Packages: mlr3 and mlbench
#> • Parameters: list()
#> • Manual: `?mlr3::mlr_task_generators_2dnormals()`
```
