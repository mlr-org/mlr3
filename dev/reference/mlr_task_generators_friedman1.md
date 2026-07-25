# Friedman1 Regression Task Generator

A
[TaskGenerator](https://mlr3.mlr-org.com/dev/reference/TaskGenerator.md)
for the friedman1 task in
[`mlbench::mlbench.friedman1()`](https://rdrr.io/pkg/mlbench/man/mlbench.friedman1.html).

## Dictionary

This
[TaskGenerator](https://mlr3.mlr-org.com/dev/reference/TaskGenerator.md)
can be instantiated via the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr_task_generators](https://mlr3.mlr-org.com/dev/reference/mlr_task_generators.md)
or with the associated sugar function
[`tgen()`](https://mlr3.mlr-org.com/dev/reference/mlr_sugar.md):

    mlr_task_generators$get("friedman1")
    tgen("friedman1")

## Parameters

|     |         |         |                  |
|-----|---------|---------|------------------|
| Id  | Type    | Default | Range            |
| sd  | numeric | 1       | \\\[0, \infty)\\ |

## See also

- [Dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
  of
  [TaskGenerators](https://mlr3.mlr-org.com/dev/reference/TaskGenerator.md):
  [mlr_task_generators](https://mlr3.mlr-org.com/dev/reference/mlr_task_generators.md)

- `as.data.table(mlr_task_generators)` for a table of available
  [TaskGenerators](https://mlr3.mlr-org.com/dev/reference/TaskGenerator.md)
  in the running session (depending on the loaded packages).

- Extension packages for additional task types:

  - [mlr3proba](https://CRAN.R-project.org/package=mlr3proba) for
    probabilistic supervised regression and survival analysis.

  - [mlr3cluster](https://CRAN.R-project.org/package=mlr3cluster) for
    unsupervised clustering.

Other TaskGenerator:
[`TaskGenerator`](https://mlr3.mlr-org.com/dev/reference/TaskGenerator.md),
[`mlr_task_generators`](https://mlr3.mlr-org.com/dev/reference/mlr_task_generators.md),
[`mlr_task_generators_2dnormals`](https://mlr3.mlr-org.com/dev/reference/mlr_task_generators_2dnormals.md),
[`mlr_task_generators_cassini`](https://mlr3.mlr-org.com/dev/reference/mlr_task_generators_cassini.md),
[`mlr_task_generators_circle`](https://mlr3.mlr-org.com/dev/reference/mlr_task_generators_circle.md),
[`mlr_task_generators_moons`](https://mlr3.mlr-org.com/dev/reference/mlr_task_generators_moons.md),
[`mlr_task_generators_peak`](https://mlr3.mlr-org.com/dev/reference/mlr_task_generators_peak.md),
[`mlr_task_generators_simplex`](https://mlr3.mlr-org.com/dev/reference/mlr_task_generators_simplex.md),
[`mlr_task_generators_smiley`](https://mlr3.mlr-org.com/dev/reference/mlr_task_generators_smiley.md),
[`mlr_task_generators_spirals`](https://mlr3.mlr-org.com/dev/reference/mlr_task_generators_spirals.md),
[`mlr_task_generators_xor`](https://mlr3.mlr-org.com/dev/reference/mlr_task_generators_xor.md)

## Super class

[`TaskGenerator`](https://mlr3.mlr-org.com/dev/reference/TaskGenerator.md)
-\> `TaskGeneratorFriedman1`

## Methods

### Public methods

- [`TaskGeneratorFriedman1$new()`](#method-TaskGeneratorFriedman1-initialize)

- [`TaskGeneratorFriedman1$clone()`](#method-TaskGeneratorFriedman1-clone)

Inherited methods

- [`TaskGenerator$format()`](https://mlr3.mlr-org.com/dev/reference/TaskGenerator.html#method-format)
- [`TaskGenerator$generate()`](https://mlr3.mlr-org.com/dev/reference/TaskGenerator.html#method-generate)
- [`TaskGenerator$print()`](https://mlr3.mlr-org.com/dev/reference/TaskGenerator.html#method-print)

------------------------------------------------------------------------

### `TaskGeneratorFriedman1$new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    TaskGeneratorFriedman1$new()

------------------------------------------------------------------------

### `TaskGeneratorFriedman1$clone()`

The objects of this class are cloneable with this method.

#### Usage

    TaskGeneratorFriedman1$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
generator = tgen("friedman1")
task = generator$generate(200)
str(task$data())
#> Classes ‘data.table’ and 'data.frame':   200 obs. of  11 variables:
#>  $ y           : num  9.27 20.83 18.4 11.69 11.16 ...
#>  $ important1  : num  0.16 0.985 0.975 0.283 0.759 ...
#>  $ important2  : num  0.4925 0.626 0.3333 0.0744 0.3592 ...
#>  $ important3  : num  0.781 0.953 0.283 0.578 0.479 ...
#>  $ important4  : num  0.0694 0.4687 0.6799 0.7998 0.1656 ...
#>  $ important5  : num  0.8134 0.6392 0.5229 0.6678 0.0114 ...
#>  $ unimportant1: num  0.717 0.731 0.488 0.227 0.716 ...
#>  $ unimportant2: num  0.763 0.781 0.7 0.215 0.532 ...
#>  $ unimportant3: num  0.0907 0.7095 0.6861 0.7796 0.9294 ...
#>  $ unimportant4: num  0.488 0.892 0.73 0.718 0.442 ...
#>  $ unimportant5: num  0.4032 0.6818 0.3526 0.3242 0.0449 ...
#>  - attr(*, ".internal.selfref")=<pointer: 0x556fb9879ea0> 
```
