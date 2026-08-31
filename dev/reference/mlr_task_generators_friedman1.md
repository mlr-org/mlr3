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
#>  $ y           : num  14.8 10.6 12.3 10.7 14.3 ...
#>  $ important1  : num  0.381 0.781 0.857 0.557 0.479 ...
#>  $ important2  : num  0.384 0.315 0.222 0.341 0.19 ...
#>  $ important3  : num  0.214 0.626 0.407 0.332 0.6 ...
#>  $ important4  : num  0.872 0.191 0.301 0.139 0.803 ...
#>  $ important5  : num  0.0183 0.3933 0.6797 0.3991 0.8944 ...
#>  $ unimportant1: num  0.632 0.4958 0.8567 0.5099 0.0483 ...
#>  $ unimportant2: num  0.951 0.704 0.338 0.136 0.918 ...
#>  $ unimportant3: num  0.97 0.283 0.157 0.812 0.848 ...
#>  $ unimportant4: num  0.472 0.166 0.945 0.447 0.309 ...
#>  $ unimportant5: num  0.5638 0.2516 0.0248 0.608 0.2025 ...
#>  - attr(*, ".internal.selfref")=<pointer: 0x557fe55dcee0> 
```
