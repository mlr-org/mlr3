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

[`mlr3::TaskGenerator`](https://mlr3.mlr-org.com/dev/reference/TaskGenerator.md)
-\> `TaskGeneratorFriedman1`

## Methods

### Public methods

- [`TaskGeneratorFriedman1$new()`](#method-TaskGeneratorFriedman1-new)

- [`TaskGeneratorFriedman1$clone()`](#method-TaskGeneratorFriedman1-clone)

Inherited methods

- [`mlr3::TaskGenerator$format()`](https://mlr3.mlr-org.com/dev/reference/TaskGenerator.html#method-format)
- [`mlr3::TaskGenerator$generate()`](https://mlr3.mlr-org.com/dev/reference/TaskGenerator.html#method-generate)
- [`mlr3::TaskGenerator$print()`](https://mlr3.mlr-org.com/dev/reference/TaskGenerator.html#method-print)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    TaskGeneratorFriedman1$new()

------------------------------------------------------------------------

### Method `clone()`

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
#>  $ y           : num  16.31 24.87 19.79 7.93 17.69 ...
#>  $ important1  : num  0.765 0.99 0.944 0.611 0.706 ...
#>  $ important2  : num  0.15013 0.31232 0.7915 0.00186 0.08468 ...
#>  $ important3  : num  0.0382 0.989 0.9477 0.2842 0.8083 ...
#>  $ important4  : num  0.686 0.858 0.69 0.77 0.982 ...
#>  $ important5  : num  0.1481 0.6111 0.3696 0.0946 0.9453 ...
#>  $ unimportant1: num  0.465 0.608 0.61 0.757 0.694 ...
#>  $ unimportant2: num  0.712 0.041 0.375 0.34 0.883 ...
#>  $ unimportant3: num  0.0165 0.3787 0.8595 0.6584 0.1895 ...
#>  $ unimportant4: num  0.392 0.647 0.081 0.373 0.786 ...
#>  $ unimportant5: num  0.0198 0.5281 0.9636 0.5646 0.7617 ...
#>  - attr(*, ".internal.selfref")=<externalptr> 
```
