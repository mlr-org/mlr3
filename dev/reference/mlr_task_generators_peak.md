# Peak Regression Task Generator

A
[TaskGenerator](https://mlr3.mlr-org.com/dev/reference/TaskGenerator.md)
for the peak task in
[`mlbench::mlbench.peak()`](https://rdrr.io/pkg/mlbench/man/mlbench.peak.html).

## Dictionary

This
[TaskGenerator](https://mlr3.mlr-org.com/dev/reference/TaskGenerator.md)
can be instantiated via the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr_task_generators](https://mlr3.mlr-org.com/dev/reference/mlr_task_generators.md)
or with the associated sugar function
[`tgen()`](https://mlr3.mlr-org.com/dev/reference/mlr_sugar.md):

    mlr_task_generators$get("peak")
    tgen("peak")

## Parameters

|     |         |         |                  |
|-----|---------|---------|------------------|
| Id  | Type    | Default | Range            |
| d   | integer | 20      | \\\[1, \infty)\\ |

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
[`mlr_task_generators_friedman1`](https://mlr3.mlr-org.com/dev/reference/mlr_task_generators_friedman1.md),
[`mlr_task_generators_moons`](https://mlr3.mlr-org.com/dev/reference/mlr_task_generators_moons.md),
[`mlr_task_generators_simplex`](https://mlr3.mlr-org.com/dev/reference/mlr_task_generators_simplex.md),
[`mlr_task_generators_smiley`](https://mlr3.mlr-org.com/dev/reference/mlr_task_generators_smiley.md),
[`mlr_task_generators_spirals`](https://mlr3.mlr-org.com/dev/reference/mlr_task_generators_spirals.md),
[`mlr_task_generators_xor`](https://mlr3.mlr-org.com/dev/reference/mlr_task_generators_xor.md)

## Super class

[`TaskGenerator`](https://mlr3.mlr-org.com/dev/reference/TaskGenerator.md)
-\> `TaskGeneratorPeak`

## Methods

### Public methods

- [`TaskGeneratorPeak$new()`](#method-TaskGeneratorPeak-initialize)

- [`TaskGeneratorPeak$clone()`](#method-TaskGeneratorPeak-clone)

Inherited methods

- [`TaskGenerator$format()`](https://mlr3.mlr-org.com/dev/reference/TaskGenerator.html#method-format)
- [`TaskGenerator$generate()`](https://mlr3.mlr-org.com/dev/reference/TaskGenerator.html#method-generate)
- [`TaskGenerator$print()`](https://mlr3.mlr-org.com/dev/reference/TaskGenerator.html#method-print)

------------------------------------------------------------------------

### `TaskGeneratorPeak$new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    TaskGeneratorPeak$new()

------------------------------------------------------------------------

### `TaskGeneratorPeak$clone()`

The objects of this class are cloneable with this method.

#### Usage

    TaskGeneratorPeak$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
generator = tgen("peak", d = 5)
task = generator$generate(200)
str(task$data())
#> Classes ‘data.table’ and 'data.frame':   200 obs. of  6 variables:
#>  $ y : num  0.556 24.009 4.556 22.178 2.88 ...
#>  $ x1: num  0.2134 -0.06337 -1.09616 -0.00355 -1.18498 ...
#>  $ x2: num  0.77828 -0.03768 0.54845 -0.06676 0.00702 ...
#>  $ x3: num  2.183 -0.138 0.15 0.401 1.113 ...
#>  $ x4: num  0.90203 0.20605 -0.90877 -0.00189 -1.28019 ...
#>  $ x5: num  1.175 -0.118 1.027 0.272 0.197 ...
#>  - attr(*, ".internal.selfref")=<pointer: 0x556fb9879ea0> 
```
