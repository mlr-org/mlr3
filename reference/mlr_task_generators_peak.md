# Peak Regression Task Generator

A [TaskGenerator](https://mlr3.mlr-org.com/reference/TaskGenerator.md)
for the peak task in
[`mlbench::mlbench.peak()`](https://rdrr.io/pkg/mlbench/man/mlbench.peak.html).

## Dictionary

This
[TaskGenerator](https://mlr3.mlr-org.com/reference/TaskGenerator.md) can
be instantiated via the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr_task_generators](https://mlr3.mlr-org.com/reference/mlr_task_generators.md)
or with the associated sugar function
[`tgen()`](https://mlr3.mlr-org.com/reference/mlr_sugar.md):

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
  [TaskGenerators](https://mlr3.mlr-org.com/reference/TaskGenerator.md):
  [mlr_task_generators](https://mlr3.mlr-org.com/reference/mlr_task_generators.md)

- `as.data.table(mlr_task_generators)` for a table of available
  [TaskGenerators](https://mlr3.mlr-org.com/reference/TaskGenerator.md)
  in the running session (depending on the loaded packages).

- Extension packages for additional task types:

  - [mlr3proba](https://CRAN.R-project.org/package=mlr3proba) for
    probabilistic supervised regression and survival analysis.

  - [mlr3cluster](https://CRAN.R-project.org/package=mlr3cluster) for
    unsupervised clustering.

Other TaskGenerator:
[`TaskGenerator`](https://mlr3.mlr-org.com/reference/TaskGenerator.md),
[`mlr_task_generators`](https://mlr3.mlr-org.com/reference/mlr_task_generators.md),
[`mlr_task_generators_2dnormals`](https://mlr3.mlr-org.com/reference/mlr_task_generators_2dnormals.md),
[`mlr_task_generators_cassini`](https://mlr3.mlr-org.com/reference/mlr_task_generators_cassini.md),
[`mlr_task_generators_circle`](https://mlr3.mlr-org.com/reference/mlr_task_generators_circle.md),
[`mlr_task_generators_friedman1`](https://mlr3.mlr-org.com/reference/mlr_task_generators_friedman1.md),
[`mlr_task_generators_moons`](https://mlr3.mlr-org.com/reference/mlr_task_generators_moons.md),
[`mlr_task_generators_simplex`](https://mlr3.mlr-org.com/reference/mlr_task_generators_simplex.md),
[`mlr_task_generators_smiley`](https://mlr3.mlr-org.com/reference/mlr_task_generators_smiley.md),
[`mlr_task_generators_spirals`](https://mlr3.mlr-org.com/reference/mlr_task_generators_spirals.md),
[`mlr_task_generators_xor`](https://mlr3.mlr-org.com/reference/mlr_task_generators_xor.md)

## Super class

[`mlr3::TaskGenerator`](https://mlr3.mlr-org.com/reference/TaskGenerator.md)
-\> `TaskGeneratorPeak`

## Methods

### Public methods

- [`TaskGeneratorPeak$new()`](#method-TaskGeneratorPeak-new)

- [`TaskGeneratorPeak$clone()`](#method-TaskGeneratorPeak-clone)

Inherited methods

- [`mlr3::TaskGenerator$format()`](https://mlr3.mlr-org.com/reference/TaskGenerator.html#method-format)
- [`mlr3::TaskGenerator$generate()`](https://mlr3.mlr-org.com/reference/TaskGenerator.html#method-generate)
- [`mlr3::TaskGenerator$print()`](https://mlr3.mlr-org.com/reference/TaskGenerator.html#method-print)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    TaskGeneratorPeak$new()

------------------------------------------------------------------------

### Method `clone()`

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
#>  $ y : num  4.89 5.75 6.96 24.04 4.72 ...
#>  $ x1: num  -1.3536 0.2737 0.2287 -0.0232 -1.3182 ...
#>  $ x2: num  -0.8278 1.1552 -0.4408 0.0973 1.2019 ...
#>  $ x3: num  0.336 -0.717 -0.949 -0.235 0.311 ...
#>  $ x4: num  0.39346 0.7602 1.10251 0.00276 0.09836 ...
#>  $ x5: num  -0.693 0.662 0.441 -0.115 0.214 ...
#>  - attr(*, ".internal.selfref")=<externalptr> 
```
