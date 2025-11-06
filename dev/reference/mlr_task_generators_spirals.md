# Spiral Classification Task Generator

A
[TaskGenerator](https://mlr3.mlr-org.com/dev/reference/TaskGenerator.md)
for the spirals task in
[`mlbench::mlbench.spirals()`](https://rdrr.io/pkg/mlbench/man/mlbench.spirals.html).

## Dictionary

This
[TaskGenerator](https://mlr3.mlr-org.com/dev/reference/TaskGenerator.md)
can be instantiated via the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr_task_generators](https://mlr3.mlr-org.com/dev/reference/mlr_task_generators.md)
or with the associated sugar function
[`tgen()`](https://mlr3.mlr-org.com/dev/reference/mlr_sugar.md):

    mlr_task_generators$get("spirals")
    tgen("spirals")

## Parameters

|        |         |         |                  |
|--------|---------|---------|------------------|
| Id     | Type    | Default | Range            |
| cycles | integer | 1       | \\\[1, \infty)\\ |
| sd     | numeric | 0       | \\\[0, \infty)\\ |

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
[`mlr_task_generators_peak`](https://mlr3.mlr-org.com/dev/reference/mlr_task_generators_peak.md),
[`mlr_task_generators_simplex`](https://mlr3.mlr-org.com/dev/reference/mlr_task_generators_simplex.md),
[`mlr_task_generators_smiley`](https://mlr3.mlr-org.com/dev/reference/mlr_task_generators_smiley.md),
[`mlr_task_generators_xor`](https://mlr3.mlr-org.com/dev/reference/mlr_task_generators_xor.md)

## Super class

[`mlr3::TaskGenerator`](https://mlr3.mlr-org.com/dev/reference/TaskGenerator.md)
-\> `TaskGeneratorSpirals`

## Methods

### Public methods

- [`TaskGeneratorSpirals$new()`](#method-TaskGeneratorSpirals-new)

- [`TaskGeneratorSpirals$plot()`](#method-TaskGeneratorSpirals-plot)

- [`TaskGeneratorSpirals$clone()`](#method-TaskGeneratorSpirals-clone)

Inherited methods

- [`mlr3::TaskGenerator$format()`](https://mlr3.mlr-org.com/dev/reference/TaskGenerator.html#method-format)
- [`mlr3::TaskGenerator$generate()`](https://mlr3.mlr-org.com/dev/reference/TaskGenerator.html#method-generate)
- [`mlr3::TaskGenerator$print()`](https://mlr3.mlr-org.com/dev/reference/TaskGenerator.html#method-print)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    TaskGeneratorSpirals$new()

------------------------------------------------------------------------

### Method [`plot()`](https://rdrr.io/r/graphics/plot.default.html)

Creates a simple plot of generated data.

#### Usage

    TaskGeneratorSpirals$plot(n = 200L, pch = 19L, ...)

#### Arguments

- `n`:

  (`integer(1)`)  
  Number of samples to draw for the plot. Default is 200.

- `pch`:

  (`integer(1)`)  
  Point char. Passed to
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html).

- `...`:

  (any)  
  Additional arguments passed to
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html).

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    TaskGeneratorSpirals$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
generator = tgen("spirals")
plot(generator, n = 200)


task = generator$generate(200)
str(task$data())
#> Classes ‘data.table’ and 'data.frame':   200 obs. of  3 variables:
#>  $ y : Factor w/ 2 levels "A","B": 1 2 2 1 1 2 1 1 2 1 ...
#>  $ x1: num  0.333 -0.324 -0.16 0.339 0.344 ...
#>  $ x2: num  0 -0.2351 0.8382 0.0213 0.0434 ...
#>  - attr(*, ".internal.selfref")=<externalptr> 
```
