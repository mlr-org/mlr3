# Simplex Classification Task Generator

A
[TaskGenerator](https://mlr3.mlr-org.com/dev/reference/TaskGenerator.md)
for the simplex task in
[`mlbench::mlbench.simplex()`](https://rdrr.io/pkg/mlbench/man/mlbench.simplex.html).

Note that the generator implemented in
[mlbench](https://CRAN.R-project.org/package=mlbench) returns fewer
samples than requested.

## Dictionary

This
[TaskGenerator](https://mlr3.mlr-org.com/dev/reference/TaskGenerator.md)
can be instantiated via the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr_task_generators](https://mlr3.mlr-org.com/dev/reference/mlr_task_generators.md)
or with the associated sugar function
[`tgen()`](https://mlr3.mlr-org.com/dev/reference/mlr_sugar.md):

    mlr_task_generators$get("simplex")
    tgen("simplex")

## Parameters

|        |         |         |             |                  |
|--------|---------|---------|-------------|------------------|
| Id     | Type    | Default | Levels      | Range            |
| center | logical | TRUE    | TRUE, FALSE | \-               |
| d      | integer | 3       |             | \\\[1, \infty)\\ |
| sd     | numeric | 0.1     |             | \\\[0, \infty)\\ |
| sides  | integer | 1       |             | \\\[1, \infty)\\ |

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
[`mlr_task_generators_smiley`](https://mlr3.mlr-org.com/dev/reference/mlr_task_generators_smiley.md),
[`mlr_task_generators_spirals`](https://mlr3.mlr-org.com/dev/reference/mlr_task_generators_spirals.md),
[`mlr_task_generators_xor`](https://mlr3.mlr-org.com/dev/reference/mlr_task_generators_xor.md)

## Super class

[`mlr3::TaskGenerator`](https://mlr3.mlr-org.com/dev/reference/TaskGenerator.md)
-\> `TaskGeneratorSimplex`

## Methods

### Public methods

- [`TaskGeneratorSimplex$new()`](#method-TaskGeneratorSimplex-new)

- [`TaskGeneratorSimplex$plot()`](#method-TaskGeneratorSimplex-plot)

- [`TaskGeneratorSimplex$clone()`](#method-TaskGeneratorSimplex-clone)

Inherited methods

- [`mlr3::TaskGenerator$format()`](https://mlr3.mlr-org.com/dev/reference/TaskGenerator.html#method-format)
- [`mlr3::TaskGenerator$generate()`](https://mlr3.mlr-org.com/dev/reference/TaskGenerator.html#method-generate)
- [`mlr3::TaskGenerator$print()`](https://mlr3.mlr-org.com/dev/reference/TaskGenerator.html#method-print)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    TaskGeneratorSimplex$new()

------------------------------------------------------------------------

### Method [`plot()`](https://rdrr.io/r/graphics/plot.default.html)

Creates a simple plot of generated data.

#### Usage

    TaskGeneratorSimplex$plot(n = 200L, pch = 19L, ...)

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

    TaskGeneratorSimplex$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
generator = tgen("simplex")
plot(generator, n = 200)


task = generator$generate(200)
str(task$data())
#> Classes ‘data.table’ and 'data.frame':   100 obs. of  4 variables:
#>  $ y : Factor w/ 4 levels "A","B","C","D": 1 1 1 1 1 1 1 1 1 1 ...
#>  $ x1: num  -0.269 -0.543 -0.483 -0.556 -0.523 ...
#>  $ x2: num  -0.264 -0.377 -0.244 -0.409 -0.376 ...
#>  $ x3: num  -0.1089 -0.0755 -0.2449 -0.2374 -0.2366 ...
#>  - attr(*, ".internal.selfref")=<externalptr> 
```
