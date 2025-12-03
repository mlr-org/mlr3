# Friedman1 Regression Task Generator

A [TaskGenerator](https://mlr3.mlr-org.com/reference/TaskGenerator.md)
for the friedman1 task in
[`mlbench::mlbench.friedman1()`](https://rdrr.io/pkg/mlbench/man/mlbench.friedman1.html).

## Dictionary

This
[TaskGenerator](https://mlr3.mlr-org.com/reference/TaskGenerator.md) can
be instantiated via the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr_task_generators](https://mlr3.mlr-org.com/reference/mlr_task_generators.md)
or with the associated sugar function
[`tgen()`](https://mlr3.mlr-org.com/reference/mlr_sugar.md):

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
[`mlr_task_generators_moons`](https://mlr3.mlr-org.com/reference/mlr_task_generators_moons.md),
[`mlr_task_generators_peak`](https://mlr3.mlr-org.com/reference/mlr_task_generators_peak.md),
[`mlr_task_generators_simplex`](https://mlr3.mlr-org.com/reference/mlr_task_generators_simplex.md),
[`mlr_task_generators_smiley`](https://mlr3.mlr-org.com/reference/mlr_task_generators_smiley.md),
[`mlr_task_generators_spirals`](https://mlr3.mlr-org.com/reference/mlr_task_generators_spirals.md),
[`mlr_task_generators_xor`](https://mlr3.mlr-org.com/reference/mlr_task_generators_xor.md)

## Super class

[`mlr3::TaskGenerator`](https://mlr3.mlr-org.com/reference/TaskGenerator.md)
-\> `TaskGeneratorFriedman1`

## Methods

### Public methods

- [`TaskGeneratorFriedman1$new()`](#method-TaskGeneratorFriedman1-new)

- [`TaskGeneratorFriedman1$clone()`](#method-TaskGeneratorFriedman1-clone)

Inherited methods

- [`mlr3::TaskGenerator$format()`](https://mlr3.mlr-org.com/reference/TaskGenerator.html#method-format)
- [`mlr3::TaskGenerator$generate()`](https://mlr3.mlr-org.com/reference/TaskGenerator.html#method-generate)
- [`mlr3::TaskGenerator$print()`](https://mlr3.mlr-org.com/reference/TaskGenerator.html#method-print)

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
#>  $ y           : num  5.32 16.53 21.87 5.97 14.48 ...
#>  $ important1  : num  0.6182 0.7875 0.6566 0.0142 0.5908 ...
#>  $ important2  : num  0.118 0.631 0.5 0.312 0.398 ...
#>  $ important3  : num  0.671 0.242 0.293 0.32 0.479 ...
#>  $ important4  : num  0.16 0.367 0.821 0.426 0.537 ...
#>  $ important5  : num  0.121 0.388 0.852 0.208 0.454 ...
#>  $ unimportant1: num  0.507 0.653 0.556 0.444 0.865 ...
#>  $ unimportant2: num  0.585 0.804 0.723 0.061 0.111 ...
#>  $ unimportant3: num  0.335 0.299 0.883 0.715 0.186 ...
#>  $ unimportant4: num  0.104 0.446 0.75 0.272 0.321 ...
#>  $ unimportant5: num  0.583 0.88 0.624 0.197 0.222 ...
#>  - attr(*, ".internal.selfref")=<externalptr> 
```
