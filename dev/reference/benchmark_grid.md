# Generate a Benchmark Grid Design

Takes a lists of [Task](https://mlr3.mlr-org.com/dev/reference/Task.md),
a list of [Learner](https://mlr3.mlr-org.com/dev/reference/Learner.md)
and a list of
[Resampling](https://mlr3.mlr-org.com/dev/reference/Resampling.md) to
generate a design in an
[`expand.grid()`](https://rdrr.io/r/base/expand.grid.html) fashion
(a.k.a. cross join or Cartesian product).

There are two modes of operation, depending on the flag `paired`.

- With `paired` set to `FALSE` (default), resampling strategies are not
  allowed to be instantiated, and instead will be instantiated per task
  internally. The only exception to this rule applies if all tasks have
  exactly the same row ids, and the resamplings are all instantiated for
  such tasks. The grid will be generated based on the Cartesian product
  of tasks, learners, and resamplings. Because the resamplings are
  instantiated on the tasks, reproducibility requires a seed to be set
  **before** calling this function, as this process is stochastic.

- With `paired` set to `TRUE`, tasks and resamplings are treated as
  pairs. This means that you must provide as many tasks as corresponding
  instantiated resamplings. The grid will be generated based on the
  Cartesian product of learners and pairs.

## Usage

``` r
benchmark_grid(
  tasks,
  learners,
  resamplings,
  param_values = NULL,
  paired = FALSE
)
```

## Arguments

- tasks:

  (list of [Task](https://mlr3.mlr-org.com/dev/reference/Task.md)).

- learners:

  (list of
  [Learner](https://mlr3.mlr-org.com/dev/reference/Learner.md)).

- resamplings:

  (list of
  [Resampling](https://mlr3.mlr-org.com/dev/reference/Resampling.md)).

- param_values:

  ([`list()`](https://rdrr.io/r/base/list.html) \| `NULL`)  
  If you want to try many parameter settings for learners, you can pass
  them through the design which is optimized to be faster than creating
  learners for each setting.

  A list of lists of named lists, from outer to inner:

  1.  One list element for each
      [Learner](https://mlr3.mlr-org.com/dev/reference/Learner.md).

  2.  One list element for each hyperparameter configuration to try.

  3.  Named list of hyperparameter settings to set in the Learner,
      possibly overwriting already set set hyperparameters in the
      [Learner](https://mlr3.mlr-org.com/dev/reference/Learner.md).

- paired:

  (`logical(1)`)  
  Set this to `TRUE` if the resamplings are instantiated on the tasks,
  i.e., the tasks and resamplings are paired. You need to provide the
  same number of tasks and instantiated resamplings.

## Value

([`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html))
with the cross product of the input vectors.

## Errors and Warnings

- `Mlr3WarningVaryingPredictTypes`: This warning will be thrown if the
  learners have different `predict_type`s.

## See also

- Chapter in the [mlr3book](https://mlr3book.mlr-org.com/):
  <https://mlr3book.mlr-org.com/chapters/chapter3/evaluation_and_benchmarking.html#sec-benchmarking>

- Package [mlr3viz](https://CRAN.R-project.org/package=mlr3viz) for some
  generic visualizations.

- [mlr3benchmark](https://CRAN.R-project.org/package=mlr3benchmark) for
  post-hoc analysis of benchmark results.

Other benchmark:
[`BenchmarkResult`](https://mlr3.mlr-org.com/dev/reference/BenchmarkResult.md),
[`benchmark()`](https://mlr3.mlr-org.com/dev/reference/benchmark.md)

## Examples

``` r
tasks = list(tsk("penguins"), tsk("sonar"))
learners = list(lrn("classif.featureless"), lrn("classif.rpart"))
resamplings = list(rsmp("cv"), rsmp("subsampling"))

# Set a seed to ensure reproducibility of the resampling instantiation
set.seed(123)
grid = benchmark_grid(tasks, learners, resamplings)
# the resamplings are now instantiated
head(grid$resampling[[1]]$instance)
#> Key: <fold>
#>    row_id  fold
#>     <int> <int>
#> 1:     12     1
#> 2:     22     1
#> 3:     24     1
#> 4:     42     1
#> 5:     47     1
#> 6:     69     1
print(grid)
#>        task             learner  resampling
#>      <char>              <char>      <char>
#> 1: penguins classif.featureless          cv
#> 2: penguins classif.featureless subsampling
#> 3: penguins       classif.rpart          cv
#> 4: penguins       classif.rpart subsampling
#> 5:    sonar classif.featureless          cv
#> 6:    sonar classif.featureless subsampling
#> 7:    sonar       classif.rpart          cv
#> 8:    sonar       classif.rpart subsampling
if (FALSE) { # \dontrun{
benchmark(grid)
} # }

# paired
learner = lrn("classif.rpart")
task1 = tsk("penguins")
task2 = tsk("german_credit")
res1 = rsmp("holdout")
res2 = rsmp("holdout")
res1$instantiate(task1)
res2$instantiate(task2)
design = benchmark_grid(list(task1, task2), learner, list(res1, res2), paired = TRUE)
print(design)
#>             task       learner resampling
#>           <char>        <char>     <char>
#> 1:      penguins classif.rpart    holdout
#> 2: german_credit classif.rpart    holdout

# manual construction of the grid with data.table::CJ()
grid = data.table::CJ(
 task = tasks,
  learner = learners,
  resampling = resamplings,
  sorted = FALSE
)

# manual instantiation (not suited for a fair comparison of learners!)
Map(function(task, resampling) {
  resampling$instantiate(task)
}, task = grid$task, resampling = grid$resampling)
#> [[1]]
#> 
#> ── <ResamplingCV> : Cross-Validation ───────────────────────────────────────────
#> • Iterations: 10
#> • Instantiated: TRUE
#> • Parameters: folds=10
#> 
#> [[2]]
#> 
#> ── <ResamplingSubsampling> : Subsampling ───────────────────────────────────────
#> • Iterations: 30
#> • Instantiated: TRUE
#> • Parameters: ratio=0.6667, repeats=30
#> 
#> [[3]]
#> 
#> ── <ResamplingCV> : Cross-Validation ───────────────────────────────────────────
#> • Iterations: 10
#> • Instantiated: TRUE
#> • Parameters: folds=10
#> 
#> [[4]]
#> 
#> ── <ResamplingSubsampling> : Subsampling ───────────────────────────────────────
#> • Iterations: 30
#> • Instantiated: TRUE
#> • Parameters: ratio=0.6667, repeats=30
#> 
#> [[5]]
#> 
#> ── <ResamplingCV> : Cross-Validation ───────────────────────────────────────────
#> • Iterations: 10
#> • Instantiated: TRUE
#> • Parameters: folds=10
#> 
#> [[6]]
#> 
#> ── <ResamplingSubsampling> : Subsampling ───────────────────────────────────────
#> • Iterations: 30
#> • Instantiated: TRUE
#> • Parameters: ratio=0.6667, repeats=30
#> 
#> [[7]]
#> 
#> ── <ResamplingCV> : Cross-Validation ───────────────────────────────────────────
#> • Iterations: 10
#> • Instantiated: TRUE
#> • Parameters: folds=10
#> 
#> [[8]]
#> 
#> ── <ResamplingSubsampling> : Subsampling ───────────────────────────────────────
#> • Iterations: 30
#> • Instantiated: TRUE
#> • Parameters: ratio=0.6667, repeats=30
#> 
if (FALSE) { # \dontrun{
benchmark(grid)
} # }
```
