# Obtain specific uhashes from a [BenchmarkResult](https://mlr3.mlr-org.com/reference/BenchmarkResult.md)

In a
[`BenchmarkResult`](https://mlr3.mlr-org.com/reference/BenchmarkResult.md),
each
[ResampleResult](https://mlr3.mlr-org.com/reference/ResampleResult.md)
is *u*niquely identified by a *hash* (*uhash*). Operations that select
specific
[ResampleResult](https://mlr3.mlr-org.com/reference/ResampleResult.md)s
from a
[BenchmarkResult](https://mlr3.mlr-org.com/reference/BenchmarkResult.md)
operate using these hashes. This function allows to obtain uhashes for
specific learners, tasks, and resamplings.

If you want more control, you can also directly obtain the uhash table
from the
[`BenchmarkResult`](https://mlr3.mlr-org.com/reference/BenchmarkResult.md)
via the field `$uhash_table`.

## Usage

``` r
uhashes(bmr, learner_ids = NULL, task_ids = NULL, resampling_ids = NULL)

uhash(bmr, learner_id = NULL, task_id = NULL, resampling_id = NULL)
```

## Arguments

- bmr:

  (`BenchmarkResult`)  
  Benchmark result.

- learner_ids:

  ([`character()`](https://rdrr.io/r/base/character.html) \| `NULL`)  
  Learner IDs.

- task_ids:

  ([`character()`](https://rdrr.io/r/base/character.html) \| `NULL`)  
  Task IDs.

- resampling_ids:

  ([`character()`](https://rdrr.io/r/base/character.html) \| `NULL`)  
  Resampling IDs.

- learner_id:

  (`character(1)` \| `NULL`)  
  Learner ID.

- task_id:

  (`character(1)` \| `NULL`)  
  Task ID.

- resampling_id:

  (`character(1)` \| `NULL`)  
  Resampling ID.

## Examples

``` r
design = benchmark_grid(
  tsks(c("sonar", "iris")),
  lrns(c("classif.debug", "classif.featureless", "classif.rpart")),
  rsmp("holdout")
)
bmr = benchmark(design)
bmr
#> 
#> ── <BenchmarkResult> of 6 rows with 6 resampling run ───────────────────────────
#>  nr task_id          learner_id resampling_id iters warnings errors
#>   1   sonar       classif.debug       holdout     1        0      0
#>   2   sonar classif.featureless       holdout     1        0      0
#>   3   sonar       classif.rpart       holdout     1        0      0
#>   4    iris       classif.debug       holdout     1        0      0
#>   5    iris classif.featureless       holdout     1        0      0
#>   6    iris       classif.rpart       holdout     1        0      0
bmr$uhashes
#> [1] "66158772-c831-40d7-af76-5419c3c88beb"
#> [2] "f207b8cb-7d5d-4487-8572-64f2748a5277"
#> [3] "2ad0d242-34c7-4202-b541-77f33279f7ef"
#> [4] "f941afa9-2597-4199-86a3-0d86eaa1d73b"
#> [5] "7db4b34c-3038-4d2d-a921-7d32a515f478"
#> [6] "25637847-9c80-46da-9e38-d3c3c6fdf79b"
uhash(bmr, learner_id = "classif.debug", task_id = "sonar", resampling_id = "holdout")
#> [1] "66158772-c831-40d7-af76-5419c3c88beb"
uhashes(bmr, learner_ids = c("classif.debug", "classif.featureless"))
#> [1] "66158772-c831-40d7-af76-5419c3c88beb"
#> [2] "f207b8cb-7d5d-4487-8572-64f2748a5277"
#> [3] "f941afa9-2597-4199-86a3-0d86eaa1d73b"
#> [4] "7db4b34c-3038-4d2d-a921-7d32a515f478"
```
