# Obtain specific uhashes from a [BenchmarkResult](https://mlr3.mlr-org.com/dev/reference/BenchmarkResult.md)

In a
[`BenchmarkResult`](https://mlr3.mlr-org.com/dev/reference/BenchmarkResult.md),
each
[ResampleResult](https://mlr3.mlr-org.com/dev/reference/ResampleResult.md)
is uniquely identified by a *hash* (*uhash*). Operations that select
specific
[ResampleResult](https://mlr3.mlr-org.com/dev/reference/ResampleResult.md)s
from a
[BenchmarkResult](https://mlr3.mlr-org.com/dev/reference/BenchmarkResult.md)
operate using these hashes. This function allows to obtain uhashes for
specific learners, tasks, and resamplings.

If you want more control, you can also directly obtain the uhash table
from the
[`BenchmarkResult`](https://mlr3.mlr-org.com/dev/reference/BenchmarkResult.md)
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
#> [1] "d19d5711-5c7e-4d2e-96ed-b36540414853"
#> [2] "d12e4932-6642-469f-b8a5-aae50c3cfe67"
#> [3] "0abed48d-6a01-4b19-b52b-9c41407aa7f5"
#> [4] "263273d9-49fe-423a-940b-651c30a3a1cf"
#> [5] "c3ac1211-939b-49a0-99d8-2c7a1f3887f9"
#> [6] "ac3ecbf0-8274-4f09-a25b-6486c23824a2"
uhash(bmr, learner_id = "classif.debug", task_id = "sonar", resampling_id = "holdout")
#> [1] "d19d5711-5c7e-4d2e-96ed-b36540414853"
uhashes(bmr, learner_ids = c("classif.debug", "classif.featureless"))
#> [1] "d19d5711-5c7e-4d2e-96ed-b36540414853"
#> [2] "d12e4932-6642-469f-b8a5-aae50c3cfe67"
#> [3] "263273d9-49fe-423a-940b-651c30a3a1cf"
#> [4] "c3ac1211-939b-49a0-99d8-2c7a1f3887f9"
```
