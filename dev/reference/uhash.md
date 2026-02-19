# Obtain specific uhashes from a [BenchmarkResult](https://mlr3.mlr-org.com/dev/reference/BenchmarkResult.md)

In a
[`BenchmarkResult`](https://mlr3.mlr-org.com/dev/reference/BenchmarkResult.md),
each
[ResampleResult](https://mlr3.mlr-org.com/dev/reference/ResampleResult.md)
is *u*niquely identified by a *hash* (*uhash*). Operations that select
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
#> [1] "6221117b-cf09-4be4-acfc-a84cd3d4a51d"
#> [2] "9b8b4196-a610-47d4-95ed-ab9a69be168f"
#> [3] "914c64fb-ce64-407d-9942-9c2384828641"
#> [4] "2da4968f-ca49-4f11-afb4-99a5b2360bbb"
#> [5] "a2d30880-9ef0-48d6-bd4d-d81685e95ea3"
#> [6] "6a6573a5-83db-4277-b445-1eae156ef2ef"
uhash(bmr, learner_id = "classif.debug", task_id = "sonar", resampling_id = "holdout")
#> [1] "6221117b-cf09-4be4-acfc-a84cd3d4a51d"
uhashes(bmr, learner_ids = c("classif.debug", "classif.featureless"))
#> [1] "6221117b-cf09-4be4-acfc-a84cd3d4a51d"
#> [2] "9b8b4196-a610-47d4-95ed-ab9a69be168f"
#> [3] "2da4968f-ca49-4f11-afb4-99a5b2360bbb"
#> [4] "a2d30880-9ef0-48d6-bd4d-d81685e95ea3"
```
