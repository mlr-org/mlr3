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
#> [1] "202e3d1d-b856-4145-a202-8843218a6946"
#> [2] "fa4ba092-6d34-46b6-9acb-42390ca3bd5f"
#> [3] "ff6527e3-b69c-41a6-ad40-d79836496943"
#> [4] "2f624603-6e97-45df-a769-e32d62eccdec"
#> [5] "77c86e20-688b-4659-9cba-58313ba7181d"
#> [6] "31029020-674b-4b40-a87b-1baaa57e1197"
uhash(bmr, learner_id = "classif.debug", task_id = "sonar", resampling_id = "holdout")
#> [1] "202e3d1d-b856-4145-a202-8843218a6946"
uhashes(bmr, learner_ids = c("classif.debug", "classif.featureless"))
#> [1] "202e3d1d-b856-4145-a202-8843218a6946"
#> [2] "fa4ba092-6d34-46b6-9acb-42390ca3bd5f"
#> [3] "2f624603-6e97-45df-a769-e32d62eccdec"
#> [4] "77c86e20-688b-4659-9cba-58313ba7181d"
```
