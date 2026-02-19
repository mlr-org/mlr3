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
#> [1] "1e517c38-7eee-4b86-ba67-cf385ccf8f54"
#> [2] "f19a1799-5a15-474b-859e-2a374cac2e59"
#> [3] "6e3e12f7-8adc-4400-bdf3-b0a67ca47576"
#> [4] "6935331c-1245-439e-80a8-82c37c9b9490"
#> [5] "b2b51243-1e5d-4e99-b921-4f5977162e21"
#> [6] "8ff15bc3-6862-47da-be8b-4c1c6395249a"
uhash(bmr, learner_id = "classif.debug", task_id = "sonar", resampling_id = "holdout")
#> [1] "1e517c38-7eee-4b86-ba67-cf385ccf8f54"
uhashes(bmr, learner_ids = c("classif.debug", "classif.featureless"))
#> [1] "1e517c38-7eee-4b86-ba67-cf385ccf8f54"
#> [2] "f19a1799-5a15-474b-859e-2a374cac2e59"
#> [3] "6935331c-1245-439e-80a8-82c37c9b9490"
#> [4] "b2b51243-1e5d-4e99-b921-4f5977162e21"
```
