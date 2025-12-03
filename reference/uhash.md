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
#> [1] "390e16bb-bc67-4404-92b7-d2feadc067a3"
#> [2] "76790c5c-98c1-4631-81f4-2626e32050eb"
#> [3] "30beb462-474a-4258-ad9e-586cbfb22ceb"
#> [4] "5c2eaf6f-b280-41bd-8191-58435136e7f3"
#> [5] "57ab2a00-6e94-428f-b2c7-55bb15e2bd9a"
#> [6] "33224098-0591-40a2-abf7-0dfbe794181f"
uhash(bmr, learner_id = "classif.debug", task_id = "sonar", resampling_id = "holdout")
#> [1] "390e16bb-bc67-4404-92b7-d2feadc067a3"
uhashes(bmr, learner_ids = c("classif.debug", "classif.featureless"))
#> [1] "390e16bb-bc67-4404-92b7-d2feadc067a3"
#> [2] "76790c5c-98c1-4631-81f4-2626e32050eb"
#> [3] "5c2eaf6f-b280-41bd-8191-58435136e7f3"
#> [4] "57ab2a00-6e94-428f-b2c7-55bb15e2bd9a"
```
