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
#> [1] "0a96df3c-e98a-41c1-9df2-82607cf64c09"
#> [2] "e04f112d-0eb8-4124-95ad-cb9bf72d9c8f"
#> [3] "a1471edf-9644-4718-97ce-bae072b75f6c"
#> [4] "d56e24b1-9520-43b8-b84d-0461fc23620e"
#> [5] "590b011c-6ee2-4ff7-b13e-85cedf424600"
#> [6] "312cb4ef-4afc-4708-bafb-b971ab70fd31"
uhash(bmr, learner_id = "classif.debug", task_id = "sonar", resampling_id = "holdout")
#> [1] "0a96df3c-e98a-41c1-9df2-82607cf64c09"
uhashes(bmr, learner_ids = c("classif.debug", "classif.featureless"))
#> [1] "0a96df3c-e98a-41c1-9df2-82607cf64c09"
#> [2] "e04f112d-0eb8-4124-95ad-cb9bf72d9c8f"
#> [3] "d56e24b1-9520-43b8-b84d-0461fc23620e"
#> [4] "590b011c-6ee2-4ff7-b13e-85cedf424600"
```
