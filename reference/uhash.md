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
#> [1] "063c6605-b4ea-493c-98dd-7856da27abb5"
#> [2] "67b9e330-4dc1-40d5-8503-fecc832adb39"
#> [3] "e44683cf-40b3-4acb-9977-3597f6b114ed"
#> [4] "3a29abdb-6c28-4e90-85b9-0242c05873a4"
#> [5] "26b5c414-1d7b-49fe-a6c1-900dc1823486"
#> [6] "3a3f237c-4e17-4655-af48-d06233325ad1"
uhash(bmr, learner_id = "classif.debug", task_id = "sonar", resampling_id = "holdout")
#> [1] "063c6605-b4ea-493c-98dd-7856da27abb5"
uhashes(bmr, learner_ids = c("classif.debug", "classif.featureless"))
#> [1] "063c6605-b4ea-493c-98dd-7856da27abb5"
#> [2] "67b9e330-4dc1-40d5-8503-fecc832adb39"
#> [3] "3a29abdb-6c28-4e90-85b9-0242c05873a4"
#> [4] "26b5c414-1d7b-49fe-a6c1-900dc1823486"
```
