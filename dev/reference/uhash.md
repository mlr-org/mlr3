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
#> [1] "1285f302-2467-4588-9d5c-217283a61ad5"
#> [2] "83254e83-e1cb-4bfd-8ea3-efeca57a60fd"
#> [3] "cab6a945-968b-406e-94de-c04cae7f2845"
#> [4] "d721b714-e642-4a12-8dde-85861029cc1c"
#> [5] "36fc1dcb-64dc-4162-a188-c0cebb53b200"
#> [6] "571e0c14-a370-45ca-a27c-ea9720e6968d"
uhash(bmr, learner_id = "classif.debug", task_id = "sonar", resampling_id = "holdout")
#> [1] "1285f302-2467-4588-9d5c-217283a61ad5"
uhashes(bmr, learner_ids = c("classif.debug", "classif.featureless"))
#> [1] "1285f302-2467-4588-9d5c-217283a61ad5"
#> [2] "83254e83-e1cb-4bfd-8ea3-efeca57a60fd"
#> [3] "d721b714-e642-4a12-8dde-85861029cc1c"
#> [4] "36fc1dcb-64dc-4162-a188-c0cebb53b200"
```
