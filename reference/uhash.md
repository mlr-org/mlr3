# Obtain specific uhashes from a [BenchmarkResult](https://mlr3.mlr-org.com/reference/BenchmarkResult.md)

In a
[`BenchmarkResult`](https://mlr3.mlr-org.com/reference/BenchmarkResult.md),
each
[ResampleResult](https://mlr3.mlr-org.com/reference/ResampleResult.md)
is uniquely identified by a *hash* (*uhash*). Operations that select
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
#> [1] "33bf3a87-f7ad-44e2-9647-d009b4b8f041"
#> [2] "f274024f-ac45-4d8d-ac97-aa8a370a11cb"
#> [3] "a4eba6a6-0929-416b-966a-280f7c5b3073"
#> [4] "cfcd9429-c657-4522-84b2-982f2aa77afe"
#> [5] "f842d461-4449-480f-bcd8-2ac0a1a3f07a"
#> [6] "bda7ed38-8fd0-4bf2-8e54-ec5dad1cfc63"
uhash(bmr, learner_id = "classif.debug", task_id = "sonar", resampling_id = "holdout")
#> [1] "33bf3a87-f7ad-44e2-9647-d009b4b8f041"
uhashes(bmr, learner_ids = c("classif.debug", "classif.featureless"))
#> [1] "33bf3a87-f7ad-44e2-9647-d009b4b8f041"
#> [2] "f274024f-ac45-4d8d-ac97-aa8a370a11cb"
#> [3] "cfcd9429-c657-4522-84b2-982f2aa77afe"
#> [4] "f842d461-4449-480f-bcd8-2ac0a1a3f07a"
```
