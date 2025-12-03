# Dictionary of Resampling Strategies

A simple
[mlr3misc::Dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
storing objects of class
[Resampling](https://mlr3.mlr-org.com/reference/Resampling.md). Each
resampling has an associated help page, see `mlr_resamplings_[id]`.

This dictionary can get populated with additional resampling strategies
by add-on packages.

For a more convenient way to retrieve and construct resampling
strategies, see
[`rsmp()`](https://mlr3.mlr-org.com/reference/mlr_sugar.md)/[`rsmps()`](https://mlr3.mlr-org.com/reference/mlr_sugar.md).

## Format

[R6::R6Class](https://r6.r-lib.org/reference/R6Class.html) object
inheriting from
[mlr3misc::Dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html).

## Methods

See
[mlr3misc::Dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html).

## S3 methods

- `as.data.table(dict, ..., objects = FALSE)`  
  [mlr3misc::Dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
  -\>
  [`data.table::data.table()`](https://rdatatable.gitlab.io/data.table/reference/data.table.html)  
  Returns a
  [`data.table::data.table()`](https://rdatatable.gitlab.io/data.table/reference/data.table.html)
  with columns "key", "label", "params", and "iters". If `objects` is
  set to `TRUE`, the constructed objects are returned in the list column
  named `object`.

## See also

Sugar functions:
[`rsmp()`](https://mlr3.mlr-org.com/reference/mlr_sugar.md),
[`rsmps()`](https://mlr3.mlr-org.com/reference/mlr_sugar.md)

Other Dictionary:
[`mlr_learners`](https://mlr3.mlr-org.com/reference/mlr_learners.md),
[`mlr_measures`](https://mlr3.mlr-org.com/reference/mlr_measures.md),
[`mlr_task_generators`](https://mlr3.mlr-org.com/reference/mlr_task_generators.md),
[`mlr_tasks`](https://mlr3.mlr-org.com/reference/mlr_tasks.md)

Other Resampling:
[`Resampling`](https://mlr3.mlr-org.com/reference/Resampling.md),
[`mlr_resamplings_bootstrap`](https://mlr3.mlr-org.com/reference/mlr_resamplings_bootstrap.md),
[`mlr_resamplings_custom`](https://mlr3.mlr-org.com/reference/mlr_resamplings_custom.md),
[`mlr_resamplings_custom_cv`](https://mlr3.mlr-org.com/reference/mlr_resamplings_custom_cv.md),
[`mlr_resamplings_cv`](https://mlr3.mlr-org.com/reference/mlr_resamplings_cv.md),
[`mlr_resamplings_holdout`](https://mlr3.mlr-org.com/reference/mlr_resamplings_holdout.md),
[`mlr_resamplings_insample`](https://mlr3.mlr-org.com/reference/mlr_resamplings_insample.md),
[`mlr_resamplings_loo`](https://mlr3.mlr-org.com/reference/mlr_resamplings_loo.md),
[`mlr_resamplings_repeated_cv`](https://mlr3.mlr-org.com/reference/mlr_resamplings_repeated_cv.md),
[`mlr_resamplings_subsampling`](https://mlr3.mlr-org.com/reference/mlr_resamplings_subsampling.md)

## Examples

``` r
as.data.table(mlr_resamplings)
#> Key: <key>
#>            key                         label        params iters
#>         <char>                        <char>        <list> <int>
#> 1:   bootstrap                     Bootstrap ratio,repeats    30
#> 2:      custom                 Custom Splits                  NA
#> 3:   custom_cv Custom Split Cross-Validation                  NA
#> 4:          cv              Cross-Validation         folds    10
#> 5:     holdout                       Holdout         ratio     1
#> 6:    insample           Insample Resampling                   1
#> 7:         loo                 Leave-One-Out                  NA
#> 8: repeated_cv     Repeated Cross-Validation folds,repeats   100
#> 9: subsampling                   Subsampling ratio,repeats    30
mlr_resamplings$get("cv")
#> 
#> ── <ResamplingCV> : Cross-Validation ───────────────────────────────────────────
#> • Iterations: 10
#> • Instantiated: FALSE
#> • Parameters: folds=10
rsmp("subsampling")
#> 
#> ── <ResamplingSubsampling> : Subsampling ───────────────────────────────────────
#> • Iterations: 30
#> • Instantiated: FALSE
#> • Parameters: ratio=0.6667, repeats=30
```
