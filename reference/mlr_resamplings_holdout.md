# Holdout Resampling

Splits data into a training set and a test set. Parameter `ratio`
determines the ratio of observation going into the training set
(default: 2/3).

## Dictionary

This [Resampling](https://mlr3.mlr-org.com/reference/Resampling.md) can
be instantiated via the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr_resamplings](https://mlr3.mlr-org.com/reference/mlr_resamplings.md)
or with the associated sugar function
[`rsmp()`](https://mlr3.mlr-org.com/reference/mlr_sugar.md):

    mlr_resamplings$get("holdout")
    rsmp("holdout")

## Parameters

- `ratio` (`numeric(1)`)  
  Ratio of observations to put into the training set.

## References

Bischl B, Mersmann O, Trautmann H, Weihs C (2012). “Resampling Methods
for Meta-Model Validation with Recommendations for Evolutionary
Computation.” *Evolutionary Computation*, **20**(2), 249–275.
[doi:10.1162/evco_a_00069](https://doi.org/10.1162/evco_a_00069) .

## See also

- Chapter in the [mlr3book](https://mlr3book.mlr-org.com/):
  <https://mlr3book.mlr-org.com/chapters/chapter3/evaluation_and_benchmarking.html#sec-resampling>

- Package
  [mlr3spatiotempcv](https://CRAN.R-project.org/package=mlr3spatiotempcv)
  for spatio-temporal resamplings.

- [Dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
  of [Resamplings](https://mlr3.mlr-org.com/reference/Resampling.md):
  [mlr_resamplings](https://mlr3.mlr-org.com/reference/mlr_resamplings.md)

- `as.data.table(mlr_resamplings)` for a table of available
  [Resamplings](https://mlr3.mlr-org.com/reference/Resampling.md) in the
  running session (depending on the loaded packages).

- [mlr3spatiotempcv](https://CRAN.R-project.org/package=mlr3spatiotempcv)
  for additional
  [Resampling](https://mlr3.mlr-org.com/reference/Resampling.md)s for
  spatio-temporal tasks.

Other Resampling:
[`Resampling`](https://mlr3.mlr-org.com/reference/Resampling.md),
[`mlr_resamplings`](https://mlr3.mlr-org.com/reference/mlr_resamplings.md),
[`mlr_resamplings_bootstrap`](https://mlr3.mlr-org.com/reference/mlr_resamplings_bootstrap.md),
[`mlr_resamplings_custom`](https://mlr3.mlr-org.com/reference/mlr_resamplings_custom.md),
[`mlr_resamplings_custom_cv`](https://mlr3.mlr-org.com/reference/mlr_resamplings_custom_cv.md),
[`mlr_resamplings_cv`](https://mlr3.mlr-org.com/reference/mlr_resamplings_cv.md),
[`mlr_resamplings_insample`](https://mlr3.mlr-org.com/reference/mlr_resamplings_insample.md),
[`mlr_resamplings_loo`](https://mlr3.mlr-org.com/reference/mlr_resamplings_loo.md),
[`mlr_resamplings_repeated_cv`](https://mlr3.mlr-org.com/reference/mlr_resamplings_repeated_cv.md),
[`mlr_resamplings_subsampling`](https://mlr3.mlr-org.com/reference/mlr_resamplings_subsampling.md)

## Super class

[`mlr3::Resampling`](https://mlr3.mlr-org.com/reference/Resampling.md)
-\> `ResamplingHoldout`

## Active bindings

- `iters`:

  (`integer(1)`)  
  Returns the number of resampling iterations, depending on the values
  stored in the `param_set`.

## Methods

### Public methods

- [`ResamplingHoldout$new()`](#method-ResamplingHoldout-new)

- [`ResamplingHoldout$clone()`](#method-ResamplingHoldout-clone)

Inherited methods

- [`mlr3::Resampling$format()`](https://mlr3.mlr-org.com/reference/Resampling.html#method-format)
- [`mlr3::Resampling$help()`](https://mlr3.mlr-org.com/reference/Resampling.html#method-help)
- [`mlr3::Resampling$instantiate()`](https://mlr3.mlr-org.com/reference/Resampling.html#method-instantiate)
- [`mlr3::Resampling$print()`](https://mlr3.mlr-org.com/reference/Resampling.html#method-print)
- [`mlr3::Resampling$test_set()`](https://mlr3.mlr-org.com/reference/Resampling.html#method-test_set)
- [`mlr3::Resampling$train_set()`](https://mlr3.mlr-org.com/reference/Resampling.html#method-train_set)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    ResamplingHoldout$new()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ResamplingHoldout$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# Create a task with 10 observations
task = tsk("penguins")
task$filter(1:10)

# Instantiate Resampling
holdout = rsmp("holdout", ratio = 0.5)
holdout$instantiate(task)

# Individual sets:
holdout$train_set(1)
#> [1]  1  3  5  9 10
holdout$test_set(1)
#> [1] 2 4 6 7 8

# Disjunct sets:
intersect(holdout$train_set(1), holdout$test_set(1))
#> integer(0)

# Internal storage:
holdout$instance # simple list
#> $train
#> [1]  1  3  5  9 10
#> 
#> $test
#> [1] 2 4 6 7 8
#> 
```
