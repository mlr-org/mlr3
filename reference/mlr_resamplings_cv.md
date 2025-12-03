# Cross-Validation Resampling

Splits data using a `folds`-folds (default: 10 folds) cross-validation.

## Dictionary

This [Resampling](https://mlr3.mlr-org.com/reference/Resampling.md) can
be instantiated via the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr_resamplings](https://mlr3.mlr-org.com/reference/mlr_resamplings.md)
or with the associated sugar function
[`rsmp()`](https://mlr3.mlr-org.com/reference/mlr_sugar.md):

    mlr_resamplings$get("cv")
    rsmp("cv")

## Parameters

- `folds` (`integer(1)`)  
  Number of folds.

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
[`mlr_resamplings_holdout`](https://mlr3.mlr-org.com/reference/mlr_resamplings_holdout.md),
[`mlr_resamplings_insample`](https://mlr3.mlr-org.com/reference/mlr_resamplings_insample.md),
[`mlr_resamplings_loo`](https://mlr3.mlr-org.com/reference/mlr_resamplings_loo.md),
[`mlr_resamplings_repeated_cv`](https://mlr3.mlr-org.com/reference/mlr_resamplings_repeated_cv.md),
[`mlr_resamplings_subsampling`](https://mlr3.mlr-org.com/reference/mlr_resamplings_subsampling.md)

## Super class

[`mlr3::Resampling`](https://mlr3.mlr-org.com/reference/Resampling.md)
-\> `ResamplingCV`

## Active bindings

- `iters`:

  (`integer(1)`)  
  Returns the number of resampling iterations, depending on the values
  stored in the `param_set`.

## Methods

### Public methods

- [`ResamplingCV$new()`](#method-ResamplingCV-new)

- [`ResamplingCV$clone()`](#method-ResamplingCV-clone)

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

    ResamplingCV$new()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ResamplingCV$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# Create a task with 10 observations
task = tsk("penguins")
task$filter(1:10)

# Instantiate Resampling
cv = rsmp("cv", folds = 3)
cv$instantiate(task)

# Individual sets:
cv$train_set(1)
#> [1]  1  2  7  3  5 10
cv$test_set(1)
#> [1] 4 6 8 9

# Disjunct sets:
intersect(cv$train_set(1), cv$test_set(1))
#> integer(0)

# Internal storage:
cv$instance # table
#> Key: <fold>
#>     row_id  fold
#>      <int> <int>
#>  1:      4     1
#>  2:      6     1
#>  3:      8     1
#>  4:      9     1
#>  5:      1     2
#>  6:      2     2
#>  7:      7     2
#>  8:      3     3
#>  9:      5     3
#> 10:     10     3
```
