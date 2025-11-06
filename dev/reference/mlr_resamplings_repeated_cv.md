# Repeated Cross-Validation Resampling

Splits data `repeats` (default: 10) times using a `folds`-fold (default:
10) cross-validation.

The iteration counter translates to `repeats` blocks of `folds`
cross-validations, i.e., the first `folds` iterations belong to a single
cross-validation.

Iteration numbers can be translated into folds or repeats with provided
methods.

## Dictionary

This [Resampling](https://mlr3.mlr-org.com/dev/reference/Resampling.md)
can be instantiated via the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr_resamplings](https://mlr3.mlr-org.com/dev/reference/mlr_resamplings.md)
or with the associated sugar function
[`rsmp()`](https://mlr3.mlr-org.com/dev/reference/mlr_sugar.md):

    mlr_resamplings$get("repeated_cv")
    rsmp("repeated_cv")

## Parameters

- `repeats` (`integer(1)`)  
  Number of repetitions.

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
  of
  [Resamplings](https://mlr3.mlr-org.com/dev/reference/Resampling.md):
  [mlr_resamplings](https://mlr3.mlr-org.com/dev/reference/mlr_resamplings.md)

- `as.data.table(mlr_resamplings)` for a table of available
  [Resamplings](https://mlr3.mlr-org.com/dev/reference/Resampling.md) in
  the running session (depending on the loaded packages).

- [mlr3spatiotempcv](https://CRAN.R-project.org/package=mlr3spatiotempcv)
  for additional
  [Resampling](https://mlr3.mlr-org.com/dev/reference/Resampling.md)s
  for spatio-temporal tasks.

Other Resampling:
[`Resampling`](https://mlr3.mlr-org.com/dev/reference/Resampling.md),
[`mlr_resamplings`](https://mlr3.mlr-org.com/dev/reference/mlr_resamplings.md),
[`mlr_resamplings_bootstrap`](https://mlr3.mlr-org.com/dev/reference/mlr_resamplings_bootstrap.md),
[`mlr_resamplings_custom`](https://mlr3.mlr-org.com/dev/reference/mlr_resamplings_custom.md),
[`mlr_resamplings_custom_cv`](https://mlr3.mlr-org.com/dev/reference/mlr_resamplings_custom_cv.md),
[`mlr_resamplings_cv`](https://mlr3.mlr-org.com/dev/reference/mlr_resamplings_cv.md),
[`mlr_resamplings_holdout`](https://mlr3.mlr-org.com/dev/reference/mlr_resamplings_holdout.md),
[`mlr_resamplings_insample`](https://mlr3.mlr-org.com/dev/reference/mlr_resamplings_insample.md),
[`mlr_resamplings_loo`](https://mlr3.mlr-org.com/dev/reference/mlr_resamplings_loo.md),
[`mlr_resamplings_subsampling`](https://mlr3.mlr-org.com/dev/reference/mlr_resamplings_subsampling.md)

## Super class

[`mlr3::Resampling`](https://mlr3.mlr-org.com/dev/reference/Resampling.md)
-\> `ResamplingRepeatedCV`

## Active bindings

- `iters`:

  (`integer(1)`)  
  Returns the number of resampling iterations, depending on the values
  stored in the `param_set`.

## Methods

### Public methods

- [`ResamplingRepeatedCV$new()`](#method-ResamplingRepeatedCV-new)

- [`ResamplingRepeatedCV$folds()`](#method-ResamplingRepeatedCV-folds)

- [`ResamplingRepeatedCV$repeats()`](#method-ResamplingRepeatedCV-repeats)

- [`ResamplingRepeatedCV$clone()`](#method-ResamplingRepeatedCV-clone)

Inherited methods

- [`mlr3::Resampling$format()`](https://mlr3.mlr-org.com/dev/reference/Resampling.html#method-format)
- [`mlr3::Resampling$help()`](https://mlr3.mlr-org.com/dev/reference/Resampling.html#method-help)
- [`mlr3::Resampling$instantiate()`](https://mlr3.mlr-org.com/dev/reference/Resampling.html#method-instantiate)
- [`mlr3::Resampling$print()`](https://mlr3.mlr-org.com/dev/reference/Resampling.html#method-print)
- [`mlr3::Resampling$test_set()`](https://mlr3.mlr-org.com/dev/reference/Resampling.html#method-test_set)
- [`mlr3::Resampling$train_set()`](https://mlr3.mlr-org.com/dev/reference/Resampling.html#method-train_set)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    ResamplingRepeatedCV$new()

------------------------------------------------------------------------

### Method `folds()`

Translates iteration numbers to fold numbers.

#### Usage

    ResamplingRepeatedCV$folds(iters)

#### Arguments

- `iters`:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  Iteration number.

#### Returns

[`integer()`](https://rdrr.io/r/base/integer.html) of fold numbers.

------------------------------------------------------------------------

### Method `repeats()`

Translates iteration numbers to repetition numbers.

#### Usage

    ResamplingRepeatedCV$repeats(iters)

#### Arguments

- `iters`:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  Iteration number.

#### Returns

[`integer()`](https://rdrr.io/r/base/integer.html) of repetition
numbers.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ResamplingRepeatedCV$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# Create a task with 10 observations
task = tsk("penguins")
task$filter(1:10)

# Instantiate Resampling
repeated_cv = rsmp("repeated_cv", repeats = 2, folds = 3)
repeated_cv$instantiate(task)
repeated_cv$iters
#> [1] 6
repeated_cv$folds(1:6)
#> [1] 1 2 3 1 2 3
repeated_cv$repeats(1:6)
#> [1] 1 1 1 2 2 2

# Individual sets:
repeated_cv$train_set(1)
#> [1]  2  6  9  7  8 10
repeated_cv$test_set(1)
#> [1] 1 3 4 5

# Disjunct sets:
intersect(repeated_cv$train_set(1), repeated_cv$test_set(1))
#> integer(0)

# Internal storage:
repeated_cv$instance # table
#>     row_id   rep  fold
#>      <int> <int> <int>
#>  1:      1     1     1
#>  2:      2     1     2
#>  3:      3     1     1
#>  4:      4     1     1
#>  5:      5     1     1
#>  6:      6     1     2
#>  7:      7     1     3
#>  8:      8     1     3
#>  9:      9     1     2
#> 10:     10     1     3
#> 11:      1     2     2
#> 12:      2     2     3
#> 13:      3     2     1
#> 14:      4     2     3
#> 15:      5     2     3
#> 16:      6     2     1
#> 17:      7     2     2
#> 18:      8     2     1
#> 19:      9     2     2
#> 20:     10     2     1
#>     row_id   rep  fold
```
