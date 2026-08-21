# Subsampling Resampling

Splits data `repeats` (default: 30) times into training and test set
with a ratio of `ratio` (default: 2/3) observations going into the
training set.

## Dictionary

This [Resampling](https://mlr3.mlr-org.com/reference/Resampling.md) can
be instantiated via the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr_resamplings](https://mlr3.mlr-org.com/reference/mlr_resamplings.md)
or with the associated sugar function
[`rsmp()`](https://mlr3.mlr-org.com/reference/mlr_sugar.md):

    mlr_resamplings$get("subsampling")
    rsmp("subsampling")

## Parameters

- `repeats` (`integer(1)`)  
  Number of repetitions.

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
[`mlr_resamplings_holdout`](https://mlr3.mlr-org.com/reference/mlr_resamplings_holdout.md),
[`mlr_resamplings_insample`](https://mlr3.mlr-org.com/reference/mlr_resamplings_insample.md),
[`mlr_resamplings_loo`](https://mlr3.mlr-org.com/reference/mlr_resamplings_loo.md),
[`mlr_resamplings_repeated_cv`](https://mlr3.mlr-org.com/reference/mlr_resamplings_repeated_cv.md)

## Super class

[`Resampling`](https://mlr3.mlr-org.com/reference/Resampling.md) -\>
`ResamplingSubsampling`

## Active bindings

- `iters`:

  (`integer(1)`)  
  Returns the number of resampling iterations, depending on the values
  stored in the `param_set`.

## Methods

### Public methods

- [`ResamplingSubsampling$new()`](#method-ResamplingSubsampling-initialize)

- [`ResamplingSubsampling$clone()`](#method-ResamplingSubsampling-clone)

Inherited methods

- [`Resampling$format()`](https://mlr3.mlr-org.com/reference/Resampling.html#method-format)
- [`Resampling$help()`](https://mlr3.mlr-org.com/reference/Resampling.html#method-help)
- [`Resampling$instantiate()`](https://mlr3.mlr-org.com/reference/Resampling.html#method-instantiate)
- [`Resampling$print()`](https://mlr3.mlr-org.com/reference/Resampling.html#method-print)
- [`Resampling$test_set()`](https://mlr3.mlr-org.com/reference/Resampling.html#method-test_set)
- [`Resampling$train_set()`](https://mlr3.mlr-org.com/reference/Resampling.html#method-train_set)

------------------------------------------------------------------------

### `ResamplingSubsampling$new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    ResamplingSubsampling$new()

------------------------------------------------------------------------

### `ResamplingSubsampling$clone()`

The objects of this class are cloneable with this method.

#### Usage

    ResamplingSubsampling$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# Create a task with 10 observations
task = tsk("penguins")
task$filter(1:10)

# Instantiate Resampling
subsampling = rsmp("subsampling", repeats = 2, ratio = 0.5)
subsampling$instantiate(task)

# Individual sets:
subsampling$train_set(1)
#> [1] 10  9  3  6  7
subsampling$test_set(1)
#> [1] 1 2 4 5 8

# Disjunct sets:
intersect(subsampling$train_set(1), subsampling$test_set(1))
#> integer(0)

# Internal storage:
subsampling$instance$train # list of index vectors
#> [[1]]
#> [1] 10  9  3  6  7
#> 
#> [[2]]
#> [1] 3 7 9 4 5
#> 
```
