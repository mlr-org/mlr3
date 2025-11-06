# Leave-One-Out Cross-Validation

Splits data using leave-one-observation-out. This is identical to
cross-validation with the number of folds set to the number of
observations.

If this resampling is combined with the grouping features of tasks, it
is possible to create custom splits based on an arbitrary factor
variable, see the examples.

## Dictionary

This [Resampling](https://mlr3.mlr-org.com/dev/reference/Resampling.md)
can be instantiated via the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr_resamplings](https://mlr3.mlr-org.com/dev/reference/mlr_resamplings.md)
or with the associated sugar function
[`rsmp()`](https://mlr3.mlr-org.com/dev/reference/mlr_sugar.md):

    mlr_resamplings$get("loo")
    rsmp("loo")

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
[`mlr_resamplings_repeated_cv`](https://mlr3.mlr-org.com/dev/reference/mlr_resamplings_repeated_cv.md),
[`mlr_resamplings_subsampling`](https://mlr3.mlr-org.com/dev/reference/mlr_resamplings_subsampling.md)

## Super class

[`mlr3::Resampling`](https://mlr3.mlr-org.com/dev/reference/Resampling.md)
-\> `ResamplingLOO`

## Active bindings

- `iters`:

  (`integer(1)`)  
  Returns the number of resampling iterations which is the number of
  rows of the task provided to instantiate. Is `NA` if the resampling
  has not been instantiated.

## Methods

### Public methods

- [`ResamplingLOO$new()`](#method-ResamplingLOO-new)

- [`ResamplingLOO$clone()`](#method-ResamplingLOO-clone)

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

    ResamplingLOO$new()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ResamplingLOO$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# Create a task with 10 observations
task = tsk("penguins")
task$filter(1:10)

# Instantiate Resampling
loo = rsmp("loo")
loo$instantiate(task)

# Individual sets:
loo$train_set(1)
#> [1]  3  6  4  1  5 10  7  2  9
loo$test_set(1)
#> [1] 8

# Disjunct sets:
intersect(loo$train_set(1), loo$test_set(1))
#> integer(0)

# Internal storage:
loo$instance # vector
#>  [1]  8  3  6  4  1  5 10  7  2  9

# Combine with group feature of tasks:
task = tsk("penguins")
task$set_col_roles("island", add_to = "group")
loo$instantiate(task)
loo$iters # one fold for each level of "island"
#> [1] 3
```
