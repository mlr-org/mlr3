# Custom Resampling

Splits data into training and test sets using manually provided indices.

## Dictionary

This [Resampling](https://mlr3.mlr-org.com/reference/Resampling.md) can
be instantiated via the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr_resamplings](https://mlr3.mlr-org.com/reference/mlr_resamplings.md)
or with the associated sugar function
[`rsmp()`](https://mlr3.mlr-org.com/reference/mlr_sugar.md):

    mlr_resamplings$get("custom")
    rsmp("custom")

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
[`mlr_resamplings_custom_cv`](https://mlr3.mlr-org.com/reference/mlr_resamplings_custom_cv.md),
[`mlr_resamplings_cv`](https://mlr3.mlr-org.com/reference/mlr_resamplings_cv.md),
[`mlr_resamplings_holdout`](https://mlr3.mlr-org.com/reference/mlr_resamplings_holdout.md),
[`mlr_resamplings_insample`](https://mlr3.mlr-org.com/reference/mlr_resamplings_insample.md),
[`mlr_resamplings_loo`](https://mlr3.mlr-org.com/reference/mlr_resamplings_loo.md),
[`mlr_resamplings_repeated_cv`](https://mlr3.mlr-org.com/reference/mlr_resamplings_repeated_cv.md),
[`mlr_resamplings_subsampling`](https://mlr3.mlr-org.com/reference/mlr_resamplings_subsampling.md)

## Super class

[`mlr3::Resampling`](https://mlr3.mlr-org.com/reference/Resampling.md)
-\> `ResamplingCustom`

## Active bindings

- `iters`:

  (`integer(1)`)  
  Returns the number of resampling iterations, depending on the values
  stored in the `param_set`.

## Methods

### Public methods

- [`ResamplingCustom$new()`](#method-ResamplingCustom-new)

- [`ResamplingCustom$instantiate()`](#method-ResamplingCustom-instantiate)

- [`ResamplingCustom$clone()`](#method-ResamplingCustom-clone)

Inherited methods

- [`mlr3::Resampling$format()`](https://mlr3.mlr-org.com/reference/Resampling.html#method-format)
- [`mlr3::Resampling$help()`](https://mlr3.mlr-org.com/reference/Resampling.html#method-help)
- [`mlr3::Resampling$print()`](https://mlr3.mlr-org.com/reference/Resampling.html#method-print)
- [`mlr3::Resampling$test_set()`](https://mlr3.mlr-org.com/reference/Resampling.html#method-test_set)
- [`mlr3::Resampling$train_set()`](https://mlr3.mlr-org.com/reference/Resampling.html#method-train_set)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    ResamplingCustom$new()

------------------------------------------------------------------------

### Method `instantiate()`

Instantiate this
[Resampling](https://mlr3.mlr-org.com/reference/Resampling.md) with
custom splits into training and test set.

#### Usage

    ResamplingCustom$instantiate(task, train_sets, test_sets)

#### Arguments

- `task`:

  [Task](https://mlr3.mlr-org.com/reference/Task.md)  
  Mainly used to check if `train_sets` and `test_sets` are feasible.

- `train_sets`:

  (list of [`integer()`](https://rdrr.io/r/base/integer.html))  
  List with row ids for training, one list element per iteration. Must
  have the same length as `test_sets`.

- `test_sets`:

  (list of [`integer()`](https://rdrr.io/r/base/integer.html))  
  List with row ids for testing, one list element per iteration. Must
  have the same length as `train_sets`.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ResamplingCustom$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# Create a task with 10 observations
task = tsk("penguins")
task$filter(1:10)

# Instantiate Resampling
custom = rsmp("custom")
train_sets = list(1:5, 5:10)
test_sets = list(5:10, 1:5)
custom$instantiate(task, train_sets, test_sets)

custom$train_set(1)
#> [1] 1 2 3 4 5
custom$test_set(1)
#> [1]  5  6  7  8  9 10
```
