# Custom Cross-Validation

Splits data into training and test sets in a cross-validation fashion
based on a user-provided categorical vector. This vector can be passed
during instantiation either via an arbitrary factor `f` with the same
length as `task$nrow`, or via a single string `col` referring to a
column in the task.

An alternative but equivalent approach using leave-one-out resampling is
showcased in the examples of
[mlr_resamplings_loo](https://mlr3.mlr-org.com/dev/reference/mlr_resamplings_loo.md).

## Dictionary

This [Resampling](https://mlr3.mlr-org.com/dev/reference/Resampling.md)
can be instantiated via the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr_resamplings](https://mlr3.mlr-org.com/dev/reference/mlr_resamplings.md)
or with the associated sugar function
[`rsmp()`](https://mlr3.mlr-org.com/dev/reference/mlr_sugar.md):

    mlr_resamplings$get("custom_cv")
    rsmp("custom_cv")

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
[`mlr_resamplings_cv`](https://mlr3.mlr-org.com/dev/reference/mlr_resamplings_cv.md),
[`mlr_resamplings_holdout`](https://mlr3.mlr-org.com/dev/reference/mlr_resamplings_holdout.md),
[`mlr_resamplings_insample`](https://mlr3.mlr-org.com/dev/reference/mlr_resamplings_insample.md),
[`mlr_resamplings_loo`](https://mlr3.mlr-org.com/dev/reference/mlr_resamplings_loo.md),
[`mlr_resamplings_repeated_cv`](https://mlr3.mlr-org.com/dev/reference/mlr_resamplings_repeated_cv.md),
[`mlr_resamplings_subsampling`](https://mlr3.mlr-org.com/dev/reference/mlr_resamplings_subsampling.md)

## Super class

[`mlr3::Resampling`](https://mlr3.mlr-org.com/dev/reference/Resampling.md)
-\> `ResamplingCustomCV`

## Active bindings

- `iters`:

  (`integer(1)`)  
  Returns the number of resampling iterations, depending on the values
  stored in the `param_set`.

## Methods

### Public methods

- [`ResamplingCustomCV$new()`](#method-ResamplingCustomCV-new)

- [`ResamplingCustomCV$instantiate()`](#method-ResamplingCustomCV-instantiate)

- [`ResamplingCustomCV$clone()`](#method-ResamplingCustomCV-clone)

Inherited methods

- [`mlr3::Resampling$format()`](https://mlr3.mlr-org.com/dev/reference/Resampling.html#method-format)
- [`mlr3::Resampling$help()`](https://mlr3.mlr-org.com/dev/reference/Resampling.html#method-help)
- [`mlr3::Resampling$print()`](https://mlr3.mlr-org.com/dev/reference/Resampling.html#method-print)
- [`mlr3::Resampling$test_set()`](https://mlr3.mlr-org.com/dev/reference/Resampling.html#method-test_set)
- [`mlr3::Resampling$train_set()`](https://mlr3.mlr-org.com/dev/reference/Resampling.html#method-train_set)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    ResamplingCustomCV$new()

------------------------------------------------------------------------

### Method `instantiate()`

Instantiate this
[Resampling](https://mlr3.mlr-org.com/dev/reference/Resampling.md) as
cross-validation with custom splits.

#### Usage

    ResamplingCustomCV$instantiate(task, f = NULL, col = NULL)

#### Arguments

- `task`:

  [Task](https://mlr3.mlr-org.com/dev/reference/Task.md)  
  Used to extract row ids.

- `f`:

  ([`factor()`](https://rdrr.io/r/base/factor.html) \|
  [`character()`](https://rdrr.io/r/base/character.html))  
  Vector of type factor or character with the same length as
  `task$nrow`. Row ids are split on this vector, each distinct value
  results in a fold. Empty factor levels are dropped and row ids
  corresponding to missing values are removed, c.f.
  [`split()`](https://rdrr.io/r/base/split.html).

- `col`:

  (`character(1)`)  
  Name of the task column to use for splitting. Alternative and mutually
  exclusive to providing the factor levels as a vector via parameter
  `f`.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ResamplingCustomCV$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# Create a task with 10 observations
task = tsk("penguins")
task$filter(1:10)

# Instantiate Resampling:
custom_cv = rsmp("custom_cv")
f = factor(c(rep(letters[1:3], each = 3), NA))
custom_cv$instantiate(task, f = f)
custom_cv$iters # 3 folds
#> [1] 3

# Individual sets:
custom_cv$train_set(1)
#> [1] 4 5 6 7 8 9
custom_cv$test_set(1)
#> [1] 1 2 3

# Disjunct sets:
intersect(custom_cv$train_set(1), custom_cv$test_set(1))
#> integer(0)
```
