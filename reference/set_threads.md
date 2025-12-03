# Set the Number of Threads

Control the parallelism via threading while calling external packages
from [mlr3](https://CRAN.R-project.org/package=mlr3).

For example, the random forest implementation in package
[ranger](https://CRAN.R-project.org/package=ranger) (connected via
[mlr3learners](https://CRAN.R-project.org/package=mlr3learners))
supports threading via OpenMP. The number of threads to use can be set
via hyperparameter `num.threads`, and defaults to 1. By calling
`set_threads(x, 4)` with `x` being a ranger learner, the hyperparameter
is changed so that 4 cores are used.

If the object `x` does not support threading, `x` is returned as-is. If
applied to a list, recurses through all list elements.

Note that threading is incompatible with other parallelization
techniques such as forking via the
[future::plan](https://future.futureverse.org/reference/plan.html)
[future::multicore](https://future.futureverse.org/reference/multicore.html).
For this reason all learners connected to
[mlr3](https://CRAN.R-project.org/package=mlr3) have threading disabled
in their defaults.

## Usage

``` r
set_threads(x, n = availableCores(), ...)

# Default S3 method
set_threads(x, n = availableCores(), ...)

# S3 method for class 'R6'
set_threads(x, n = availableCores(), ...)

# S3 method for class 'list'
set_threads(x, n = availableCores(), ...)
```

## Arguments

- x:

  (any)  
  Object to set threads for, e.g. a
  [Learner](https://mlr3.mlr-org.com/reference/Learner.md). This object
  is modified in-place.

- n:

  (`integer(1)`)  
  Number of threads to use. Defaults to
  [`parallelly::availableCores()`](https://parallelly.futureverse.org/reference/availableCores.html).

- ...:

  (any)  
  Additional arguments.

## Value

Same object as input `x` (changed in-place), with possibly updated
parameter values.
