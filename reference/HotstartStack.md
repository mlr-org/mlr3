# Stack for Hot Start Learners

This class stores learners for hot starting training, i.e. resuming or
continuing from an already fitted model. We assume that hot starting is
only possible if a single hyperparameter (also called the fidelity
parameter, usually controlling the complexity or expensiveness) is
altered and all other hyperparameters are identical.

The `HotstartStack` stores trained learners which can be potentially
used to hot start a learner. Learner automatically hot start while
training if a stack is attached to the `$hotstart_stack` field and the
stack contains a suitable learner.

For example, if you want to train a random forest learner with 1000
trees but already have a random forest learner with 500 trees (hot start
learner), you can add the hot start learner to the `HotstartStack` of
the expensive learner with 1000 trees. If you now call the `train()`
method (or
[`resample()`](https://mlr3.mlr-org.com/reference/resample.md) or
[`benchmark()`](https://mlr3.mlr-org.com/reference/benchmark.md)), a
random forest with 500 trees will be fitted and combined with the 500
trees of the hotstart learner, effectively saving you to fit 500 trees.

Hot starting is only supported by learners which have the property
`"hotstart_forward"` or `"hotstart_backward"`. For example, an `xgboost`
model (in
[mlr3learners](https://CRAN.R-project.org/package=mlr3learners)) can hot
start forward by adding more boosting iterations, and a random forest
can go backwards by removing trees. The fidelity parameters are tagged
with `"hotstart"` in learner's parameter set.

## Public fields

- `stack`:

  [`data.table::data.table()`](https://rdatatable.gitlab.io/data.table/reference/data.table.html)  
  Stores hot start learners.

- `hotstart_threshold`:

  (named `numeric(1)`)  
  Threshold for storing learners in the stack. If the value of the
  hotstart parameter is below this threshold, the learner is not added
  to the stack.

## Methods

### Public methods

- [`HotstartStack$new()`](#method-HotstartStack-new)

- [`HotstartStack$add()`](#method-HotstartStack-add)

- [`HotstartStack$start_cost()`](#method-HotstartStack-start_cost)

- [`HotstartStack$format()`](#method-HotstartStack-format)

- [`HotstartStack$print()`](#method-HotstartStack-print)

- [`HotstartStack$clone()`](#method-HotstartStack-clone)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    HotstartStack$new(learners = NULL, hotstart_threshold = NULL)

#### Arguments

- `learners`:

  (List of [Learner](https://mlr3.mlr-org.com/reference/Learner.md)s)  
  Learners are added to the hotstart stack. If `NULL` (default), empty
  stack is created.

- `hotstart_threshold`:

  (named `numeric(1)`)  
  Threshold for storing learners in the stack.

------------------------------------------------------------------------

### Method `add()`

Add learners to hot start stack.

#### Usage

    HotstartStack$add(learners)

#### Arguments

- `learners`:

  (List of [Learner](https://mlr3.mlr-org.com/reference/Learner.md)s).
  Learners are added to the hotstart stack.

#### Returns

self (invisibly).

------------------------------------------------------------------------

### Method `start_cost()`

Calculates the cost for each learner of the stack to hot start the
target `learner`.

The following cost values can be returned:

- `NA_real_`: Learner is unsuitable to hot start target `learner`.

- `-1`: Hotstart learner in the stack and target `learner` are
  identical.

- `0` Cost for hot starting backwards is always 0.

- `> 0` Cost for hot starting forward.

#### Usage

    HotstartStack$start_cost(learner, task_hash)

#### Arguments

- `learner`:

  [Learner](https://mlr3.mlr-org.com/reference/Learner.md)  
  Target learner.

- `task_hash`:

  [Task](https://mlr3.mlr-org.com/reference/Task.md)  
  Hash of the task on which the target learner is trained.

------------------------------------------------------------------------

### Method [`format()`](https://rdrr.io/r/base/format.html)

Helper for print outputs.

#### Usage

    HotstartStack$format(...)

#### Arguments

- `...`:

  (ignored).

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Printer.

#### Usage

    HotstartStack$print(...)

#### Arguments

- `...`:

  (ignored).

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    HotstartStack$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# train learner on pima task
task = tsk("pima")
learner = lrn("classif.debug", iter = 1)
learner$train(task)

# initialize stack with previously fitted learner
hot = HotstartStack$new(list(learner))

# retrieve learner with increased fidelity parameter
learner = lrn("classif.debug", iter = 2)

# calculate cost of hot starting
hot$start_cost(learner, task$hash)
#> [1] 1

# add stack with hot start learner
learner$hotstart_stack = hot

# train automatically uses hot start learner while fitting the model
learner$train(task)
```
