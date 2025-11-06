# Resample Context

A
[CallbackResample](https://mlr3.mlr-org.com/dev/reference/CallbackResample.md)
accesses and modifies data during
[`resample()`](https://mlr3.mlr-org.com/dev/reference/resample.md) and
[`benchmark()`](https://mlr3.mlr-org.com/dev/reference/benchmark.md) via
the `ContextResample`. See the section on fields for a list of
modifiable objects. See
[`callback_resample()`](https://mlr3.mlr-org.com/dev/reference/callback_resample.md)
for a list of stages that access `ContextResample`.

## Super class

[`mlr3misc::Context`](https://mlr3misc.mlr-org.com/reference/Context.html)
-\> `ContextResample`

## Active bindings

- `task`:

  ([Task](https://mlr3.mlr-org.com/dev/reference/Task.md))  
  The task to be evaluated. The task is unchanged during the evaluation.
  The task is read-only.

- `learner`:

  ([Learner](https://mlr3.mlr-org.com/dev/reference/Learner.md))  
  The learner to be evaluated. The learner contains the models after
  stage `on_resample_before_train`.

- `resampling`:

  [Resampling](https://mlr3.mlr-org.com/dev/reference/Resampling.md)  
  The resampling strategy to be used. The resampling is unchanged during
  the evaluation. The resampling is read-only.

- `iteration`:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  The current iteration. The iteration is read-only.

- `pdatas`:

  (List of
  [PredictionData](https://mlr3.mlr-org.com/dev/reference/PredictionData.md))  
  The prediction data. The data is available on stage `on_resample_end`.

- `data_extra`:

  (list())  
  Data saved in the
  [ResampleResult](https://mlr3.mlr-org.com/dev/reference/ResampleResult.md)
  or
  [BenchmarkResult](https://mlr3.mlr-org.com/dev/reference/BenchmarkResult.md).
  Use this field to save results. Must be a
  [`list()`](https://rdrr.io/r/base/list.html).

## Methods

### Public methods

- [`ContextResample$new()`](#method-ContextResample-new)

- [`ContextResample$clone()`](#method-ContextResample-clone)

Inherited methods

- [`mlr3misc::Context$format()`](https://mlr3misc.mlr-org.com/reference/Context.html#method-format)
- [`mlr3misc::Context$print()`](https://mlr3misc.mlr-org.com/reference/Context.html#method-print)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    ContextResample$new(task, learner, resampling, iteration)

#### Arguments

- `task`:

  ([Task](https://mlr3.mlr-org.com/dev/reference/Task.md))  
  The task to be evaluated.

- `learner`:

  ([Learner](https://mlr3.mlr-org.com/dev/reference/Learner.md))  
  The learner to be evaluated.

- `resampling`:

  ([Resampling](https://mlr3.mlr-org.com/dev/reference/Resampling.md))  
  The resampling strategy to be used.

- `iteration`:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  The current iteration.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ContextResample$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
