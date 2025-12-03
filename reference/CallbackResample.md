# Resample Callback

Specialized
[mlr3misc::Callback](https://mlr3misc.mlr-org.com/reference/Callback.html)
to customize the behavior of
[`resample()`](https://mlr3.mlr-org.com/reference/resample.md) and
[`benchmark()`](https://mlr3.mlr-org.com/reference/benchmark.md) in
mlr3. For example, callbacks can be used to extract information from
models on the worker or to store intermediate results to disk. The
[`callback_resample()`](https://mlr3.mlr-org.com/reference/callback_resample.md)
function is used to create instances of this class. Predefined callbacks
are stored in the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr_callbacks](https://mlr3misc.mlr-org.com/reference/mlr_callbacks.html)
and can be retrieved with
[`clbk()`](https://mlr3misc.mlr-org.com/reference/clbk.html). For more
information on callbacks, see the
[`callback_resample()`](https://mlr3.mlr-org.com/reference/callback_resample.md)
documentation.

## Super class

[`mlr3misc::Callback`](https://mlr3misc.mlr-org.com/reference/Callback.html)
-\> `CallbackResample`

## Public fields

- `on_resample_begin`:

  (`function()`)  
  Stage called at the beginning of the resampling iteration. Called in
  `workhorse()` (internal).

- `on_resample_before_train`:

  (`function()`)  
  Stage called before training the learner. Called in `workhorse()`
  (internal).

- `on_resample_before_predict`:

  (`function()`)  
  Stage called before predicting. Called in `workhorse()` (internal).

- `on_resample_end`:

  (`function()`)  
  Stage called at the end of the resample iteration. Called in
  `workhorse()` (internal).

## Methods

### Public methods

- [`CallbackResample$clone()`](#method-CallbackResample-clone)

Inherited methods

- [`mlr3misc::Callback$call()`](https://mlr3misc.mlr-org.com/reference/Callback.html#method-call)
- [`mlr3misc::Callback$format()`](https://mlr3misc.mlr-org.com/reference/Callback.html#method-format)
- [`mlr3misc::Callback$help()`](https://mlr3misc.mlr-org.com/reference/Callback.html#method-help)
- [`mlr3misc::Callback$initialize()`](https://mlr3misc.mlr-org.com/reference/Callback.html#method-initialize)
- [`mlr3misc::Callback$print()`](https://mlr3misc.mlr-org.com/reference/Callback.html#method-print)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    CallbackResample$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
