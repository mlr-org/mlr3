# (Un)marshal a Learner

Marshaling is the process of processing the model of a trained
[`Learner`](https://mlr3.mlr-org.com/reference/Learner.md) so it an be
successfully serialized and deserialized. The naming is inspired by the
[marshal package](https://github.com/futureverse/marshal) and we plan to
fully migrate to this package once it is on CRAN. The current
implementation should therfore be considered as a temporary solution and
is likely to change in the future.

The central functions (and the only methods that are used by `mlr3`
internally) are:

- the S3 generic `marshal_model(model, inplace, ...)`. Which takes in a
  model and returns it in marshaled form. This means, that the resulting
  object can be serialized and de-serialzed without loss of information.
  If a model is serializable anyway, nothing has to be implemented and
  the generic will fall back to the default implementation of
  `marshal_model`, which is to return the object as-is. Otherwise, the
  marshaled object should be a list with named elements `marshaled` and
  `packages`, where the former contains the marshaled object, and the
  latter the package that contains the packages required to unmarshal.
  Most importantly, this list should contain the package that contains
  the `unmarshal_model` method. The returned object should have the
  classes of the original object with the suffix `"_marshaled"` appended
  and the root class should be set to `"marshaled"`.

- the S3 generic `unmarshal_model(model, inplace ...)`. Which takes in
  the marshaled model and returns it in unmarshaled form. The generic
  takes care that the packages specified during `"marshal"` are loaded,
  and errs if they are not availabe. Calling this function on a
  marshaled model should reconstruct the original model, i.e.
  `unmarshal_model(marshal_model(x))` should return `x`. The default
  implementation of this generic returns `x` as-is.

- the function `is_marshaled_model(model)`. This (helper) function
  returns `TRUE` if the model inherits from class `"marshaled"` and
  `FALSE` otherwise. Note that it is not guarateed that
  `is_marshaled_model(marshal_model(x))` returns `TRUE`. This is because
  the default `marshal_model(x)` returns `x` as-is.

For both `marshal_model` and `unmarshal_model`, the `inplace` argument
determines whether in-place marshaling should be performed. This is
especially relevant in the context of references semantics. If `inplace`
is `FALSE`, the original input should not be modified, otherwise this is
allowed. Note that the input and output can still share references, even
when `inplace` is `FALSE`.

## Usage

``` r
learner_unmarshal(.learner, ...)

learner_marshal(.learner, ...)

learner_marshaled(.learner)

marshal_model(model, inplace = FALSE, ...)

unmarshal_model(model, inplace = FALSE, ...)

is_marshaled_model(model)
```

## Arguments

- .learner:

  [`Learner`](https://mlr3.mlr-org.com/reference/Learner.md)  
  The learner.

- ...:

  (any)  
  Additional parameters, currently unused.

- model:

  (any)  
  Model to marshal.

- inplace:

  (`logical(1)`)  
  Whether to marshal in-place.

## Implementing Marshaling

In order to implement marshaling for a Learner, you need to overload the
`marshal_model` and `unmarshal_model` methods for the class of the
learner's model and tag the learner with the `"marshal"` property. To
make marshaling accessible in an R6-manner, you should also add the
public methods `$marshal()`, `$unmarshal()` and the active binding
`$marshaled`. To make this as convenient as possible, the functions
`learner_marshal(.learner, ...)`, `learner_unmarshal(.learner, ...)` and
`learner_marshaled(.learner)` are provided and can be called from the
public methods.

You can verify whether you have correctly implemented marshaling by
using the internal test helper
`expect_marshalable_learner(learner, task)`. This is also run by
`expect_learner()` if a task is provided.

For a concrete example on how to implement marshaling, see
[`LearnerClassifDebug`](https://mlr3.mlr-org.com/reference/mlr_learners_classif.debug.md).
