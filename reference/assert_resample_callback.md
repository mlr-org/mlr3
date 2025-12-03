# Assertions for Callbacks

Assertions for
[CallbackResample](https://mlr3.mlr-org.com/reference/CallbackResample.md)
class.

## Usage

``` r
assert_resample_callback(callback, null_ok = FALSE)

assert_resample_callbacks(callbacks, null_ok = FALSE)
```

## Arguments

- callback:

  ([CallbackResample](https://mlr3.mlr-org.com/reference/CallbackResample.md)).

- null_ok:

  (`logical(1)`)  
  If `TRUE`, `NULL` is allowed.

- callbacks:

  (list of
  [CallbackResample](https://mlr3.mlr-org.com/reference/CallbackResample.md)).

## Value

[CallbackResample](https://mlr3.mlr-org.com/reference/CallbackResample.md)
\| List of
[CallbackResample](https://mlr3.mlr-org.com/reference/CallbackResample.md)s.
