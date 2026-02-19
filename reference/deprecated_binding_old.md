# Create an Active Binding that Generates a Deprecation Warning

Creates an active binding that generates a warning when accessed, using
[`warn_deprecated_old()`](https://mlr3.mlr-org.com/reference/warn_deprecated_old.md).
The active binding will otherwise be read-only.

## Usage

``` r
deprecated_binding_old(what, value)
```

## Arguments

- what:

  (character(1))  
  A description of the deprecated binding. Should be of the form
  `"Class$field"`.

- value:

  (any)  
  The value of the active binding. This should be an expression that
  will be evaluated in the context of the active binding. It could, for
  example, refer to `self`.

## Examples

``` r
MyClass = R6::R6Class("MyClass", public = list(),
  active = list(
    foo = deprecated_binding_old("MyClass$foo", "bar")
  )
)
mco = MyClass$new()
mco$foo
#> Warning: MyClass$foo is deprecated and will be removed in the future.
#> [1] "bar"
```
