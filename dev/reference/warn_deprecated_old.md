# Give a Warning about a Deprecated Function, Argument, or Active Binding

Generates a warning when a deprecated function, argument, or active
binding is used or accessed. A warning will only be given once per
session, and all deprecation warnings can be suppressed by setting the
option `mlr3.warn_deprecated = FALSE`.

The warning is of the format "what is deprecated and will be removed in
the future."

Use the 'deprecated_binding_old()' helper function to create an active
binding that generates a warning when accessed.

## Usage

``` r
warn_deprecated_old(what)
```

## Arguments

- what:

  (character(1))  
  A description of the deprecated entity. This should be somewhat
  descriptive, e.g. `"Class$method()"` or
  `"Argument 'foo' of Class$method()"`.  
  The `what` is used to determine if the warning has already been given,
  so it should be unique for each deprecated entity.
