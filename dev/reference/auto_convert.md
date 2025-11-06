# Column Auto-Converter

Set of rules to automatically convert column types. This is used during
`rbind`-ing of [Task](https://mlr3.mlr-org.com/dev/reference/Task.md)s,
but also in some pipe operators in
[mlr3pipelines](https://CRAN.R-project.org/package=mlr3pipelines).

All rules are stored as functions in
[mlr_reflections\$auto_converters](https://mlr3.mlr-org.com/dev/reference/mlr_reflections.md).

## Usage

``` r
auto_convert(value, id, type, levels)
```

## Arguments

- value:

  (any)  
  New values to convert in order to match `type`.

- id:

  (`character(1)`)  
  Name of the column, used in error messages.

- type:

  (`character(1)`)  
  Type to convert `values` to.

- levels:

  ([`character()`](https://rdrr.io/r/base/character.html) \| `NULL`)  
  Levels to use for conversion to `factor` or `ordered`.

## Value

Vector `value` converted to type `type`.
