# TaskGenerator Class

Creates a [Task](https://mlr3.mlr-org.com/dev/reference/Task.md) of
arbitrary size. Predefined task generators are stored in the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr_task_generators](https://mlr3.mlr-org.com/dev/reference/mlr_task_generators.md),
e.g.
[`xor`](https://mlr3.mlr-org.com/dev/reference/mlr_task_generators_xor.md).

## See also

- [Dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
  of TaskGenerators:
  [mlr_task_generators](https://mlr3.mlr-org.com/dev/reference/mlr_task_generators.md)

- `as.data.table(mlr_task_generators)` for a table of available
  TaskGenerators in the running session (depending on the loaded
  packages).

- Extension packages for additional task types:

  - [mlr3proba](https://CRAN.R-project.org/package=mlr3proba) for
    probabilistic supervised regression and survival analysis.

  - [mlr3cluster](https://CRAN.R-project.org/package=mlr3cluster) for
    unsupervised clustering.

Other TaskGenerator:
[`mlr_task_generators`](https://mlr3.mlr-org.com/dev/reference/mlr_task_generators.md),
[`mlr_task_generators_2dnormals`](https://mlr3.mlr-org.com/dev/reference/mlr_task_generators_2dnormals.md),
[`mlr_task_generators_cassini`](https://mlr3.mlr-org.com/dev/reference/mlr_task_generators_cassini.md),
[`mlr_task_generators_circle`](https://mlr3.mlr-org.com/dev/reference/mlr_task_generators_circle.md),
[`mlr_task_generators_friedman1`](https://mlr3.mlr-org.com/dev/reference/mlr_task_generators_friedman1.md),
[`mlr_task_generators_moons`](https://mlr3.mlr-org.com/dev/reference/mlr_task_generators_moons.md),
[`mlr_task_generators_peak`](https://mlr3.mlr-org.com/dev/reference/mlr_task_generators_peak.md),
[`mlr_task_generators_simplex`](https://mlr3.mlr-org.com/dev/reference/mlr_task_generators_simplex.md),
[`mlr_task_generators_smiley`](https://mlr3.mlr-org.com/dev/reference/mlr_task_generators_smiley.md),
[`mlr_task_generators_spirals`](https://mlr3.mlr-org.com/dev/reference/mlr_task_generators_spirals.md),
[`mlr_task_generators_xor`](https://mlr3.mlr-org.com/dev/reference/mlr_task_generators_xor.md)

## Public fields

- `id`:

  (`character(1)`)  
  Identifier of the object. Used in tables, plot and text output.

- `label`:

  (`character(1)`)  
  Label for this object. Can be used in tables, plot and text output
  instead of the ID.

- `task_type`:

  (`character(1)`)  
  Task type, e.g. `"classif"` or `"regr"`.

  For a complete list of possible task types (depending on the loaded
  packages), see
  [`mlr_reflections$task_types$type`](https://mlr3.mlr-org.com/dev/reference/mlr_reflections.md).

- `param_set`:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html))  
  Set of hyperparameters.

- `packages`:

  (`character(1)`)  
  Set of required packages. These packages are loaded, but not attached.

- `man`:

  (`character(1)`)  
  String in the format `[pkg]::[topic]` pointing to a manual page for
  this object. Defaults to `NA`, but can be set by child classes.

## Methods

### Public methods

- [`TaskGenerator$new()`](#method-TaskGenerator-new)

- [`TaskGenerator$format()`](#method-TaskGenerator-format)

- [`TaskGenerator$print()`](#method-TaskGenerator-print)

- [`TaskGenerator$generate()`](#method-TaskGenerator-generate)

- [`TaskGenerator$clone()`](#method-TaskGenerator-clone)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    TaskGenerator$new(
      id,
      task_type,
      packages = character(),
      param_set = ps(),
      label = NA_character_,
      man = NA_character_
    )

#### Arguments

- `id`:

  (`character(1)`)  
  Identifier for the new instance.

- `task_type`:

  (`character(1)`)  
  Type of task, e.g. `"regr"` or `"classif"`. Must be an element of
  [mlr_reflections\$task_types\$type](https://mlr3.mlr-org.com/dev/reference/mlr_reflections.md).

- `packages`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Set of required packages. A warning is signaled by the constructor if
  at least one of the packages is not installed, but loaded (not
  attached) later on-demand via
  [`requireNamespace()`](https://rdrr.io/r/base/ns-load.html).

- `param_set`:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html))  
  Set of hyperparameters.

- `label`:

  (`character(1)`)  
  Label for the new instance.

- `man`:

  (`character(1)`)  
  String in the format `[pkg]::[topic]` pointing to a manual page for
  this object. The referenced help package can be opened via method
  `$help()`.

------------------------------------------------------------------------

### Method [`format()`](https://rdrr.io/r/base/format.html)

Helper for print outputs.

#### Usage

    TaskGenerator$format(...)

#### Arguments

- `...`:

  (ignored).

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Printer.

#### Usage

    TaskGenerator$print(...)

#### Arguments

- `...`:

  (ignored).

------------------------------------------------------------------------

### Method `generate()`

Creates a task of type `task_type` with `n` observations, possibly using
additional settings stored in `param_set`.

#### Usage

    TaskGenerator$generate(n)

#### Arguments

- `n`:

  (`integer(1)`)  
  Number of rows to generate.

#### Returns

[Task](https://mlr3.mlr-org.com/dev/reference/Task.md).

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    TaskGenerator$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
